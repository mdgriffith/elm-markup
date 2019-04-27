module Mark.Edit exposing
    ( bool, int, float, string, multiline, oneOf, manyOf
    , text, Replacement, Inline
    , Tree(..), Icon(..), tree
    , update, Error, Edit, replace, delete, insertAt
    , restyle, addStyles, removeStyles
    )

{-|


# Editable Blocks

@docs bool, int, float, string, multiline, oneOf, manyOf

@docs text, Replacement, Inline

@docs Tree, Icon, tree


# Making Edits

@docs update, Error, Edit, replace, delete, insertAt

@docs restyle, addStyles, removeStyles

-}

import Mark.Internal.Description as Desc exposing (..)
import Mark.Internal.Error as Error
import Mark.Internal.Format as Format
import Mark.Internal.Id as Id exposing (..)
import Mark.Internal.Index as Index
import Mark.Internal.Outcome as Outcome
import Mark.Internal.Parser as Parse
import Mark.New
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)



-- {-| -}
-- type alias Error =
--     { message : List Format.Text
--     , region : { start : Position, end : Position }
--     , title : String
--     }


{-| -}
type alias Error =
    Error.Rendered


{-| -}
type alias Id =
    Id.Id


{-| -}
type alias Position =
    { offset : Int
    , line : Int
    , column : Int
    }


{-| -}
type alias Range =
    { start : Position
    , end : Position
    }


{-| -}
type Proved
    = Proved Id (List (Found Description))


{-| -}
type Edit
    = Replace Id Mark.New.Block
      -- Create an element in a ManyOf
      -- Indexes overflow, so if it's too large, it just puts it at the end.
      -- Indexes that are below 0 and clamped to 0
    | InsertAt Id Int Mark.New.Block
    | Delete Id Int
      -- Text Editing
    | StyleText Id Selection Restyle
    | Annotate Id Selection Annotation
    | ReplaceSelection Id Selection (List Mark.New.Text)


type Annotation
    = Annotation String (List Mark.New.Attribute)
    | Verbatim String (List Mark.New.Attribute)


{-| -}
replace : Id -> Mark.New.Block -> Edit
replace =
    Replace


{-| -}
delete : Id -> Int -> Edit
delete =
    Delete


{-| -}
insertAt : Id -> Int -> Mark.New.Block -> Edit
insertAt =
    InsertAt


{-| -}
annotate : Id -> Selection -> String -> List Mark.New.Attribute -> Edit
annotate id selection name attrs =
    Annotate id selection (Annotation name attrs)


{-| -}
makeVerbatim : Id -> Selection -> String -> Edit
makeVerbatim id selection name =
    Annotate id selection (Verbatim name [])


{-| -}
makeVerbatimWith : Id -> Selection -> String -> List Mark.New.Attribute -> Edit
makeVerbatimWith id selection name attrs =
    Annotate id selection (Verbatim name attrs)


{-| -}
restyle : Id -> Selection -> Styles -> Edit
restyle id selection styles =
    StyleText id selection (Restyle styles)


{-| -}
removeStyles : Id -> Selection -> Styles -> Edit
removeStyles id selection styles =
    StyleText id selection (RemoveStyle styles)


{-| -}
addStyles : Id -> Selection -> Styles -> Edit
addStyles id selection styles =
    StyleText id selection (AddStyle styles)


prepareResults doc original ( edited, newDescription ) =
    case edited of
        NoEditMade ->
            Err [ Error.idNotFound ]

        YesEditMade _ ->
            let
                newParsed =
                    Parsed { original | found = newDescription }
            in
            case Desc.render doc newParsed of
                Outcome.Success _ ->
                    Ok newParsed

                Outcome.Almost details ->
                    Err details.errors

                Outcome.Failure errs ->
                    Err errs


editAtId id fn indentation pos desc =
    if Desc.getId desc == id then
        fn indentation pos desc

    else
        Nothing


replaceOption id new desc =
    case desc of
        OneOf one ->
            -- When we're replacing the OneOf, we actually want to replace it's contents.
            let
                newSize =
                    getSize new

                existingSize =
                    sizeFromRange (getFoundRange one.child)
            in
            case one.child of
                Found range val ->
                    Just
                        ( minusSize newSize existingSize
                            |> sizeToPush
                        , OneOf { one | child = Found range new }
                        )

                Unexpected unexpected ->
                    Just
                        ( minusSize newSize existingSize
                            |> sizeToPush
                        , OneOf { one | child = Found unexpected.range new }
                        )

        _ ->
            let
                newSize =
                    getSize new

                existingSize =
                    getSize desc
            in
            Just
                ( minusSize newSize existingSize
                    |> sizeToPush
                , new
                )


sizeToPush size =
    if size.offset == 0 && size.line == 0 then
        Nothing

    else
        Just size


makeDeleteBlock id index indentation pos desc =
    case desc of
        ManyOf many ->
            let
                cleaned =
                    removeByIndex index many.children
            in
            Just
                ( cleaned.push
                , ManyOf
                    { many
                        | children = List.reverse cleaned.items
                    }
                )

        _ ->
            Nothing


{-| -}
update : Document data -> Edit -> Parsed -> Result (List Error) Parsed
update doc edit (Parsed original) =
    let
        editFn =
            case edit of
                Replace id new ->
                    editAtId id <|
                        \i pos desc ->
                            let
                                created =
                                    create
                                        { indent = i
                                        , base = pos
                                        , expectation = new
                                        , seed = original.currentSeed
                                        }
                            in
                            replaceOption id created.desc desc

                InsertAt id index expectation ->
                    editAtId id <|
                        \indentation pos desc ->
                            case desc of
                                ManyOf many ->
                                    let
                                        ( pushed, newChildren ) =
                                            makeInsertAt
                                                original.currentSeed
                                                index
                                                indentation
                                                many
                                                expectation
                                    in
                                    Just
                                        ( pushed
                                        , ManyOf
                                            { many
                                                | children =
                                                    newChildren
                                            }
                                        )

                                _ ->
                                    -- inserts only work for
                                    -- `ManyOf`, `Tree`, and `Text`
                                    Nothing

                Delete id index ->
                    editAtId id
                        (makeDeleteBlock id index)

                StyleText id selection restyleAction ->
                    editAtId id
                        (\indent pos desc ->
                            case desc of
                                DescribeText details ->
                                    let
                                        newTexts =
                                            details.text
                                                |> List.foldl
                                                    (doTextEdit selection
                                                        (List.map (applyStyles restyleAction))
                                                    )
                                                    emptySelectionEdit
                                                |> .elements
                                                |> List.foldl mergeStyles []
                                    in
                                    Just
                                        ( Just (pushNewTexts details.text newTexts)
                                        , DescribeText
                                            { details | text = newTexts }
                                        )

                                _ ->
                                    Nothing
                        )

                Annotate id selection wrapper ->
                    editAtId id
                        (\indent pos desc ->
                            case desc of
                                DescribeText details ->
                                    let
                                        newTexts =
                                            details.text
                                                |> List.foldl
                                                    (doTextEdit selection
                                                        (\els ->
                                                            let
                                                                textStart =
                                                                    getTextStart els
                                                                        |> Maybe.withDefault pos

                                                                wrapped =
                                                                    case wrapper of
                                                                        Annotation name attrs ->
                                                                            ExpectAnnotation name attrs (List.concatMap onlyText els)

                                                                        Verbatim name attrs ->
                                                                            ExpectVerbatim name
                                                                                attrs
                                                                                (List.concatMap onlyText els
                                                                                    |> List.map textString
                                                                                    |> String.join ""
                                                                                )

                                                                ( end, newText ) =
                                                                    createInline
                                                                        textStart
                                                                        [ wrapped ]
                                                            in
                                                            newText
                                                        )
                                                    )
                                                    emptySelectionEdit
                                                |> .elements
                                                |> List.foldl mergeStyles []
                                    in
                                    Just
                                        ( Just (pushNewTexts details.text newTexts)
                                        , DescribeText
                                            { details | text = newTexts }
                                        )

                                _ ->
                                    Nothing
                        )

                ReplaceSelection id selection newTextEls ->
                    editAtId id
                        (\indent pos desc ->
                            case desc of
                                DescribeText details ->
                                    let
                                        makeNewText selectedEls =
                                            newTextEls
                                                |> createInline (Maybe.withDefault pos (getTextStart selectedEls))
                                                |> Tuple.second

                                        newTexts =
                                            details.text
                                                |> List.foldl
                                                    (doTextEdit selection
                                                        makeNewText
                                                    )
                                                    emptySelectionEdit
                                                |> .elements
                                                |> List.foldl mergeStyles []
                                    in
                                    Just
                                        ( Just (pushNewTexts details.text newTexts)
                                        , DescribeText
                                            { details | text = newTexts }
                                        )

                                _ ->
                                    Nothing
                        )
    in
    original.found
        |> makeFoundEdit
            { makeEdit = editFn
            , indentation = 0
            }
        |> prepareResults doc original


pushNewTexts existing new =
    minusSize
        (textSize new)
        (textSize existing)


getTextStart els =
    case els of
        [] ->
            Nothing

        starter :: _ ->
            Just (.start (textDescriptionRange starter))


textString (Text _ str) =
    str


{-| -}
prove : List (Found Description) -> List ( Id, Expectation ) -> Maybe Proved
prove found choices =
    let
        combineChoices ( id, exp ) ( lastId, foundExpectations, matchingIds ) =
            case lastId of
                Nothing ->
                    ( Just id, exp :: foundExpectations, matchingIds )

                Just prev ->
                    if prev == id then
                        ( lastId, exp :: foundExpectations, matchingIds )

                    else
                        ( lastId, foundExpectations, False )

        ( maybeId, expectations, allMatching ) =
            List.foldl combineChoices ( Nothing, [], True ) choices
    in
    if allMatching then
        case maybeId of
            Just id ->
                List.foldl (validate expectations) (Just []) found
                    |> Maybe.map (Proved id << List.reverse)

            Nothing ->
                Nothing

    else
        Nothing


{-| -}
validate : List Expectation -> Found Description -> Maybe (List (Found Description)) -> Maybe (List (Found Description))
validate expectations found validated =
    case validated of
        Nothing ->
            Nothing

        Just vals ->
            case found of
                Found _ description ->
                    if List.any (match description) expectations then
                        Just (found :: vals)

                    else
                        Nothing

                Unexpected unexpected ->
                    Nothing


match description exp =
    case description of
        DescribeBlock details ->
            case exp of
                ExpectBlock expectedName expectedChild ->
                    if expectedName == details.name then
                        matchExpected details.expected expectedChild

                    else
                        False

                _ ->
                    False

        Record details ->
            matchExpected details.expected exp

        OneOf one ->
            matchExpected (ExpectOneOf one.choices) exp

        ManyOf many ->
            matchExpected (ExpectManyOf many.choices) exp

        StartsWith details ->
            case exp of
                ExpectStartsWith startExp endExp ->
                    match details.first.found startExp
                        && match details.second.found endExp

                _ ->
                    False

        DescribeTree myTree ->
            matchExpected myTree.expected exp

        DescribeBoolean foundBoolean ->
            case exp of
                ExpectBoolean _ ->
                    True

                _ ->
                    False

        DescribeInteger _ ->
            case exp of
                ExpectInteger _ ->
                    True

                _ ->
                    False

        DescribeFloat _ ->
            case exp of
                ExpectFloat _ ->
                    True

                _ ->
                    False

        DescribeText _ ->
            case exp of
                ExpectTextBlock _ ->
                    True

                _ ->
                    False

        DescribeString _ _ _ ->
            case exp of
                ExpectString _ ->
                    True

                _ ->
                    False

        DescribeMultiline _ _ _ ->
            case exp of
                ExpectMultiline _ ->
                    True

                _ ->
                    False

        DescribeNothing _ ->
            False


{-| Is the first expectation a subset of the second?
-}
matchExpected : Expectation -> Expectation -> Bool
matchExpected subExp expected =
    case ( subExp, expected ) of
        ( ExpectBlock oneName oneExp, ExpectBlock twoName twoExp ) ->
            oneName == twoName && matchExpected oneExp twoExp

        ( ExpectRecord one oneFields, ExpectRecord two twoFields ) ->
            one == two && List.all (matchFields twoFields) oneFields

        ( ExpectOneOf oneOptions, ExpectOneOf twoOptions ) ->
            List.all (matchExpectedOptions twoOptions) oneOptions

        ( ExpectManyOf oneOptions, ExpectManyOf twoOptions ) ->
            List.all (matchExpectedOptions twoOptions) oneOptions

        ( ExpectStartsWith oneStart oneRemain, ExpectStartsWith twoStart twoRemain ) ->
            matchExpected oneStart twoStart
                && matchExpected oneRemain twoRemain

        ( ExpectBoolean _, ExpectBoolean _ ) ->
            True

        ( ExpectInteger _, ExpectInteger _ ) ->
            True

        ( ExpectFloat _, ExpectFloat _ ) ->
            True

        ( ExpectTextBlock oneInline, ExpectTextBlock twoInline ) ->
            True

        ( ExpectString _, ExpectString _ ) ->
            True

        ( ExpectMultiline _, ExpectMultiline _ ) ->
            True

        ( ExpectTree oneContent _, ExpectTree twoContent _ ) ->
            True

        _ ->
            False


matchExpectedOptions : List Expectation -> Expectation -> Bool
matchExpectedOptions opts target =
    List.any (matchExpected target) opts


matchFields : List ( String, Expectation ) -> ( String, Expectation ) -> Bool
matchFields valid ( targetFieldName, targetFieldExpectation ) =
    let
        innerMatch ( validFieldName, validExpectation ) =
            validFieldName
                == targetFieldName
                && matchExpected validExpectation targetFieldExpectation
    in
    List.any innerMatch valid


type alias EditCursor =
    -- An edit takes the indentation level
    -- , the last reference position
    -- and the current description
    { makeEdit : Int -> Position -> Description -> Maybe ( Push, Description )
    , indentation : Int
    }


type alias Push =
    Maybe Size


type alias Size =
    { offset : Int
    , line : Int
    }


type EditMade
    = YesEditMade Push
    | NoEditMade


{-| -}
makeFoundEdit : EditCursor -> Found Description -> ( EditMade, Found Description )
makeFoundEdit cursor foundDesc =
    case foundDesc of
        Found range desc ->
            case cursor.makeEdit cursor.indentation range.start desc of
                Nothing ->
                    makeEdit cursor desc
                        |> Tuple.mapSecond (Found range)

                Just ( maybePush, newDesc ) ->
                    ( YesEditMade maybePush, Found range newDesc )

        Unexpected unexpected ->
            ( NoEditMade, foundDesc )


increaseIndent x =
    { x | indentation = x.indentation + 1 }


{-| -}
makeEdit : EditCursor -> Description -> ( EditMade, Description )
makeEdit cursor desc =
    case desc of
        DescribeBlock details ->
            case cursor.makeEdit cursor.indentation (foundStart details.found) desc of
                Just ( maybePush, newDesc ) ->
                    -- replace current description
                    ( YesEditMade maybePush, newDesc )

                Nothing ->
                    -- dive further
                    makeFoundEdit (increaseIndent cursor) details.found
                        |> (\( editMade, newFound ) ->
                                case editMade of
                                    NoEditMade ->
                                        ( NoEditMade, desc )

                                    YesEditMade maybePush ->
                                        ( YesEditMade maybePush
                                        , DescribeBlock
                                            { details
                                                | found = newFound
                                            }
                                        )
                           )

        Record details ->
            case cursor.makeEdit (cursor.indentation + 1) (foundStart details.found) desc of
                Just ( maybePush, newDesc ) ->
                    -- replace current description
                    ( YesEditMade maybePush, newDesc )

                Nothing ->
                    case details.found of
                        Found rng fields ->
                            let
                                ( fieldsEdited, updatedFields ) =
                                    List.foldl
                                        (\(( fieldName, foundField ) as field) ( editMade, pastFields ) ->
                                            case editMade of
                                                YesEditMade maybePush ->
                                                    ( editMade
                                                    , ( fieldName
                                                      , case maybePush of
                                                            Nothing ->
                                                                foundField

                                                            Just to ->
                                                                pushFound to foundField
                                                      )
                                                        :: pastFields
                                                    )

                                                NoEditMade ->
                                                    case makeFoundEdit (increaseIndent (increaseIndent cursor)) foundField of
                                                        ( NoEditMade, _ ) ->
                                                            ( NoEditMade, field :: pastFields )

                                                        ( YesEditMade maybePush, newField ) ->
                                                            ( YesEditMade maybePush
                                                            , ( fieldName, newField ) :: pastFields
                                                            )
                                        )
                                        ( NoEditMade, [] )
                                        fields
                            in
                            case fieldsEdited of
                                NoEditMade ->
                                    ( NoEditMade, desc )

                                YesEditMade maybePush ->
                                    ( YesEditMade maybePush
                                    , Record
                                        { details
                                            | found =
                                                Found (expandRange maybePush rng)
                                                    (List.reverse updatedFields)
                                        }
                                    )

                        Unexpected unexpected ->
                            ( NoEditMade, desc )

        OneOf details ->
            case cursor.makeEdit cursor.indentation (foundStart details.child) desc of
                Just ( maybePush, newDesc ) ->
                    -- replace current description
                    ( YesEditMade maybePush
                    , newDesc
                    )

                Nothing ->
                    -- dive further
                    makeFoundEdit (increaseIndent cursor) details.child
                        |> (\( editMade, newFound ) ->
                                case editMade of
                                    NoEditMade ->
                                        ( NoEditMade, desc )

                                    YesEditMade maybePush ->
                                        ( YesEditMade maybePush
                                        , OneOf
                                            { details
                                                | child = expandFound maybePush newFound
                                            }
                                        )
                           )

        ManyOf many ->
            case cursor.makeEdit cursor.indentation many.range.start desc of
                Just ( maybePush, newDesc ) ->
                    -- replace current description
                    ( YesEditMade maybePush, newDesc )

                Nothing ->
                    -- dive further
                    let
                        ( childrenEdited, updatedChildren ) =
                            editMany makeFoundEdit push cursor many.children
                    in
                    case childrenEdited of
                        NoEditMade ->
                            ( NoEditMade, desc )

                        YesEditMade maybePush ->
                            ( childrenEdited
                            , ManyOf
                                { many
                                    | children =
                                        updatedChildren
                                    , range =
                                        case maybePush of
                                            Nothing ->
                                                many.range

                                            Just p ->
                                                pushRange p many.range
                                }
                            )

        StartsWith details ->
            let
                ( firstEdited, firstUpdated ) =
                    makeEdit cursor details.first.found
            in
            case firstEdited of
                NoEditMade ->
                    let
                        ( secondEdited, secondUpdated ) =
                            makeEdit cursor details.second.found
                    in
                    case secondEdited of
                        NoEditMade ->
                            ( NoEditMade, desc )

                        YesEditMade maybePush ->
                            ( YesEditMade maybePush
                            , StartsWith
                                { range = details.range
                                , id = details.id
                                , first = details.first
                                , second =
                                    details.second
                                        |> (\snd ->
                                                { snd | found = secondUpdated }
                                           )
                                }
                            )

                YesEditMade maybePush ->
                    ( YesEditMade maybePush
                    , StartsWith
                        { range = details.range
                        , id = details.id
                        , second = details.second
                        , first =
                            details.first
                                |> (\fst ->
                                        { fst | found = firstUpdated }
                                   )
                        }
                    )

        DescribeTree details ->
            let
                ( treeEdited, newChildren ) =
                    editListNested cursor details.children
            in
            case treeEdited of
                NoEditMade ->
                    ( treeEdited, desc )

                YesEditMade maybePush ->
                    ( treeEdited
                    , DescribeTree
                        { details
                            | children = newChildren
                            , range =
                                case maybePush of
                                    Nothing ->
                                        details.range

                                    Just p ->
                                        pushRange p details.range
                        }
                    )

        -- Primitives
        DescribeBoolean details ->
            replacePrimitive cursor (foundStart details.found) desc

        DescribeInteger found ->
            replacePrimitive cursor (foundStart found.found) desc

        DescribeFloat found ->
            replacePrimitive cursor (foundStart found.found) desc

        DescribeText txt ->
            replacePrimitive cursor (.start txt.range) desc

        DescribeString id range str ->
            replacePrimitive cursor range.start desc

        DescribeMultiline id range str ->
            replacePrimitive cursor range.start desc

        DescribeNothing _ ->
            ( NoEditMade, desc )


editNested cursor (Nested nestedDetails) =
    let
        ( contentEdited, newContent ) =
            editMany makeEdit
                (\maybePush desc ->
                    case maybePush of
                        Nothing ->
                            desc

                        Just p ->
                            pushDescription p desc
                )
                cursor
                nestedDetails.content
    in
    editListNested cursor nestedDetails.children


editListNested cursor lsNested =
    let
        indentedCursor =
            increaseIndent cursor
    in
    lsNested
        |> List.foldl
            (\foundChild ( editMade, pastChildren ) ->
                case editMade of
                    YesEditMade maybePush ->
                        ( editMade
                        , pushNested maybePush foundChild :: pastChildren
                        )

                    NoEditMade ->
                        case editNested indentedCursor foundChild of
                            ( NoEditMade, _ ) ->
                                ( NoEditMade, foundChild :: pastChildren )

                            ( YesEditMade maybePush, newChild ) ->
                                ( YesEditMade maybePush
                                , newChild :: pastChildren
                                )
            )
            ( NoEditMade, [] )
        |> Tuple.mapSecond List.reverse


editMany fn pusher cursor manyItems =
    manyItems
        |> List.foldl
            (\node ( editMade, pastChildren ) ->
                case editMade of
                    YesEditMade maybePush ->
                        ( editMade
                        , pusher maybePush node :: pastChildren
                        )

                    NoEditMade ->
                        case fn (increaseIndent (increaseIndent cursor)) node of
                            ( NoEditMade, _ ) ->
                                ( NoEditMade, node :: pastChildren )

                            ( YesEditMade maybePush, newChild ) ->
                                ( YesEditMade maybePush
                                , newChild :: pastChildren
                                )
            )
            ( NoEditMade, [] )
        |> Tuple.mapSecond List.reverse


foundStart found =
    case found of
        Found rng _ ->
            rng.start

        Unexpected unexpected ->
            unexpected.range.start


replacePrimitive cursor startingPos desc =
    case cursor.makeEdit cursor.indentation startingPos desc of
        Just ( maybePush, newDesc ) ->
            -- replace current description
            ( YesEditMade maybePush, newDesc )

        Nothing ->
            ( NoEditMade, desc )


within rangeOne rangeTwo =
    withinOffsetRange { start = rangeOne.start.offset, end = rangeOne.end.offset } rangeTwo


withinOffsetRange offset range =
    range.start.offset <= offset.start && range.end.offset >= offset.end



{- All the above ids are opaque, so we know they can't be spoofed.

       The editing commands all require one of these opaque values to be constructed.

       An id captures:

           1. The coordinates of a specific point
           2. What operations can be performed at that point
           3. A valid payload

       For Replace

           -> Can we accept an Expectation ++ ID Combo?

           -> Means we can't let the dev create their own Description


   Editing Messages are generated by an Editor that we create.

   Or by an editor fragment that we create.

   The expectation would be inflated with built in defaults


-}
{- EDITING

   A general sketch of Edits.

   If a human is sending updates, then likely these will be single character updates or deletions.



   Simple case, the edit is completely within a leaf node

       -> replace leaf node

   More advanced

       -> get smallest containing block
       -> generate source for that block
       -> replace target range with new string
       -> generate parser for that block
            -> Adjusting correctly for offsets
       -> reparse
       -> replace on AST
            -> Adjust node indexes

   Issues:
       -> Seems like a lot of work.

   Individual Edits

       -> addChar
           -> add space
           -> add newline
       -> deleteChar


-}


{-| Given an expectation and a list of choices, verify that the expectation is a valid choice.
-}
make : Expectation -> List ( id, Expectation ) -> Maybe ( id, Expectation )
make expected options =
    List.filterMap
        (\( id, exp ) ->
            if matchExpected expected exp then
                Just ( id, expected )

            else
                Nothing
        )
        options
        |> List.head


boolToString : Bool -> String
boolToString b =
    if b then
        "True"

    else
        "False"


moveColumn : Int -> Position -> Position
moveColumn num pos =
    { offset = pos.offset + num
    , column = pos.column + num
    , line = pos.line
    }


moveNewline : Position -> Position
moveNewline pos =
    { offset = pos.offset + 1
    , column = 1
    , line = pos.line + 1
    }


removeByIndex index list =
    {- We want to remove an item and push subsequent items based on

    -}
    List.foldl
        (\item cursor ->
            if cursor.index == index then
                let
                    range =
                        getFoundRange item

                    pushSize =
                        range
                            |> sizeFromRange
                            -- we want to remove this, so invert the size.
                            |> invertSize
                            |> Just
                in
                { index = cursor.index + 1
                , items = cursor.items

                -- we also want to eliminate all space till the next item,
                -- so we record the end of this item, and wait
                -- till we see the start of the next to add it to the push
                , recordPushGapTillNextItem =
                    Just range.end
                , push =
                    pushSize
                }

            else
                let
                    pushAmount =
                        case cursor.recordPushGapTillNextItem of
                            Nothing ->
                                cursor.push

                            Just previousEnd ->
                                let
                                    range =
                                        getFoundRange item
                                in
                                { start = previousEnd, end = range.start }
                                    |> sizeFromRange
                                    |> invertSize
                                    |> (\additionalSize ->
                                            Maybe.map (addSizes additionalSize) cursor.push
                                       )
                in
                { index = cursor.index + 1
                , items = push pushAmount item :: cursor.items
                , recordPushGapTillNextItem = Nothing
                , push = pushAmount
                }
        )
        { index = 0
        , items = []
        , recordPushGapTillNextItem = Nothing
        , push = Nothing
        }
        list


addSizes one two =
    { line = one.line + two.line
    , offset = one.offset + two.offset
    }


invertSize size =
    { line = -1 * size.line
    , offset = -1 * size.offset
    }


{-| -}
startDocRange : Range
startDocRange =
    { start =
        startingPoint
    , end =
        startingPoint
    }


expandFound : Push -> Found a -> Found a
expandFound maybePush found =
    case found of
        Found rng a ->
            Found
                (expandRange maybePush rng)
                a

        Unexpected unexp ->
            Unexpected
                { unexp | range = expandRange maybePush unexp.range }


{-| -}
expandRange : Push -> Range -> Range
expandRange maybePush range =
    case maybePush of
        Nothing ->
            range

        Just to ->
            { range
                | end =
                    { offset = range.end.offset + to.offset
                    , line = range.end.line + to.line
                    , column = range.end.column
                    }
            }


pushNested : Push -> Nested Description -> Nested Description
pushNested maybePush ((Nested nestedDetails) as nestedDesc) =
    case maybePush of
        Nothing ->
            nestedDesc

        Just to ->
            -- TODO: need to push and account for new index
            nestedDesc


push : Push -> Found Description -> Found Description
push maybePush found =
    case maybePush of
        Nothing ->
            found

        Just to ->
            pushFound to found


pushFound : Size -> Found Description -> Found Description
pushFound to found =
    case found of
        Found range item ->
            Found (pushRange to range) (pushDescription to item)

        Unexpected unexpected ->
            Unexpected { unexpected | range = pushRange to unexpected.range }


pushFoundRange to found =
    case found of
        Found range item ->
            Found (pushRange to range) item

        Unexpected unexpected ->
            Unexpected { unexpected | range = pushRange to unexpected.range }


pushDescription to desc =
    case desc of
        DescribeNothing _ ->
            desc

        DescribeBlock details ->
            DescribeBlock
                { id = details.id
                , name = details.name
                , found = pushFound to details.found
                , expected = details.expected
                }

        Record details ->
            Record
                { id = details.id
                , name = details.name
                , found =
                    details.found
                        |> pushFoundRange to
                        |> mapFound
                            (List.map
                                (\( field, foundField ) ->
                                    ( field, pushFound to foundField )
                                )
                            )
                , expected = details.expected
                }

        OneOf one ->
            OneOf
                { id = one.id
                , choices = one.choices
                , child = pushFound to one.child
                }

        ManyOf many ->
            ManyOf
                { id = many.id
                , range = pushRange to many.range
                , choices = many.choices
                , children = List.map (pushFound to) many.children
                }

        StartsWith details ->
            StartsWith
                { range = pushRange to details.range
                , id = details.id
                , first =
                    { found = pushDescription to details.first.found
                    , expected = details.first.expected
                    }
                , second =
                    { found = pushDescription to details.second.found
                    , expected = details.second.expected
                    }
                }

        DescribeBoolean details ->
            DescribeBoolean
                { details
                    | found = pushFoundRange to details.found
                }

        DescribeInteger details ->
            DescribeInteger
                { details
                    | found = pushFoundRange to details.found
                }

        DescribeFloat details ->
            DescribeFloat
                { details
                    | found = pushFoundRange to details.found
                }

        DescribeText txt ->
            DescribeText
                { txt
                    | range = pushRange to txt.range
                }

        DescribeString id range str ->
            DescribeString id (pushRange to range) str

        DescribeMultiline id range str ->
            DescribeMultiline
                id
                (pushRange to range)
                str

        DescribeTree myTree ->
            DescribeTree
                { myTree
                    | range = pushRange to myTree.range
                    , children =
                        List.map
                            (Desc.mapNested (pushDescription to))
                            myTree.children
                }


pushRange to range =
    { start = pushPosition to range.start
    , end = pushPosition to range.end
    }


pushPosition to pos =
    { offset = pos.offset + to.offset
    , line = pos.line + to.line
    , column = pos.column
    }


addPositions to pos =
    { offset = pos.offset + to.offset
    , line = pos.line + to.line
    , column = pos.column + to.column
    }


addNewline pos =
    { offset = pos.offset + 1
    , line = pos.line + 1
    }


pushFromRange { start, end } =
    { offset = end.offset - start.offset
    , line = end.line - start.line
    , column = end.column - start.column
    }


minusPosition end start =
    { offset = end.offset - start.offset
    , line = end.line - start.line
    , column = end.column - start.column
    }


sizeToRange start delta =
    { start = start
    , end =
        addPositions start delta
    }


makeInsertAt :
    Id.Seed
    -> Int
    -> Int
    ->
        { children : List (Found Description)
        , choices : List Expectation
        , id : Id
        , range : Range
        }
    -> Expectation
    -> ( Push, List (Found Description) )
makeInsertAt seed index indentation many expectation =
    many.children
        |> List.foldl (insertHelper seed index indentation expectation)
            { index = 0
            , position = many.range.start
            , inserted = False
            , list = []
            , push = Nothing
            }
        |> (\found ->
                if found.inserted then
                    ( Maybe.map
                        (\p ->
                            { offset = p.offset
                            , line = p.line
                            }
                        )
                        found.push
                    , List.reverse found.list
                    )

                else
                    let
                        newStart =
                            { offset = found.position.offset + 2
                            , line = found.position.line + 2
                            , column = (indentation * 4) + 1
                            }

                        created =
                            create
                                { indent = indentation
                                , base = newStart
                                , expectation = expectation
                                , seed = seed
                                }
                    in
                    ( Just
                        (sizeFromRange
                            { start = found.position
                            , end = created.pos
                            }
                        )
                    , List.reverse
                        (Found
                            { start = newStart
                            , end = created.pos
                            }
                            created.desc
                            :: found.list
                        )
                    )
           )


insertHelper seed index indentation expectation item found =
    if found.index == index then
        let
            newStart =
                if index == 0 then
                    { offset = found.position.offset
                    , line = found.position.line
                    , column = (indentation * 4) + 1
                    }

                else
                    { offset = found.position.offset + 2
                    , line = found.position.line + 2
                    , column = (indentation * 4) + 1
                    }

            created =
                create
                    { indent = indentation
                    , base = newStart
                    , expectation = expectation
                    , seed = seed
                    }

            newFound =
                Found
                    { start = newStart
                    , end = created.pos
                    }
                    created.desc

            newDescSize =
                minusPosition created.pos newStart
                    -- A block doesn't account for it's own newline,
                    -- so we have to add one here.
                    |> addNewline

            pushAmount =
                Just (addNewline newDescSize)

            pushed =
                push pushAmount item
        in
        { index = found.index + 1
        , inserted = True
        , list =
            pushed
                :: newFound
                :: found.list
        , push = pushAmount
        , position = .end (getFoundRange pushed)
        }

    else
        let
            pushed =
                push found.push item
        in
        { index = found.index + 1
        , inserted = found.inserted
        , list = pushed :: found.list
        , push = found.push
        , position = .end (getFoundRange pushed)
        }


getFoundRange found =
    case found of
        Found rng _ ->
            rng

        Unexpected unexp ->
            unexp.range


updateFoundBool id newBool desc =
    case desc of
        DescribeBoolean details ->
            if details.id == id then
                case details.found of
                    Found boolRng fl ->
                        Just
                            (DescribeBoolean
                                { id = details.id
                                , found =
                                    Found boolRng
                                        newBool
                                }
                            )

                    Unexpected unexpected ->
                        Just
                            (DescribeBoolean
                                { id = details.id
                                , found =
                                    Found unexpected.range
                                        newBool
                                }
                            )

            else
                Nothing

        _ ->
            Nothing


updateFoundFloat id newFloat desc =
    case desc of
        DescribeFloat details ->
            if details.id == id then
                case details.found of
                    Found floatRng fl ->
                        Just
                            (DescribeFloat
                                { id = details.id
                                , found =
                                    Found floatRng
                                        ( String.fromFloat newFloat, newFloat )
                                }
                            )

                    Unexpected unexpected ->
                        Just
                            (DescribeFloat
                                { id = details.id
                                , found =
                                    Found unexpected.range
                                        ( String.fromFloat newFloat, newFloat )
                                }
                            )

            else
                Nothing

        _ ->
            Nothing


updateFoundString id newString desc =
    case desc of
        DescribeString strId range _ ->
            if strId == id then
                Just (DescribeString strId range newString)

            else
                Nothing

        DescribeMultiline strId range _ ->
            if strId == id then
                Just (DescribeMultiline strId range newString)

            else
                Nothing

        _ ->
            Nothing


updateFoundInt id newInt desc =
    case desc of
        DescribeInteger details ->
            if details.id == id then
                case details.found of
                    Found floatRng fl ->
                        Just
                            (DescribeInteger
                                { id = details.id
                                , found =
                                    Found floatRng
                                        newInt
                                }
                            )

                    Unexpected unexpected ->
                        Just
                            (DescribeInteger
                                { id = details.id
                                , found =
                                    Found unexpected.range
                                        newInt
                                }
                            )

            else
                Nothing

        _ ->
            Nothing


{-| -}
getDescription : Parsed -> Found Description
getDescription (Parsed parsed) =
    parsed.found


{-| -}
getDesc : { start : Int, end : Int } -> Parsed -> List Description
getDesc offset (Parsed parsed) =
    getWithinFound offset parsed.found


{-| -}
getWithinFound : { start : Int, end : Int } -> Found Description -> List Description
getWithinFound offset found =
    case found of
        Found range item ->
            if withinOffsetRange offset range then
                if isPrimitive item then
                    [ item ]

                else
                    [ item ]
                        ++ getContainingDescriptions item offset

            else
                []

        Unexpected unexpected ->
            []


withinFoundLeaf offset found =
    case found of
        Found range item ->
            withinOffsetRange offset range

        Unexpected unexpected ->
            withinOffsetRange offset unexpected.range


isPrimitive : Description -> Bool
isPrimitive description =
    case description of
        DescribeBlock _ ->
            False

        Record _ ->
            False

        OneOf _ ->
            False

        ManyOf _ ->
            False

        StartsWith _ ->
            False

        DescribeTree details ->
            False

        -- Primitives
        DescribeBoolean found ->
            True

        DescribeInteger found ->
            True

        DescribeFloat found ->
            True

        DescribeText _ ->
            True

        DescribeString _ _ _ ->
            True

        DescribeMultiline _ _ _ ->
            True

        DescribeNothing _ ->
            True


{-| -}
getContainingDescriptions : Description -> { start : Int, end : Int } -> List Description
getContainingDescriptions description offset =
    case description of
        DescribeNothing _ ->
            []

        DescribeBlock details ->
            getWithinFound offset details.found

        Record details ->
            case details.found of
                Found range fields ->
                    if withinOffsetRange offset range then
                        List.concatMap (getWithinFound offset << Tuple.second) fields

                    else
                        []

                Unexpected unexpected ->
                    if withinOffsetRange offset unexpected.range then
                        []

                    else
                        []

        OneOf one ->
            getWithinFound offset one.child

        ManyOf many ->
            List.concatMap (getWithinFound offset) many.children

        StartsWith details ->
            if withinOffsetRange offset details.range then
                getContainingDescriptions details.first.found offset
                    ++ getContainingDescriptions details.second.found offset

            else
                []

        DescribeTree details ->
            if withinOffsetRange offset details.range then
                List.concatMap (getWithinNested offset) details.children

            else
                []

        -- Primitives
        DescribeBoolean details ->
            if withinFoundLeaf offset details.found then
                [ description ]

            else
                []

        DescribeInteger details ->
            if withinFoundLeaf offset details.found then
                [ description ]

            else
                []

        DescribeFloat details ->
            if withinFoundLeaf offset details.found then
                [ description ]

            else
                []

        DescribeText txt ->
            if withinOffsetRange offset txt.range then
                [ description ]

            else
                []

        DescribeString id range str ->
            if withinOffsetRange offset range then
                [ description ]

            else
                []

        DescribeMultiline id range str ->
            if withinOffsetRange offset range then
                [ description ]

            else
                []


getWithinNested offset (Nested nest) =
    -- case nest.content of
    --     ( desc, items ) ->
    --         getContainingDescriptions desc offset
    List.concatMap
        (\item ->
            getContainingDescriptions item offset
        )
        nest.content



{- EDITABLE BLOCKS -}
{- Editable blocks generally have

   - Id type -> so edit events cant be emitted.
   - Range -> To inform where in the source document you're working.
   - Expectation ->


-}


{-| Parse an `Int` block.
-}
int : ({ id : Id, range : Range } -> Int -> a) -> Block a
int view =
    Value
        { converter =
            \desc ->
                case desc of
                    DescribeInteger details ->
                        case details.found of
                            Found rng i ->
                                Outcome.Success
                                    (view
                                        { id = details.id
                                        , range = rng
                                        }
                                        i
                                    )

                            Unexpected unexpected ->
                                Outcome.Almost (Uncertain ( unexpected, [] ))

                    _ ->
                        Outcome.Failure Error.NoMatch
        , expect = ExpectInteger 0
        , parser =
            \seed ->
                let
                    ( id, newSeed ) =
                        Id.step seed
                in
                ( newSeed
                , Parser.map
                    (\foundInt ->
                        DescribeInteger
                            { id = id
                            , found = foundInt
                            }
                    )
                    Parse.int
                )
        }


{-| -}
float : ({ id : Id, range : Range } -> ( String, Float ) -> a) -> Block a
float view =
    Value
        { converter =
            \desc ->
                case desc of
                    DescribeFloat details ->
                        case details.found of
                            Found rng i ->
                                Outcome.Success
                                    (view
                                        { id = details.id
                                        , range = rng
                                        }
                                        i
                                    )

                            Unexpected unexpected ->
                                Outcome.Almost (Uncertain ( unexpected, [] ))

                    _ ->
                        Outcome.Failure Error.NoMatch
        , expect = ExpectFloat 0
        , parser =
            \seed ->
                let
                    ( id, newSeed ) =
                        Id.step seed
                in
                ( newSeed
                , Parser.map
                    (\fl ->
                        DescribeFloat
                            { id = id
                            , found = fl
                            }
                    )
                    Parse.float
                )
        }


{-| -}
string : ({ id : Id, range : Range } -> String -> a) -> Block a
string view =
    Value
        { expect = ExpectString "-- Replace Me --"
        , converter =
            \desc ->
                case desc of
                    DescribeString id range str ->
                        Outcome.Success
                            (view
                                { id = id
                                , range = range
                                }
                                str
                            )

                    _ ->
                        Outcome.Failure Error.NoMatch
        , parser =
            \seed ->
                let
                    ( id, newSeed ) =
                        Id.step seed
                in
                ( newSeed
                , Parser.succeed
                    (\start val end ->
                        DescribeString id
                            { start = start
                            , end = end
                            }
                            val
                    )
                    |= Parse.getPosition
                    |= Parser.getChompedString
                        (Parser.chompWhile
                            (\c -> c /= '\n')
                        )
                    |= Parse.getPosition
                )
        }


{-| -}
multiline : ({ id : Id, range : Range } -> String -> a) -> Block a
multiline view =
    Value
        { expect = ExpectMultiline "REPLACE"
        , converter =
            \desc ->
                case desc of
                    DescribeMultiline id range str ->
                        Outcome.Success
                            (view
                                { id = id
                                , range = range
                                }
                                str
                            )

                    _ ->
                        Outcome.Failure Error.NoMatch
        , parser =
            \seed ->
                let
                    ( id, newSeed ) =
                        Id.step seed
                in
                ( newSeed
                , Parser.map
                    (\( pos, str ) ->
                        DescribeMultiline id pos str
                    )
                    (Parse.withRange
                        (Parser.getIndent
                            |> Parser.andThen
                                (\indentation ->
                                    Parser.loop "" (Parse.indentedString indentation)
                                )
                        )
                    )
                )
        }


{-| Parse either `True` or `False`.
-}
bool : ({ id : Id, range : Range } -> Bool -> a) -> Block a
bool view =
    Value
        { expect = ExpectBoolean False
        , converter =
            \desc ->
                case desc of
                    DescribeBoolean details ->
                        case details.found of
                            Found rng b ->
                                Outcome.Success
                                    (view
                                        { id = details.id
                                        , range = rng
                                        }
                                        b
                                    )

                            Unexpected unexpected ->
                                Outcome.Almost (Uncertain ( unexpected, [] ))

                    _ ->
                        Outcome.Failure Error.NoMatch
        , parser =
            \seed ->
                let
                    ( id, newSeed ) =
                        Id.step seed
                in
                ( newSeed
                , Parser.map
                    (\( range, boolResult ) ->
                        DescribeBoolean
                            { id = id
                            , found =
                                case boolResult of
                                    Err err ->
                                        Unexpected
                                            { range = range
                                            , problem = Error.BadBool
                                            }

                                    Ok b ->
                                        Found range
                                            b
                            }
                    )
                    (Parse.withRange
                        (Parser.oneOf
                            [ Parser.token (Parser.Token "True" (Error.Expecting "True"))
                                |> Parser.map (always (Ok True))
                            , Parser.token (Parser.Token "False" (Error.Expecting "False"))
                                |> Parser.map (always (Ok False))
                            , Parser.map Err Parse.word
                            ]
                        )
                    )
                )
        }


{-| -}
oneOf :
    ({ id : Id
     , range : Range
     , options : List Expectation
     }
     -> a
     -> b
    )
    -> List (Block a)
    -> Block b
oneOf view blocks =
    let
        matchBlock description blck found =
            case found of
                Outcome.Failure _ ->
                    case renderBlock blck description of
                        Outcome.Failure _ ->
                            found

                        otherwise ->
                            otherwise

                _ ->
                    found

        expectations =
            List.map getBlockExpectation blocks
    in
    Value
        { expect = ExpectOneOf expectations
        , converter =
            \desc ->
                case desc of
                    OneOf details ->
                        case details.child of
                            Found rng found ->
                                List.foldl (matchBlock found) (Outcome.Failure Error.NoMatch) blocks
                                    |> mapSuccessAndRecovered
                                        (\x ->
                                            view
                                                { id = details.id
                                                , options = details.choices
                                                , range = rng
                                                }
                                                x
                                        )

                            Unexpected unexpected ->
                                uncertain unexpected

                    _ ->
                        Outcome.Failure Error.NoMatch
        , parser =
            \seed ->
                let
                    gatherParsers myBlock details =
                        let
                            ( currentSeed, parser ) =
                                getParser details.seed myBlock
                        in
                        case blockName myBlock of
                            Just name ->
                                { blockNames = name :: details.blockNames
                                , childBlocks = Parser.map Ok parser :: details.childBlocks
                                , childValues = details.childValues
                                , seed = currentSeed
                                }

                            Nothing ->
                                { blockNames = details.blockNames
                                , childBlocks = details.childBlocks
                                , childValues = Parser.map Ok parser :: details.childValues
                                , seed = currentSeed
                                }

                    children =
                        List.foldl gatherParsers
                            { blockNames = []
                            , childBlocks = []
                            , childValues = []
                            , seed = newSeed
                            }
                            blocks

                    blockParser =
                        Parser.succeed identity
                            |. Parser.token (Parser.Token "|>" Error.BlockStart)
                            |. Parser.chompWhile (\c -> c == ' ')
                            |= Parser.oneOf
                                (List.reverse children.childBlocks
                                    ++ [ Parser.getIndent
                                            |> Parser.andThen
                                                (\indentation ->
                                                    Parser.succeed
                                                        (\( pos, foundWord ) ->
                                                            Err ( pos, Error.UnknownBlock children.blockNames )
                                                        )
                                                        |= Parse.withRange Parse.word
                                                        |. Parse.newline
                                                        |. Parser.loop "" (Parse.raggedIndentedStringAbove indentation)
                                                )
                                       ]
                                )

                    ( parentId, newSeed ) =
                        Id.step seed
                in
                ( children.seed
                , Parser.succeed
                    (\( range, result ) ->
                        case result of
                            Ok found ->
                                OneOf
                                    { choices = expectations
                                    , child = Found range found
                                    , id = parentId
                                    }

                            Err ( pos, unexpected ) ->
                                OneOf
                                    { choices = expectations
                                    , child =
                                        Unexpected
                                            { range = pos
                                            , problem = unexpected
                                            }
                                    , id = parentId
                                    }
                    )
                    |= Parse.withRange
                        (Parser.oneOf
                            (blockParser :: List.reverse (unexpectedInOneOf expectations :: children.childValues))
                        )
                )
        }


unexpectedInOneOf expectations =
    Parser.getIndent
        |> Parser.andThen
            (\indentation ->
                Parser.succeed
                    (\( pos, foundWord ) ->
                        Err ( pos, Error.FailMatchOneOf (List.map humanReadableExpectations expectations) )
                    )
                    |= Parse.withRange Parse.word
            )


{-| Many blocks that are all at the same indentation level.
-}
manyOf :
    ({ id : Id
     , range : Range
     }
     -> List a
     -> b
    )
    -> List (Block a)
    -> Block b
manyOf view blocks =
    let
        expectations =
            List.map getBlockExpectation blocks
    in
    Value
        { expect = ExpectManyOf expectations
        , converter =
            \desc ->
                let
                    matchBlock description blck found =
                        case found of
                            Outcome.Failure _ ->
                                case renderBlock blck description of
                                    Outcome.Failure _ ->
                                        found

                                    otherwise ->
                                        otherwise

                            _ ->
                                found

                    getRendered id choices found ( existingResult, index ) =
                        case found of
                            Unexpected unexpected ->
                                ( uncertain unexpected
                                , index + 1
                                )

                            Found range child ->
                                ( mergeWith (::)
                                    (List.foldl (matchBlock child) (Outcome.Failure Error.NoMatch) blocks)
                                    existingResult
                                , index + 1
                                )
                in
                case desc of
                    ManyOf many ->
                        List.foldl (getRendered many.id many.choices) ( Outcome.Success [], 0 ) many.children
                            |> Tuple.first
                            |> mapSuccessAndRecovered
                                (view
                                    { id = many.id
                                    , range = many.range
                                    }
                                    << List.reverse
                                )

                    _ ->
                        Outcome.Failure Error.NoMatch
        , parser =
            \seed ->
                let
                    ( parentId, newSeed ) =
                        Id.step seed

                    ( _, childStart ) =
                        Id.step newSeed

                    reseeded =
                        Id.reseed childStart
                in
                ( reseeded
                , Parser.succeed
                    (\( range, results ) ->
                        ManyOf
                            { choices = expectations
                            , id = parentId
                            , range = range
                            , children = List.map resultToFound results
                            }
                    )
                    |= Parse.withRange
                        (Parse.withIndent
                            (\indentation ->
                                Parser.loop
                                    { parsedSomething = False
                                    , found = []
                                    , seed = childStart
                                    }
                                    (Parse.blocksOrNewlines indentation blocks)
                            )
                        )
                )
        }


{-| -}
type Tree item
    = Tree
        { index : List Int
        , icon : Icon
        , content : List item
        , children :
            List (Tree item)
        }


{-| -}
type Icon
    = Bullet
    | Number


{-| It can be useful to parse a tree structure. For example, here's a nested list.

    | List
        - item one
        - item two
            - nested item two

            additional text for nested item two
        - item three
            - nested item three

In order to parse the above, you could define a block as

    Mark.nested "List"
        ((Nested nested) ->
        -- Do something with nested.content and nested.children
        )
        text

**Note** the indentation is always a multiple of 4.

-}
tree :
    String
    ->
        ({ id : Id
         , range : Range
         }
         -> List (Tree item)
         -> result
        )
    -> Block item
    -> Block result
tree name view contentBlock =
    let
        expectation =
            ExpectTree (getBlockExpectation contentBlock) []
    in
    Block name
        { expect = expectation
        , converter =
            \description ->
                case description of
                    DescribeTree details ->
                        details.children
                            |> reduceRender Index.zero (renderTreeNodeSmall contentBlock)
                            |> Tuple.second
                            |> mapSuccessAndRecovered
                                (view
                                    { id = details.id
                                    , range = details.range
                                    }
                                )

                    _ ->
                        Outcome.Failure Error.NoMatch
        , parser =
            \seed ->
                let
                    ( newId, newSeed ) =
                        Id.step seed

                    reseeded =
                        Id.reseed newSeed
                in
                ( reseeded
                , Parse.withIndent
                    (\baseIndent ->
                        Parser.succeed identity
                            |. Parser.keyword
                                (Parser.Token name
                                    (Error.ExpectingBlockName name)
                                )
                            |. Parser.chompWhile (\c -> c == ' ')
                            |. Parse.skipBlankLineWith ()
                            |= Parser.map
                                (\( pos, result ) ->
                                    DescribeTree
                                        { id = newId
                                        , children = Parse.buildTree (baseIndent + 4) result
                                        , range = pos
                                        , expected = expectation
                                        }
                                )
                                (Parse.withRange
                                    (Parser.loop
                                        ( { base = baseIndent + 4
                                          , prev = baseIndent + 4
                                          }
                                        , []
                                        )
                                        (Parse.indentedBlocksOrNewlines
                                            seed
                                            contentBlock
                                        )
                                    )
                                )
                    )
                )
        }


iconParser =
    Parser.oneOf
        [ Parser.succeed Desc.Bullet
            |. Parser.chompIf (\c -> c == '-') (Error.Expecting "-")
            |. Parser.chompWhile (\c -> c == '-' || c == ' ')
        , Parser.succeed Desc.AutoNumber
            |. Parser.chompIf (\c -> c == '#') (Error.Expecting "#")
            |. Parser.chompWhile (\c -> c == '.' || c == ' ')
        ]


{-| -}
renderTreeNodeSmall :
    Block item
    -> Index.Index
    -> Nested Description
    -> Outcome.Outcome Error.AstError (Uncertain (Tree item)) (Tree item)
renderTreeNodeSmall contentBlock index (Nested cursor) =
    let
        ( newIndex, renderedChildren ) =
            reduceRender index (renderTreeNodeSmall contentBlock) cursor.children

        ( _, renderedContent ) =
            reduceRender newIndex
                (\i content ->
                    renderBlock contentBlock content
                )
                cursor.content
    in
    mergeWith
        (\content children ->
            Tree
                { icon =
                    case cursor.icon of
                        Desc.Bullet ->
                            Bullet

                        Desc.AutoNumber ->
                            Number
                , index = Index.toList index
                , content = content
                , children = children
                }
        )
        renderedContent
        renderedChildren


reduceRender :
    Index.Index
    -> (Index.Index -> thing -> Outcome.Outcome Error.AstError (Uncertain other) other)
    -> List thing
    -> ( Index.Index, Outcome.Outcome Error.AstError (Uncertain (List other)) (List other) )
reduceRender index fn list =
    list
        |> List.foldl
            (\x ( i, gathered ) ->
                Tuple.pair (Index.increment i) <|
                    case gathered of
                        Outcome.Success remain ->
                            case fn i x of
                                Outcome.Success newThing ->
                                    Outcome.Success (newThing :: remain)

                                Outcome.Almost (Uncertain err) ->
                                    Outcome.Almost (Uncertain err)

                                Outcome.Almost (Recovered err data) ->
                                    Outcome.Almost
                                        (Recovered err
                                            (data :: remain)
                                        )

                                Outcome.Failure f ->
                                    Outcome.Failure f

                        almostOrfailure ->
                            almostOrfailure
            )
            ( index, Outcome.Success [] )
        |> Tuple.mapSecond (Outcome.mapSuccess List.reverse)


errorToList ( x, xs ) =
    x :: xs


{-| -}
onError : (List { range : Range } -> a) -> Block a -> Block a
onError recover myBlock =
    case myBlock of
        Block name details ->
            Block name
                { expect = details.expect
                , parser = details.parser
                , converter =
                    \desc ->
                        case details.converter desc of
                            Outcome.Success a ->
                                Outcome.Success a

                            Outcome.Almost (Recovered err a) ->
                                Outcome.Almost (Recovered err a)

                            Outcome.Almost (Uncertain errs) ->
                                Outcome.Almost
                                    (Recovered errs
                                        (recover
                                            (List.map (\e -> { range = e.range }) (errorToList errs))
                                        )
                                    )

                            Outcome.Failure f ->
                                Outcome.Failure f
                }

        Value details ->
            Value
                { expect = details.expect
                , parser = details.parser
                , converter =
                    \desc ->
                        case details.converter desc of
                            Outcome.Success a ->
                                Outcome.Success a

                            Outcome.Almost (Recovered err a) ->
                                Outcome.Almost (Recovered err a)

                            Outcome.Almost (Uncertain errs) ->
                                Outcome.Almost
                                    (Recovered errs
                                        (recover (List.map (\e -> { range = e.range }) (errorToList errs)))
                                    )

                            Outcome.Failure f ->
                                Outcome.Failure f
                }



{- EDITING TEXT -}


{-| -}
type alias Inline data =
    Desc.Inline data


{-| -}
type alias Replacement =
    Parse.Replacement


{-| -}
type alias Styles =
    { bold : Bool
    , italic : Bool
    , strike : Bool
    }


{-| -}
type alias Selection =
    { anchor : Offset
    , focus : Offset
    }


{-| -}
type alias Offset =
    Int


{-| Handling formatted text is a little more involved than may be initially apparent.

Text styling can be overlapped such as

    /My italicized sentence can have *bold* words./

In order to render this, the above sentence is chopped up into `Text` fragments that can have multiple styles active.

  - `view` is the function to render an individual fragment.
  - `inlines` are custom inline blocks.
  - `replacements` will replace characters before rendering. For example, we can replace `...` with the real ellipses unicode character, `…`.

-}
text :
    { view :
        { id : Id
        , range : Range
        , selection : Selection
        }
        -> Styles
        -> String
        -> rendered
    , inlines : List (Inline rendered)
    , replacements : List Replacement
    }
    -> Block (List rendered)
text options =
    let
        inlineExpectations =
            List.map getInlineExpectation options.inlines
    in
    Value
        { expect = ExpectTextBlock inlineExpectations
        , converter = renderText options
        , parser =
            \seed ->
                let
                    ( _, newSeed ) =
                        Id.step seed

                    ( _, returnSeed ) =
                        Id.step newSeed
                in
                ( returnSeed
                , Parse.getPosition
                    |> Parser.andThen
                        (\pos ->
                            Parse.styledText
                                { inlines = inlineExpectations
                                , replacements = options.replacements
                                }
                                newSeed
                                pos
                                emptyStyles
                                []
                        )
                )
        }


getInlineExpectation (Inline details) =
    details.expect


type alias Cursor data =
    Outcome.Outcome Error.AstError (Uncertain data) data


renderText :
    { view :
        { id : Id
        , range : Range
        , selection : Selection
        }
        -> Styles
        -> String
        -> rendered
    , inlines : List (Inline rendered)
    , replacements : List Replacement
    }
    -> Description
    -> Cursor (List rendered)
renderText options description =
    case description of
        DescribeText details ->
            List.foldl (convertTextDescription details.id options) (Outcome.Success []) details.text
                |> mapSuccessAndRecovered List.reverse

        _ ->
            Outcome.Failure Error.NoMatch


textToText (Desc.Text styling txt) =
    Text styling txt


convertTextDescription :
    Id
    ->
        { view :
            { id : Id
            , range : Range
            , selection : Selection
            }
            -> Styles
            -> String
            -> rendered
        , inlines : List (Inline rendered)
        , replacements : List Replacement
        }
    -> TextDescription
    -> Cursor (List rendered)
    -> Cursor (List rendered)
convertTextDescription id options comp cursor =
    case comp of
        Styled range (Desc.Text styling str) ->
            mergeWith (::)
                (Outcome.Success
                    (options.view
                        { id = id
                        , range = range
                        , selection =
                            { anchor = 0
                            , focus = 0
                            }
                        }
                        styling
                        str
                    )
                )
                cursor

        InlineToken details ->
            let
                matchInlineName name ((Inline inlineDetails) as inline) maybeFound =
                    case maybeFound of
                        Nothing ->
                            if name == inlineDetails.name && isToken inline then
                                Just inlineDetails

                            else
                                Nothing

                        _ ->
                            maybeFound

                maybeMatched =
                    List.foldl
                        (matchInlineName details.name)
                        Nothing
                        options.inlines
            in
            case maybeMatched of
                Nothing ->
                    uncertain
                        { range = details.range
                        , problem =
                            Error.UnknownInline
                                (List.map
                                    (Desc.inlineExample
                                        << getInlineExpectation
                                    )
                                    options.inlines
                                )
                        }

                Just matchedInline ->
                    mergeWith (++)
                        (matchedInline.converter [] details.attributes)
                        cursor

        InlineAnnotation details ->
            let
                matchInlineName name ((Inline inlineDetails) as inline) maybeFound =
                    case maybeFound of
                        Nothing ->
                            if name == inlineDetails.name && not (isToken inline) then
                                Just inlineDetails

                            else
                                Nothing

                        _ ->
                            maybeFound

                maybeMatched =
                    List.foldl
                        (matchInlineName details.name)
                        Nothing
                        options.inlines
            in
            case maybeMatched of
                Just matchedInline ->
                    mergeWith (++)
                        (matchedInline.converter
                            (List.map textToText details.text)
                            details.attributes
                        )
                        cursor

                Nothing ->
                    uncertain
                        { range = details.range
                        , problem =
                            Error.UnknownInline
                                (List.map
                                    (Desc.inlineExample
                                        << getInlineExpectation
                                    )
                                    options.inlines
                                )
                        }

        InlineVerbatim details ->
            let
                matchInlineName name ((Inline inlineDetails) as inline) maybeFound =
                    case maybeFound of
                        Nothing ->
                            if
                                isVerbatim inline
                                    && noInlineAttributes inlineDetails.expect
                                    && (name == Nothing)
                            then
                                Just inlineDetails

                            else if isVerbatim inline && name == Just inlineDetails.name then
                                Just inlineDetails

                            else
                                Nothing

                        _ ->
                            maybeFound

                maybeMatched =
                    List.foldl
                        (matchInlineName details.name)
                        Nothing
                        options.inlines
            in
            case maybeMatched of
                Just matchedInline ->
                    mergeWith (++)
                        (matchedInline.converter
                            [ textToText details.text ]
                            details.attributes
                        )
                        cursor

                Nothing ->
                    uncertain
                        { range = details.range
                        , problem =
                            Error.UnknownInline
                                (List.map
                                    (Desc.inlineExample
                                        << getInlineExpectation
                                    )
                                    options.inlines
                                )
                        }

        UnexpectedInline details ->
            uncertain details


{-| -}
isToken : Inline data -> Bool
isToken (Inline inline) =
    case inline.expect of
        ExpectToken _ _ ->
            True

        _ ->
            False


{-| -}
isVerbatim : Inline data -> Bool
isVerbatim (Inline inline) =
    case inline.expect of
        ExpectVerbatim _ _ _ ->
            True

        _ ->
            False



{- TEXT EDITING -}


type Restyle
    = Restyle Styles
    | AddStyle Styles
    | RemoveStyle Styles



{- TEXT EDITING HELP -}


onlyText : TextDescription -> List Text
onlyText txt =
    case txt of
        InlineAnnotation details ->
            details.text

        InlineToken details ->
            []

        InlineVerbatim details ->
            [ details.text ]

        x ->
            []


{-| Folds over a list of styles and merges them if they're compatible
-}
mergeStyles : TextDescription -> List TextDescription -> List TextDescription
mergeStyles inlineEl gathered =
    case gathered of
        [] ->
            [ inlineEl ]

        prev :: tail ->
            case attemptMerge inlineEl prev of
                Nothing ->
                    inlineEl :: prev :: tail

                Just merged ->
                    merged :: tail


attemptMerge : TextDescription -> TextDescription -> Maybe TextDescription
attemptMerge first second =
    case ( first, second ) of
        ( Styled rngOne (Text stylingOne strOne), Styled rngTwo (Text stylingTwo strTwo) ) ->
            if stylingOne == stylingTwo then
                Just (Styled (mergeRanges rngOne rngTwo) (Text stylingOne (strOne ++ strTwo)))

            else
                Nothing

        ( InlineAnnotation one, InlineAnnotation two ) ->
            if one.name == two.name && one.attributes == two.attributes then
                Just <|
                    InlineAnnotation
                        { name = one.name
                        , attributes = one.attributes
                        , range = mergeRanges one.range two.range
                        , text = one.text ++ two.text
                        }

            else
                Nothing

        ( InlineVerbatim one, InlineVerbatim two ) ->
            Nothing

        ( _, _ ) ->
            Nothing


mergeRanges one two =
    { start = one.start
    , end = two.end
    }


emptySelectionEdit =
    { offset = 0
    , elements = []
    , selection = Nothing
    }


doTextEdit :
    Selection
    -> (List TextDescription -> List TextDescription)
    -> TextDescription
    ->
        { elements : List TextDescription
        , offset : Int
        , selection : Maybe (List TextDescription)
        }
    ->
        { elements : List TextDescription
        , offset : Int
        , selection : Maybe (List TextDescription)
        }
doTextEdit { anchor, focus } editFn current cursor =
    let
        start =
            min anchor focus

        end =
            max anchor focus

        len =
            length current
    in
    case cursor.selection of
        Nothing ->
            if cursor.offset <= start && cursor.offset + len >= start then
                {- Start Selection -}
                if cursor.offset + len >= end then
                    {- We finish the selection in this element -}
                    let
                        ( before, afterLarge ) =
                            splitAt (start - cursor.offset) current

                        ( selected, after ) =
                            splitAt (end - start) afterLarge
                    in
                    { offset = cursor.offset + len
                    , elements =
                        after :: editFn [ selected ] ++ (before :: cursor.elements)
                    , selection =
                        Nothing
                    }

                else
                    let
                        ( before, after ) =
                            splitAt (start - cursor.offset) current
                    in
                    { offset = cursor.offset + len
                    , elements =
                        before :: cursor.elements
                    , selection =
                        Just [ after ]
                    }

            else
                { offset = cursor.offset + len
                , elements = current :: cursor.elements
                , selection = cursor.selection
                }

        Just selection ->
            if cursor.offset + len >= end then
                let
                    ( before, after ) =
                        splitAt (end - cursor.offset) current

                    fullSelection =
                        before :: selection
                in
                { offset = cursor.offset + len
                , elements =
                    if cursor.offset + len == end then
                        editFn fullSelection ++ cursor.elements

                    else
                        after :: editFn fullSelection ++ cursor.elements
                , selection = Nothing
                }

            else
                { offset = cursor.offset + len
                , elements = cursor.elements
                , selection = Just (current :: selection)
                }


applyStyles : Restyle -> TextDescription -> TextDescription
applyStyles styling inlineEl =
    case inlineEl of
        Styled range txt ->
            Styled range (applyStylesToText styling txt)

        InlineAnnotation details ->
            InlineAnnotation
                { details
                    | text = List.map (applyStylesToText styling) details.text
                }

        InlineToken details ->
            InlineToken details

        InlineVerbatim details ->
            InlineVerbatim details

        x ->
            x


applyStylesToText styling (Text styles str) =
    case styling of
        Restyle newStyle ->
            Text newStyle str

        RemoveStyle toRemove ->
            Text
                { bold = styles.bold && not toRemove.bold
                , italic = styles.italic && not toRemove.italic
                , strike = styles.strike && not toRemove.strike
                }
                str

        AddStyle toAdd ->
            Text
                { bold = styles.bold || toAdd.bold
                , italic = styles.italic || toAdd.italic
                , strike = styles.strike || toAdd.strike
                }
                str


length : TextDescription -> Int
length inlineEl =
    case inlineEl of
        Styled _ txt ->
            textLength txt

        InlineAnnotation details ->
            List.sum (List.map textLength details.text)

        InlineToken _ ->
            0

        InlineVerbatim details ->
            textLength details.text

        UnexpectedInline err ->
            0


textLength : Desc.Text -> Int
textLength (Text _ str) =
    String.length str


{-| Splits the current element based on an index.

This function should only be called when the offset is definitely contained within the element provided, not on the edges.

-}
splitAt : Offset -> TextDescription -> ( TextDescription, TextDescription )
splitAt offset inlineEl =
    case inlineEl of
        Styled range txt ->
            let
                ( leftRange, rightRange ) =
                    splitRange offset range

                ( leftText, rightText ) =
                    splitText offset txt
            in
            ( Styled leftRange leftText
            , Styled rightRange rightText
            )

        -- InlineAnnotation name attrs textElements ->
        InlineAnnotation details ->
            let
                { left, right } =
                    List.foldl (splitTextElements offset)
                        { offset = 0
                        , left = []
                        , right = []
                        }
                        details.text

                splitTextElements off (Text styling txt) cursor =
                    if off >= cursor.offset && off <= cursor.offset + String.length txt then
                        { offset = cursor.offset + String.length txt
                        , left = Text styling (String.left (offset - cursor.offset) txt) :: cursor.left
                        , right = Text styling (String.dropLeft (offset - cursor.offset) txt) :: cursor.right
                        }

                    else if off < cursor.offset then
                        { offset = cursor.offset + String.length txt
                        , left = cursor.left
                        , right = Text styling txt :: cursor.right
                        }

                    else
                        { offset = cursor.offset + String.length txt
                        , left = Text styling txt :: cursor.left
                        , right = cursor.right
                        }

                ( leftRange, rightRange ) =
                    splitRange offset details.range
            in
            ( InlineAnnotation
                { name = details.name
                , range = leftRange
                , text = List.reverse left
                , attributes = details.attributes
                }
            , InlineAnnotation
                { name = details.name
                , range = rightRange
                , text = List.reverse right
                , attributes = details.attributes
                }
            )

        InlineToken details ->
            -- This shoudn't happen because we're expecting the offset
            -- to be within the range, and a token has a lenght of 0
            ( Styled emptyRange (Text emptyStyles "")
            , InlineToken details
            )

        InlineVerbatim details ->
            let
                ( leftRange, rightRange ) =
                    splitRange offset details.range

                ( leftText, rightText ) =
                    splitText offset details.text
            in
            ( --ExpectVerbatim name attrs (String.left offset str)
              InlineVerbatim
                { details
                    | range = leftRange
                    , text = leftText
                }
            , InlineVerbatim
                { details
                    | range = rightRange
                    , text = rightText
                }
            )

        UnexpectedInline err ->
            ( UnexpectedInline err
            , UnexpectedInline err
            )


splitText offset (Text styling str) =
    ( Text styling (String.left offset str)
    , Text styling (String.dropLeft offset str)
    )


splitRange offset range =
    let
        -- TODO: This stays on the same line
        middle =
            { offset = range.start.offset + offset
            , line = range.start.line
            , column = range.start.column + offset
            }
    in
    ( range
    , range
    )


emptyRange =
    { start =
        { offset = 0
        , line = 1
        , column = 1
        }
    , end =
        { offset = 0
        , line = 1
        , column = 1
        }
    }
