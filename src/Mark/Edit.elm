module Mark.Edit exposing
    ( update, Id, Edit
    , Offset
    , insertText, deleteText
    , Styles, restyle, addStyles, removeStyles
    , annotate, verbatim
    , replace, delete, insertAt
    )

{-| This module allows you to make **edits** to `Parsed`, that intermediate data structure we talked about in [`Mark`](Mark).

This means you can build an editor for your document.

In order to make edits to your document you need an [`Id`](#Id) and an [`Edit`](#Edit).

Once you have those you can [`update`](#update) your document, which can succeed or fail depending on if the edit was valid.


# Updating `Parsed`

@docs update, Id, Edit


# Text Edits

Here are edits you cna make against [`Mark.text`](Mark#text) and [`Mark.textWith`](Mark#textWith) blocks.

**Note** These edits don't apply to [`Mark.string`](Mark#string). If you want to modify a `Mark.string`, use [`Mark.Edit.replace`](Mark-Edit#replace).

@docs Offset

@docs insertText, deleteText

@docs Styles, restyle, addStyles, removeStyles

@docs annotate, verbatim


# General Edits

@docs replace, delete, insertAt

-}

import Mark.Error
import Mark.Internal.Description as Desc exposing (..)
import Mark.Internal.Error as Error
import Mark.Internal.Format as Format
import Mark.Internal.Id as Id exposing (..)
import Mark.Internal.Index as Index
import Mark.Internal.Outcome as Outcome
import Mark.Internal.Parser as Parse
import Mark.New
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


{-| Every block has an `Id`. You can retrieve the `Id` for a block using [`Mark.withId`](Mark#withId)
-}
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
type Edit
    = Replace Id Mark.New.Block
      -- Create an element in a ManyOf
      -- Indexes overflow, so if it's too large, it just puts it at the end.
      -- Indexes that are below 0 and clamped to 0
    | InsertAt Id Int Mark.New.Block
    | Delete Id Int
      -- Text Editing
    | StyleText Id Offset Offset Restyle
    | Annotate Id Offset Offset Annotation
    | ReplaceSelection Id Offset Offset (List Mark.New.Text)


type Annotation
    = Annotation String (List ( String, Mark.New.Block ))
    | Verbatim String (List ( String, Mark.New.Block ))


{-| -}
deleteText : Id -> Offset -> Offset -> Edit
deleteText id start end =
    ReplaceSelection id
        start
        end
        []


{-| -}
insertText : Id -> Offset -> List Mark.New.Text -> Edit
insertText id at els =
    ReplaceSelection id at at els


{-| The `Block` mentioned here is actually a `Mark.New.Block`. Use [`Mark.New`](/Mark-New) to create the new block you'd like.
-}
replace : Id -> Mark.New.Block -> Edit
replace =
    Replace


{-| Delete a block at an index within a `Mark.manyOf`.
-}
delete : Id -> Int -> Edit
delete =
    Delete


{-| Insert a block at an index within a `Mark.manyOf`.
-}
insertAt : Id -> Int -> Mark.New.Block -> Edit
insertAt =
    InsertAt


{-| Put the current text selection within an `annotation` with some attributes.

Here's an example that would turn the selection into a `link`.

    Mark.Edit.annotate id
        start
        end
        "link"
        [ ( "url", Mark.New.string "https://guide.elm-lang.org/" ) ]

**Note** existing annotations within the selection are removed.

-}
annotate : Id -> Offset -> Offset -> String -> List ( String, Mark.New.Block ) -> Edit
annotate id start end name attrs =
    Annotate id start end (Annotation name attrs)


{-| Same as `annotate`, but creates a `verbatim` instead.

**Note** existing annotations within the selection are removed.

-}
verbatim : Id -> Offset -> Offset -> String -> List ( String, Mark.New.Block ) -> Edit
verbatim id start end name attrs =
    Annotate id start end (Verbatim name attrs)


{-| Remove all styling on the text selection and replace with the given styling.
-}
restyle : Id -> Offset -> Offset -> Styles -> Edit
restyle id start end styles =
    StyleText id start end (Restyle styles)


{-| Remove the given styles if they are present in the text selection.

**Note:** Other styling is unaffected.

-}
removeStyles : Id -> Offset -> Offset -> Styles -> Edit
removeStyles id start end styles =
    StyleText id start end (RemoveStyle styles)


{-| Add the given styles if they are not present in the text selection.

**Note:** Other styling is unaffected.

-}
addStyles : Id -> Offset -> Offset -> Styles -> Edit
addStyles id start end styles =
    StyleText id start end (AddStyle styles)


editAtId id fn indentation pos desc =
    if Desc.getId desc == id then
        fn indentation pos desc

    else
        NoIdFound


{-| Set the content of the current block
-}
replaceOption id i pos original new desc =
    let
        created =
            create
                { indent = i
                , base = pos
                , expectation = new
                , seed = original.currentSeed
                }
    in
    case desc of
        OneOf one ->
            -- When we're replacing the OneOf, we actually want to replace it's contents.
            if List.any (matchExpected new) one.choices then
                let
                    newSize =
                        getSize created.desc

                    existingSize =
                        sizeFromRange (getFoundRange one.child)
                in
                case one.child of
                    Found range val ->
                        EditMade
                            (Just created.seed)
                            (minusSize newSize existingSize
                                |> sizeToPush
                            )
                            (OneOf { one | child = Found range created.desc })

                    Unexpected unexpected ->
                        EditMade
                            (Just created.seed)
                            (minusSize newSize existingSize
                                |> sizeToPush
                            )
                            (OneOf { one | child = Found unexpected.range created.desc })

            else
                ErrorMakingEdit
                    (Error.DocumentDoesntAllow
                        (Desc.humanReadableExpectations new)
                        (List.map Desc.humanReadableExpectations one.choices)
                    )

        _ ->
            if Desc.match desc new then
                let
                    newSize =
                        getSize created.desc

                    existingSize =
                        getSize desc
                in
                EditMade
                    (Just created.seed)
                    (minusSize newSize existingSize
                        |> sizeToPush
                    )
                    created.desc

            else
                ErrorMakingEdit
                    (Error.DocumentDoesntAllow
                        (Desc.humanReadableExpectations new)
                        [ Desc.descriptionToString desc ]
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
            EditMade
                Nothing
                cleaned.push
                (ManyOf
                    { many
                        | children = List.reverse cleaned.items
                    }
                )

        _ ->
            ErrorMakingEdit Error.InvalidDelete


{-| -}
update : Document data -> Edit -> Parsed -> Result (List Mark.Error.Error) Parsed
update doc edit (Parsed original) =
    let
        editFn =
            case edit of
                Replace id new ->
                    editAtId id <|
                        \i pos desc ->
                            -- if Desc.match desc new then
                            replaceOption id i pos original new desc

                InsertAt id index new ->
                    editAtId id <|
                        \indentation pos desc ->
                            case desc of
                                ManyOf many ->
                                    if List.any (matchExpected new) many.choices then
                                        let
                                            inserted =
                                                makeInsertAt
                                                    original.currentSeed
                                                    index
                                                    indentation
                                                    many
                                                    new
                                        in
                                        EditMade
                                            (Just inserted.seed)
                                            inserted.push
                                            (ManyOf
                                                { many
                                                    | children =
                                                        inserted.updated
                                                }
                                            )

                                    else
                                        ErrorMakingEdit
                                            (Error.DocumentDoesntAllow
                                                (Desc.humanReadableExpectations new)
                                                (List.map Desc.humanReadableExpectations many.choices)
                                            )

                                _ ->
                                    -- inserts= by index only works for `manyOf`
                                    ErrorMakingEdit Error.InvalidInsert

                Delete id index ->
                    editAtId id
                        (makeDeleteBlock id index)

                StyleText id start end restyleAction ->
                    editAtId id
                        (\indent pos desc ->
                            case desc of
                                DescribeText details ->
                                    let
                                        newTexts =
                                            details.text
                                                |> List.foldl
                                                    (doTextEdit start
                                                        end
                                                        (List.map (applyStyles restyleAction))
                                                    )
                                                    emptySelectionEdit
                                                |> .elements
                                                |> List.foldl mergeStyles []
                                    in
                                    EditMade
                                        Nothing
                                        (Just (pushNewTexts details.text newTexts))
                                        (DescribeText
                                            { details | text = newTexts }
                                        )

                                _ ->
                                    ErrorMakingEdit Error.InvalidTextEdit
                        )

                Annotate id start end wrapper ->
                    editAtId id
                        (\indent pos desc ->
                            case desc of
                                DescribeText details ->
                                    let
                                        newTexts =
                                            details.text
                                                |> List.foldl
                                                    (doTextEdit start
                                                        end
                                                        (\els ->
                                                            let
                                                                textStart =
                                                                    getTextStart els
                                                                        |> Maybe.withDefault pos

                                                                wrapped =
                                                                    case wrapper of
                                                                        Annotation name attrs ->
                                                                            ExpectInlineBlock
                                                                                { name = name
                                                                                , kind =
                                                                                    SelectText
                                                                                        (List.concatMap onlyText els)
                                                                                , fields = attrs
                                                                                }

                                                                        Verbatim name attrs ->
                                                                            ExpectInlineBlock
                                                                                { name = name
                                                                                , kind =
                                                                                    SelectString
                                                                                        (List.concatMap onlyText els
                                                                                            |> List.map textString
                                                                                            |> String.join ""
                                                                                        )
                                                                                , fields = attrs
                                                                                }

                                                                ( end_, newText ) =
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
                                    EditMade
                                        Nothing
                                        (Just (pushNewTexts details.text newTexts))
                                        (DescribeText { details | text = newTexts })

                                _ ->
                                    ErrorMakingEdit Error.InvalidTextEdit
                        )

                ReplaceSelection id start end newTextEls ->
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
                                                    (doTextEdit start
                                                        end
                                                        makeNewText
                                                    )
                                                    emptySelectionEdit
                                                |> .elements
                                                |> List.foldl mergeStyles []
                                    in
                                    EditMade
                                        Nothing
                                        (Just (pushNewTexts details.text newTexts))
                                        (DescribeText { details | text = newTexts })

                                _ ->
                                    ErrorMakingEdit Error.InvalidTextEdit
                        )
    in
    original.found
        |> makeFoundEdit
            { makeEdit = editFn
            , indentation = 0
            }
        |> prepareResults doc original


prepareResults doc original edited =
    case edited of
        NoIdFound ->
            Err [ Error.idNotFound ]

        ErrorMakingEdit err ->
            Err [ Error.renderEditError err ]

        EditMade maybeSeed maybePush newDescription ->
            let
                newParsed =
                    Parsed
                        { original
                            | found = newDescription
                            , currentSeed =
                                case maybeSeed of
                                    Nothing ->
                                        original.currentSeed

                                    Just seed ->
                                        seed
                        }
            in
            case Desc.render doc newParsed of
                Outcome.Success _ ->
                    Ok newParsed

                Outcome.Almost details ->
                    Err details.errors

                Outcome.Failure errs ->
                    Err errs


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


type alias EditCursor =
    -- An edit takes the indentation level
    -- , the last reference position
    -- and the current description
    { makeEdit : Int -> Position -> Description -> EditOutcome Description
    , indentation : Int
    }


type alias Push =
    Maybe Size


type alias Size =
    { offset : Int
    , line : Int
    }


type EditOutcome desc
    = ErrorMakingEdit Error.EditErr
    | EditMade (Maybe Id.Seed) Push desc
    | NoIdFound


mapEdit fn edit =
    case edit of
        ErrorMakingEdit err ->
            ErrorMakingEdit err

        EditMade maybeSeed maybePush desc ->
            EditMade maybeSeed maybePush (fn desc)

        NoIdFound ->
            NoIdFound


{-| -}
makeFoundEdit : EditCursor -> Found Description -> EditOutcome (Found Description)
makeFoundEdit cursor foundDesc =
    case foundDesc of
        Found range desc ->
            makeEdit cursor desc
                |> mapEdit (Found range)

        Unexpected unexpected ->
            NoIdFound


{-| -}
makeEdit : EditCursor -> Description -> EditOutcome Description
makeEdit cursor desc =
    case desc of
        DescribeBlock details ->
            case cursor.makeEdit cursor.indentation (foundStart details.found) desc of
                NoIdFound ->
                    -- dive further
                    case makeFoundEdit (increaseIndent cursor) details.found of
                        EditMade maybeSeed maybePush newFound ->
                            EditMade maybeSeed
                                maybePush
                                (DescribeBlock
                                    { details
                                        | found = newFound
                                    }
                                )

                        NoIdFound ->
                            NoIdFound

                        ErrorMakingEdit err ->
                            ErrorMakingEdit err

                otherwise ->
                    otherwise

        Record details ->
            case cursor.makeEdit (cursor.indentation + 1) (foundStart details.found) desc of
                NoIdFound ->
                    case details.found of
                        Unexpected unexpected ->
                            NoIdFound

                        Found rng fields ->
                            case editFields (increaseIndent (increaseIndent cursor)) fields of
                                EditMade maybeSeed maybePush updatedFields ->
                                    EditMade maybeSeed
                                        maybePush
                                        (Record
                                            { details
                                                | found =
                                                    Found (expandRange maybePush rng)
                                                        updatedFields
                                            }
                                        )

                                NoIdFound ->
                                    NoIdFound

                                ErrorMakingEdit err ->
                                    ErrorMakingEdit err

                otherwise ->
                    otherwise

        OneOf details ->
            case cursor.makeEdit cursor.indentation (foundStart details.child) desc of
                NoIdFound ->
                    -- dive further
                    case makeFoundEdit (increaseIndent cursor) details.child of
                        EditMade maybeSeed maybePush newFound ->
                            EditMade maybeSeed
                                maybePush
                                (OneOf
                                    { details
                                        | child = expandFound maybePush newFound
                                    }
                                )

                        NoIdFound ->
                            NoIdFound

                        ErrorMakingEdit err ->
                            ErrorMakingEdit err

                otherwise ->
                    otherwise

        ManyOf many ->
            case cursor.makeEdit cursor.indentation many.range.start desc of
                NoIdFound ->
                    -- dive further
                    case editMany makeFoundEdit push cursor many.children of
                        EditMade maybeSeed maybePush updatedChildren ->
                            EditMade maybeSeed
                                maybePush
                                (ManyOf
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

                        NoIdFound ->
                            NoIdFound

                        ErrorMakingEdit err ->
                            ErrorMakingEdit err

                otherwise ->
                    otherwise

        StartsWith details ->
            case makeEdit cursor details.first.found of
                NoIdFound ->
                    case makeEdit cursor details.second.found of
                        EditMade maybeSeed maybePush secondUpdated ->
                            EditMade maybeSeed
                                maybePush
                                (StartsWith
                                    { range = expandRange maybePush details.range
                                    , id = details.id
                                    , first = details.first
                                    , second =
                                        details.second
                                            |> (\snd ->
                                                    { snd | found = secondUpdated }
                                               )
                                    }
                                )

                        otherwise ->
                            otherwise

                EditMade maybeSeed maybePush firstUpdated ->
                    EditMade maybeSeed
                        maybePush
                        (StartsWith
                            { range = expandRange maybePush details.range
                            , id = details.id
                            , second =
                                { expected = details.second.expected
                                , found =
                                    case maybePush of
                                        Nothing ->
                                            details.second.found

                                        Just p ->
                                            pushDescription p details.second.found
                                }
                            , first =
                                details.first
                                    |> (\fst ->
                                            { fst | found = firstUpdated }
                                       )
                            }
                        )

                otherwise ->
                    otherwise

        DescribeTree details ->
            case cursor.makeEdit cursor.indentation details.range.start desc of
                NoIdFound ->
                    case editListNested cursor details.children of
                        EditMade maybeSeed maybePush newChildren ->
                            EditMade maybeSeed
                                maybePush
                                (DescribeTree
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

                        NoIdFound ->
                            NoIdFound

                        ErrorMakingEdit err ->
                            ErrorMakingEdit err

                otherwise ->
                    otherwise

        -- Primitives
        DescribeBoolean details ->
            cursor.makeEdit cursor.indentation (foundStart details.found) desc

        DescribeInteger found ->
            cursor.makeEdit cursor.indentation (foundStart found.found) desc

        DescribeFloat found ->
            cursor.makeEdit cursor.indentation (foundStart found.found) desc

        DescribeText txt ->
            cursor.makeEdit cursor.indentation (.start txt.range) desc

        DescribeString id range str ->
            cursor.makeEdit cursor.indentation range.start desc

        DescribeNothing _ ->
            NoIdFound


editListNested cursor lsNested =
    let
        indentedCursor =
            increaseIndent cursor
    in
    lsNested
        |> List.foldl
            (\foundChild ( editMade, pastChildren ) ->
                case editMade of
                    EditMade maybeSeed maybePush _ ->
                        ( editMade
                        , pushNested maybePush foundChild :: pastChildren
                        )

                    ErrorMakingEdit err ->
                        ( ErrorMakingEdit err
                        , foundChild :: pastChildren
                        )

                    NoIdFound ->
                        case editNested indentedCursor foundChild of
                            NoIdFound ->
                                ( NoIdFound
                                , foundChild :: pastChildren
                                )

                            ErrorMakingEdit err ->
                                ( ErrorMakingEdit err
                                , foundChild :: pastChildren
                                )

                            EditMade maybeSeed maybePush newChild ->
                                ( EditMade maybeSeed maybePush []
                                , newChild :: pastChildren
                                )
            )
            ( NoIdFound, [] )
        |> (\( editMade, updatedList ) ->
                case editMade of
                    EditMade maybeSeed maybePush _ ->
                        EditMade maybeSeed maybePush (List.reverse updatedList)

                    otherwise ->
                        otherwise
           )


editNested cursor (Nested nestedDetails) =
    let
        -- TODO: This code doesn't look like it's hooked up!
        _ =
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


editFields cursor fields =
    let
        makeFieldEdit (( fieldName, foundField ) as field) ( editMade, pastFields ) =
            case editMade of
                EditMade maybeSeed maybePush ls ->
                    ( EditMade maybeSeed maybePush ls
                    , ( fieldName
                      , case maybePush of
                            Nothing ->
                                foundField

                            Just to ->
                                pushFound to foundField
                      )
                        :: pastFields
                    )

                NoIdFound ->
                    case makeFoundEdit cursor foundField of
                        NoIdFound ->
                            ( NoIdFound
                            , field :: pastFields
                            )

                        ErrorMakingEdit err ->
                            ( ErrorMakingEdit err
                            , field :: pastFields
                            )

                        EditMade maybeSeed maybePush newField ->
                            ( EditMade maybeSeed maybePush []
                            , ( fieldName, newField ) :: pastFields
                            )

                ErrorMakingEdit err ->
                    ( ErrorMakingEdit err
                    , field :: pastFields
                    )
    in
    fields
        |> List.foldl makeFieldEdit ( NoIdFound, [] )
        |> (\( editMade, updatedList ) ->
                case editMade of
                    EditMade maybeSeed maybePush _ ->
                        EditMade maybeSeed maybePush (List.reverse updatedList)

                    otherwise ->
                        otherwise
           )


editMany fn pusher cursor manyItems =
    manyItems
        |> List.foldl
            (\node ( editMade, pastChildren ) ->
                case editMade of
                    EditMade maybeSeed maybePush _ ->
                        ( editMade
                        , pusher maybePush node :: pastChildren
                        )

                    ErrorMakingEdit err ->
                        ( ErrorMakingEdit err
                        , node :: pastChildren
                        )

                    NoIdFound ->
                        case fn (increaseIndent (increaseIndent cursor)) node of
                            EditMade maybeSeed maybePush newChild ->
                                ( EditMade maybeSeed maybePush []
                                , newChild :: pastChildren
                                )

                            NoIdFound ->
                                ( NoIdFound
                                , node :: pastChildren
                                )

                            ErrorMakingEdit err ->
                                ( ErrorMakingEdit err
                                , node :: pastChildren
                                )
            )
            ( NoIdFound, [] )
        |> (\( editMade, updatedList ) ->
                case editMade of
                    EditMade maybeSeed maybePush _ ->
                        EditMade maybeSeed maybePush (List.reverse updatedList)

                    otherwise ->
                        otherwise
           )


increaseIndent x =
    { x | indentation = x.indentation + 1 }


foundStart found =
    case found of
        Found rng _ ->
            rng.start

        Unexpected unexpected ->
            unexpected.range.start


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
            Nested
                { nestedDetails
                    | content =
                        List.map
                            (pushDescription to)
                            nestedDetails.content
                    , children =
                        List.map
                            (pushNested maybePush)
                            nestedDetails.children
                }


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


addNewline pos =
    { offset = pos.offset + 1
    , line = pos.line + 1
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
    ->
        { push : Push
        , updated : List (Found Description)
        , seed : Id.Seed
        }
makeInsertAt seed index indentation many expectation =
    many.children
        |> List.foldl (insertHelper seed index indentation expectation)
            { index = 0
            , seed = seed
            , position = many.range.start
            , inserted = False
            , list = []
            , push = Nothing
            }
        |> (\found ->
                if found.inserted then
                    { push =
                        Maybe.map
                            (\p ->
                                { offset = p.offset
                                , line = p.line
                                }
                            )
                            found.push
                    , updated = List.reverse found.list
                    , seed = found.seed
                    }

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
                    { push =
                        Just
                            (sizeFromRange
                                { start = found.position
                                , end = created.pos
                                }
                            )
                    , updated =
                        List.reverse
                            (Found
                                { start = newStart
                                , end = created.pos
                                }
                                created.desc
                                :: found.list
                            )
                    , seed = created.seed
                    }
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
        , seed = created.seed
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
        , seed = found.seed
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



{- EDITING TEXT -}


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


{-| The index of the rendered `String`. Let's say you have this string in your markup source.

```markup
Here is *my string*.
```

When you're rendering this, it's broken into three segments.

  - `Here is`
  - `my string` (which is bold)
  - `.`

The `Offset` is character position where those three strings are considered as one big one.

Here are some lookups:

  - `0  -> H`
  - `8  -> m`
  - `17 -> .`

We're not counting control characters from our markup source.

-}
type alias Offset =
    Int



{- TEXT EDITING -}


type Restyle
    = Restyle Styles
    | AddStyle Styles
    | RemoveStyle Styles



{- TEXT EDITING HELP -}


onlyText : TextDescription -> List Text
onlyText txt =
    case txt of
        InlineBlock details ->
            case details.kind of
                EmptyAnnotation ->
                    []

                SelectText ts ->
                    ts

                SelectString str ->
                    [ Text emptyStyles str ]

        Styled _ t ->
            [ t ]


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

        ( InlineBlock one, InlineBlock two ) ->
            let
                matchingAttributes foundOne foundTwo =
                    case ( foundOne, foundTwo ) of
                        ( Found _ attr1, Found _ attr2 ) ->
                            List.map Tuple.first attr1
                                == List.map Tuple.first attr2

                        _ ->
                            False

                mergeMatchingRecords r1 r2 newKind =
                    case ( r1, r2 ) of
                        ( Record rec1, Record rec2 ) ->
                            if
                                rec1.name
                                    == rec2.name
                                    && matchingAttributes rec1.found rec2.found
                            then
                                Just
                                    (InlineBlock
                                        { kind = newKind
                                        , range = mergeRanges one.range two.range
                                        , record = one.record
                                        }
                                    )

                            else
                                Nothing

                        _ ->
                            Nothing
            in
            -- Same == same type, same attribute list, same attribute values
            case ( one.kind, two.kind ) of
                ( EmptyAnnotation, EmptyAnnotation ) ->
                    mergeMatchingRecords one.record two.record EmptyAnnotation

                ( SelectText txt1, SelectText txt2 ) ->
                    mergeMatchingRecords one.record two.record (SelectText (txt1 ++ txt2))

                ( SelectString str1, SelectString str2 ) ->
                    mergeMatchingRecords one.record two.record (SelectString (str1 ++ str2))

                _ ->
                    Nothing

        _ ->
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
    Offset
    -> Offset
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
doTextEdit anchor focus editFn current cursor =
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

        InlineBlock details ->
            case details.kind of
                SelectText txts ->
                    InlineBlock
                        { details
                            | kind = SelectText (List.map (applyStylesToText styling) txts)
                        }

                x ->
                    inlineEl


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


{-| Splits the current element based on an index.

This function should only be called when the offset is definitely contained within the element provided, not on the edges.

_Reminder_ Indexes are based on the size of the rendered text.

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

        InlineBlock details ->
            case details.kind of
                EmptyAnnotation ->
                    -- This shoudn't happen because we're expecting the offset
                    -- to be within the range, and a token has a length of 0
                    ( Styled emptyRange (Text emptyStyles "")
                    , InlineBlock details
                    )

                SelectString str ->
                    let
                        ( leftRange, rightRange ) =
                            splitRange offset details.range

                        leftString =
                            String.slice 0 offset str

                        rightString =
                            String.slice offset -1 str
                    in
                    ( InlineBlock
                        { details
                            | range = leftRange
                            , kind = SelectString leftString
                        }
                    , InlineBlock
                        { details
                            | range = rightRange
                            , kind = SelectString rightString
                        }
                    )

                SelectText txts ->
                    let
                        { left, right } =
                            List.foldl (splitTextElements offset)
                                { offset = 0
                                , left = []
                                , right = []
                                }
                                txts

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
                    ( InlineBlock
                        { details
                            | range = leftRange
                            , kind = SelectText (List.reverse left)
                        }
                    , InlineBlock
                        { details
                            | range = rightRange
                            , kind = SelectText (List.reverse right)
                        }
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
