module Mark.Edit exposing
    ( update
    , Edit, updateInt, updateString, replaceWith, delete, insertAt, updateFloat, move
    , setInt, setString, setFloat, setBool, setField, withinBlock
    )

{-|


# Making Edits

@docs update

@docs Edit, updateInt, updateString, replaceWith, delete, insertAt, updateFloat, move


# Creating Values

@docs setInt, setString, setFloat, setBool, setField, withinBlock

-}

import Iso8601
import Mark.Format as Format
import Mark.Internal.Description exposing (..)
import Mark.Internal.Error as Error
import Mark.Internal.Id as Id exposing (..)
import Time


type alias Error =
    { message : List Format.Text
    , region : { start : Position, end : Position }
    , title : String
    }


{-| -}
type alias UnexpectedDetails =
    { range : Range
    , problem : Error.Error
    }


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
    = Proved (Id ManyOptions) (List (Found Description))


{-| -}
updateInt : Id Int -> Int -> Edit
updateInt =
    UpdateInt


{-| -}
updateString : Id String -> String -> Edit
updateString =
    UpdateString


{-| -}
replaceWith : Choice (Id Options) Expectation -> Edit
replaceWith =
    ReplaceOneOf


{-| -}
delete : Id ManyOptions -> Int -> Edit
delete =
    Delete


{-| -}
insertAt : Int -> Choice (Id ManyOptions) Expectation -> Edit
insertAt =
    InsertAt


{-| -}
updateFloat : Id Float -> Float -> Edit
updateFloat =
    UpdateFloat


{-| -}
move :
    { targetIndex : Int
    , payload : Proved
    }
    -> Edit
move =
    Move


{-| -}
type Edit
    = UpdateFloat (Id Float) Float
    | UpdateString (Id String) String
    | UpdateDate (Id Time.Posix) Time.Posix
    | UpdateBool (Id Bool) Bool
    | UpdateInt (Id Int) Int
    | ReplaceOneOf (Choice (Id Options) Expectation)
      -- For singular movement, Choice has to be an existing Description
      --, not an Expectation -> Description
    | Move
        { targetIndex : Int

        -- Can we make is so the payload is proved to be valid ?
        , payload : Proved
        }
      -- Create an element in a ManyOf
      -- Indexes overflow, so if it's too large, it just puts it at the end.
      -- Indexes that are below 0 and clamped to 0
    | InsertAt Int (Choice (Id ManyOptions) Expectation)
    | Delete (Id ManyOptions) Int


{-| -}
prove : List (Found Description) -> List (Choice (Id ManyOptions) Expectation) -> Maybe Proved
prove found choices =
    let
        combineChoices (Choice id exp) ( lastId, foundExpectations, matchingIds ) =
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


choiceExpectation (Choice id exp) =
    exp


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

        DescribeStub name found ->
            case exp of
                ExpectStub expectedName ->
                    name == expectedName

                _ ->
                    False

        Record details ->
            matchExpected details.expected exp

        OneOf one ->
            matchExpected (ExpectOneOf (List.map choiceExpectation one.choices)) exp

        ManyOf many ->
            matchExpected (ExpectManyOf (List.map choiceExpectation many.choices)) exp

        StartsWith range start end ->
            case exp of
                ExpectStartsWith startExp endExp ->
                    match start.found startExp && match end.found endExp

                _ ->
                    False

        DescribeTree tree ->
            matchExpected tree.expected exp

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

        DescribeFloatBetween _ ->
            case exp of
                ExpectFloatBetween _ ->
                    True

                _ ->
                    False

        DescribeIntBetween _ ->
            case exp of
                ExpectIntBetween _ ->
                    True

                _ ->
                    False

        DescribeText _ ->
            case exp of
                ExpectText _ ->
                    True

                _ ->
                    False

        DescribeString _ _ ->
            case exp of
                ExpectString _ ->
                    True

                _ ->
                    False

        DescribeMultiline _ _ ->
            case exp of
                ExpectMultiline _ ->
                    True

                _ ->
                    False

        DescribeStringExactly _ _ ->
            case exp of
                ExpectStringExactly _ ->
                    True

                _ ->
                    False

        DescribeDate foundPosix ->
            case exp of
                ExpectDate _ ->
                    True

                _ ->
                    False


{-| Is the first expectation a subset of the second?
-}
matchExpected : Expectation -> Expectation -> Bool
matchExpected subExp expected =
    case ( subExp, expected ) of
        ( ExpectBlock oneName oneExp, ExpectBlock twoName twoExp ) ->
            oneName == twoName && matchExpected oneExp twoExp

        ( ExpectStub one, ExpectStub two ) ->
            one == two

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

        ( ExpectFloatBetween oneDetails, ExpectFloatBetween twoDetails ) ->
            oneDetails.max == twoDetails.max && oneDetails.min == twoDetails.min

        ( ExpectIntBetween oneDetails, ExpectIntBetween twoDetails ) ->
            oneDetails.max == twoDetails.max && oneDetails.min == twoDetails.min

        ( ExpectText oneInline, ExpectText twoInline ) ->
            True

        ( ExpectString _, ExpectString _ ) ->
            True

        ( ExpectMultiline _, ExpectMultiline _ ) ->
            True

        ( ExpectStringExactly oneName, ExpectStringExactly twoName ) ->
            oneName == twoName

        ( ExpectDate _, ExpectDate _ ) ->
            True

        ( ExpectTree oneIcon oneContent, ExpectTree twoIcon twoContent ) ->
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


{-| -}
update : Edit -> Parsed -> Parsed
update edit (Parsed original) =
    case edit of
        UpdateDate id newDate ->
            Parsed
                { original
                    | found =
                        makeFoundEdit
                            { targetRange = getRange id
                            , makeEdit = \i pos desc -> updateFoundDate id newDate desc
                            , indentation = 0
                            }
                            original.found
                }

        UpdateBool id newBool ->
            Parsed
                { original
                    | found =
                        makeFoundEdit
                            { targetRange = getRange id
                            , makeEdit = \i pos desc -> updateFoundBool id newBool desc
                            , indentation = 0
                            }
                            original.found
                }

        UpdateFloat id newFloat ->
            Parsed
                { original
                    | found =
                        makeFoundEdit
                            { targetRange = getRange id
                            , makeEdit = \i pos desc -> updateFoundFloat id newFloat desc
                            , indentation = 0
                            }
                            original.found
                }

        UpdateString id newStr ->
            Parsed
                { original
                    | found =
                        makeFoundEdit
                            { targetRange = getRange id
                            , makeEdit = \i pos desc -> updateFoundString id newStr desc
                            , indentation = 0
                            }
                            original.found
                }

        UpdateInt id newInt ->
            Parsed
                { original
                    | found =
                        makeFoundEdit
                            { targetRange = getRange id
                            , makeEdit = \i pos desc -> updateFoundInt id newInt desc
                            , indentation = 0
                            }
                            original.found
                }

        ReplaceOneOf (Choice id expectation) ->
            Parsed
                { original
                    | found =
                        makeFoundEdit
                            { targetRange = getRange id
                            , makeEdit =
                                \i pos desc ->
                                    let
                                        new =
                                            create
                                                { indent = i
                                                , base = pos
                                                , expectation = expectation
                                                , seed = original.currentSeed
                                                }
                                    in
                                    replaceOption id new.desc desc
                            , indentation = 0
                            }
                            original.found
                }

        InsertAt index (Choice id expectation) ->
            Parsed
                { original
                    | found =
                        makeFoundEdit
                            { targetRange = getRange id
                            , makeEdit =
                                \indentation pos desc ->
                                    case desc of
                                        ManyOf many ->
                                            if id == many.id then
                                                Just
                                                    (ManyOf
                                                        { many
                                                            | children = makeInsertAt original.currentSeed index indentation many expectation
                                                        }
                                                    )

                                            else
                                                Nothing

                                        _ ->
                                            Nothing
                            , indentation = 0
                            }
                            original.found
                }

        Delete id index ->
            Parsed
                { original
                    | found =
                        makeFoundEdit
                            { targetRange = getRange id
                            , makeEdit = \i pos desc -> makeDeleteBlock id index desc
                            , indentation = 0
                            }
                            original.found
                }

        Move data ->
            Parsed original


type alias EditCursor =
    -- An edit takes the indentation level
    -- , the last reference position
    -- and the current description
    { makeEdit : Int -> Position -> Description -> Maybe Description
    , indentation : Int
    , targetRange : Range
    }


makeFoundEdit : EditCursor -> Found Description -> Found Description
makeFoundEdit cursor foundDesc =
    case foundDesc of
        Found range desc ->
            if within cursor.targetRange range then
                case cursor.makeEdit cursor.indentation range.start desc of
                    Nothing ->
                        Found range (makeEdit cursor desc)

                    Just newDesc ->
                        Found range newDesc

            else
                foundDesc

        Unexpected unexpected ->
            foundDesc


increaseIndent x =
    { x | indentation = x.indentation + 1 }


{-| -}
makeEdit : EditCursor -> Description -> Description
makeEdit cursor desc =
    case desc of
        DescribeBlock details ->
            case cursor.makeEdit cursor.indentation (foundStart details.found) desc of
                Just newDesc ->
                    -- replace current description
                    newDesc

                Nothing ->
                    -- dive further
                    case details.found of
                        Found rng child ->
                            DescribeBlock
                                { details
                                    | found = Found rng (makeEdit (increaseIndent cursor) child)
                                }

                        Unexpected unexpected ->
                            desc

        Record details ->
            case cursor.makeEdit (cursor.indentation + 1) (foundStart details.found) desc of
                Just newDesc ->
                    newDesc

                Nothing ->
                    case details.found of
                        Found rng fields ->
                            if within cursor.targetRange rng then
                                Record
                                    { details
                                        | found =
                                            Found rng
                                                (List.map
                                                    (Tuple.mapSecond
                                                        (makeFoundEdit (increaseIndent (increaseIndent cursor)))
                                                    )
                                                    fields
                                                )
                                    }

                            else
                                desc

                        Unexpected unexpected ->
                            desc

        OneOf one ->
            case cursor.makeEdit cursor.indentation (foundStart one.child) desc of
                Just newDesc ->
                    -- replace current description
                    newDesc

                Nothing ->
                    -- dive further
                    case one.child of
                        Found rng child ->
                            OneOf
                                { one
                                    | child =
                                        Found rng (makeEdit cursor child)
                                }

                        Unexpected unexpected ->
                            desc

        ManyOf many ->
            if within cursor.targetRange (getRange many.id) then
                case cursor.makeEdit cursor.indentation (.start (getRange many.id)) desc of
                    Just newDesc ->
                        -- replace current description
                        newDesc

                    Nothing ->
                        -- dive further
                        ManyOf
                            { many
                                | children = List.map (makeFoundEdit cursor) many.children
                            }

            else
                desc

        StartsWith range fst snd ->
            -- if id is within range
            if within cursor.targetRange range then
                StartsWith range
                    { fst | found = makeEdit cursor fst.found }
                    { snd | found = makeEdit cursor snd.found }

            else
                desc

        DescribeTree details ->
            -- TODO
            desc

        -- Primitives
        DescribeStub name found ->
            replacePrimitive cursor (foundStart found) desc

        DescribeBoolean details ->
            replacePrimitive cursor (foundStart details.found) desc

        DescribeInteger found ->
            replacePrimitive cursor (foundStart found.found) desc

        DescribeFloat found ->
            replacePrimitive cursor (foundStart found.found) desc

        DescribeFloatBetween details ->
            replacePrimitive cursor (foundStart details.found) desc

        DescribeIntBetween details ->
            replacePrimitive cursor (foundStart details.found) desc

        DescribeText txt ->
            replacePrimitive cursor (.start (getRange txt.id)) desc

        DescribeString id str ->
            replacePrimitive cursor (.start (getRange id)) desc

        DescribeMultiline id str ->
            replacePrimitive cursor (.start (getRange id)) desc

        DescribeStringExactly rng str ->
            replacePrimitive cursor rng.start desc

        DescribeDate details ->
            replacePrimitive cursor (foundStart details.found) desc


foundStart found =
    case found of
        Found rng _ ->
            rng.start

        Unexpected unexpected ->
            unexpected.range.start


replacePrimitive cursor startingPos desc =
    case cursor.makeEdit cursor.indentation startingPos desc of
        Just newDesc ->
            newDesc

        Nothing ->
            desc


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

       For ReplaceOneOf

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
make : Expectation -> List (Choice id Expectation) -> Maybe (Choice id Expectation)
make expected options =
    List.filterMap
        (\(Choice id exp) ->
            if matchExpected expected exp then
                Just (Choice id expected)

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
    List.foldl
        (\item ( i, found ) ->
            if i == index then
                ( i + 1, found )

            else
                ( i + 1, item :: found )
        )
        ( 0, [] )
        list
        |> Tuple.second
        |> List.reverse


{-| -}
startDocRange : Range
startDocRange =
    { start =
        startingPoint
    , end =
        startingPoint
    }


makeDeleteBlock id index desc =
    case desc of
        ManyOf many ->
            if id == many.id then
                Just
                    (ManyOf
                        { many
                            | children = removeByIndex index many.children
                        }
                    )

            else
                Nothing

        _ ->
            Nothing


push maybePush found =
    case maybePush of
        Nothing ->
            found

        Just to ->
            case found of
                Found range item ->
                    Found (pushRange to range) (pushDescription to item)

                Unexpected unexpected ->
                    Unexpected { unexpected | range = pushRange to unexpected.range }


pushDescription to desc =
    case desc of
        _ ->
            desc



-- pushId to (Id range) =
--     Id (pushRange to range)


pushRange to range =
    { start = addPositions to range.start
    , end = addPositions to range.end
    }


addPositions to pos =
    { offset = pos.offset + to.offset
    , line = pos.line + to.line
    , column = pos.column + to.column
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


{-| TODO: return coordinate adjustment
-}
makeInsertAt seed index indentation many expectation =
    List.foldl
        (\item found ->
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

                    -- ( createdEnd, new ) =
                    created =
                        create
                            { indent = indentation
                            , base = newStart
                            , expectation = expectation
                            , seed = seed
                            }

                    newDescSize =
                        minusPosition created.pos found.position

                    newFound =
                        Found
                            { start = newStart
                            , end = created.pos
                            }
                            created.desc

                    pushAmount =
                        Just <|
                            if index == 0 then
                                { offset = newDescSize.offset + 2
                                , line = newDescSize.line + 2
                                , column = 0
                                }

                            else
                                { offset = newDescSize.offset
                                , line = newDescSize.line
                                , column = 0
                                }

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
        )
        { index = 0
        , position = .start (getRange many.id)
        , inserted = False
        , list = []
        , push = Nothing
        }
        many.children
        |> (\found ->
                if found.inserted then
                    found.list

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
                    Found
                        { start = newStart
                        , end = created.pos
                        }
                        created.desc
                        :: found.list
           )
        |> List.reverse


getFoundRange found =
    case found of
        Found rng _ ->
            rng

        Unexpected unexp ->
            unexp.range


replaceOption id new desc =
    case desc of
        OneOf one ->
            if id == one.id then
                case one.child of
                    Found range val ->
                        Just (OneOf { one | child = Found range new })

                    Unexpected unexpected ->
                        Just (OneOf { one | child = Found unexpected.range new })

            else
                Nothing

        _ ->
            Nothing


updateFoundDate id newDate desc =
    case desc of
        DescribeDate details ->
            if details.id == id then
                case details.found of
                    Found dateRng fl ->
                        Just
                            (DescribeDate
                                { id = details.id
                                , found =
                                    Found dateRng
                                        ( Iso8601.fromTime newDate, newDate )
                                }
                            )

                    Unexpected unexpected ->
                        Just
                            (DescribeDate
                                { id = details.id
                                , found =
                                    Found unexpected.range
                                        ( Iso8601.fromTime newDate, newDate )
                                }
                            )

            else
                Nothing

        _ ->
            Nothing


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
        DescribeFloatBetween details ->
            if details.id == id then
                case details.found of
                    Found intRng fl ->
                        if newFloat >= details.min && newFloat <= details.max then
                            Just
                                (DescribeFloatBetween
                                    { details
                                        | found =
                                            Found intRng
                                                ( String.fromFloat newFloat, newFloat )
                                    }
                                )

                        else
                            Just
                                (DescribeFloatBetween
                                    { details
                                        | found =
                                            Unexpected
                                                { range = intRng
                                                , problem =
                                                    Error.FloatOutOfRange
                                                        { found = newFloat
                                                        , min = details.min
                                                        , max = details.max
                                                        }
                                                }
                                    }
                                )

                    Unexpected unexpected ->
                        if newFloat >= details.min && newFloat <= details.max then
                            Just
                                (DescribeFloatBetween
                                    { details
                                        | found =
                                            Found unexpected.range
                                                ( String.fromFloat newFloat, newFloat )
                                    }
                                )

                        else
                            Just
                                (DescribeFloatBetween
                                    { details
                                        | found =
                                            Unexpected
                                                { range = unexpected.range
                                                , problem =
                                                    Error.FloatOutOfRange
                                                        { found = newFloat
                                                        , min = details.min
                                                        , max = details.max
                                                        }
                                                }
                                    }
                                )

            else
                Nothing

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
        DescribeString range _ ->
            if range == id then
                Just
                    (DescribeString range
                        newString
                    )

            else
                Nothing

        DescribeMultiline range _ ->
            if range == id then
                Just
                    (DescribeMultiline range
                        newString
                    )

            else
                Nothing

        _ ->
            Nothing


updateFoundInt id newInt desc =
    case desc of
        DescribeIntBetween details ->
            if details.id == id then
                case details.found of
                    Found intRng fl ->
                        if newInt >= details.min && newInt <= details.max then
                            Just
                                (DescribeIntBetween
                                    { details
                                        | found =
                                            Found intRng
                                                newInt
                                    }
                                )

                        else
                            Just
                                (DescribeIntBetween
                                    { details
                                        | found =
                                            Unexpected
                                                { range = intRng
                                                , problem =
                                                    Error.IntOutOfRange
                                                        { found = newInt
                                                        , min = details.min
                                                        , max = details.max
                                                        }
                                                }
                                    }
                                )

                    Unexpected unexpected ->
                        if newInt >= details.min && newInt <= details.max then
                            Just
                                (DescribeIntBetween
                                    { details
                                        | found =
                                            Found unexpected.range
                                                newInt
                                    }
                                )

                        else
                            Just
                                (DescribeIntBetween
                                    { details
                                        | found =
                                            Unexpected
                                                { range = unexpected.range
                                                , problem =
                                                    Error.IntOutOfRange
                                                        { found = newInt
                                                        , min = details.min
                                                        , max = details.max
                                                        }
                                                }
                                    }
                                )

            else
                Nothing

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

        StartsWith _ fst snd ->
            False

        DescribeTree details ->
            False

        -- Primitives
        DescribeStub name found ->
            True

        DescribeBoolean found ->
            True

        DescribeInteger found ->
            True

        DescribeFloat found ->
            True

        DescribeFloatBetween _ ->
            True

        DescribeIntBetween _ ->
            True

        DescribeText _ ->
            True

        DescribeString rng str ->
            True

        DescribeMultiline rng str ->
            True

        DescribeStringExactly rng str ->
            True

        DescribeDate found ->
            True


{-| -}
getContainingDescriptions : Description -> { start : Int, end : Int } -> List Description
getContainingDescriptions description offset =
    case description of
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

        StartsWith range fst snd ->
            if withinOffsetRange offset range then
                getContainingDescriptions fst.found offset ++ getContainingDescriptions snd.found offset

            else
                []

        DescribeTree details ->
            case details.found of
                ( range, items ) ->
                    if withinOffsetRange offset range then
                        List.concatMap (getWithinNested offset) items

                    else
                        []

        -- Primitives
        DescribeStub name found ->
            if withinFoundLeaf offset found then
                [ description ]

            else
                []

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

        DescribeFloatBetween details ->
            if withinFoundLeaf offset details.found then
                [ description ]

            else
                []

        DescribeIntBetween details ->
            if withinFoundLeaf offset details.found then
                [ description ]

            else
                []

        DescribeText txt ->
            if withinOffsetRange offset (getRange txt.id) then
                [ description ]

            else
                []

        DescribeString id str ->
            if withinOffsetRange offset (getRange id) then
                [ description ]

            else
                []

        DescribeMultiline id str ->
            if withinOffsetRange offset (getRange id) then
                [ description ]

            else
                []

        DescribeStringExactly rng str ->
            if withinOffsetRange offset rng then
                [ description ]

            else
                []

        DescribeDate details ->
            if withinFoundLeaf offset details.found then
                [ description ]

            else
                []


getWithinNested offset (Nested nest) =
    case nest.content of
        ( desc, items ) ->
            getContainingDescriptions desc offset
                ++ List.concatMap
                    (\item ->
                        getContainingDescriptions item offset
                    )
                    items



--- Setting Expectations
-- type Expectation
--     = ExpectBlock String Expectation
--     | ExpectStub String
--     | ExpectRecord String (List ( String, Expectation ))
--     | ExpectOneOf (List Expectation)
--     | ExpectManyOf (List Expectation)
--     | ExpectStartsWith Expectation Expectation
--     | ExpectBoolean Bool
--     | ExpectInteger Int
--     | ExpectFloat Float
--     | ExpectFloatBetween
--         { min : Float
--         , max : Float
--         , default : Float
--         }
--     | ExpectIntBetween
--         { min : Int
--         , max : Int
--         , default : Int
--         }
--     | ExpectText (List InlineExpectation)
--     | ExpectString String
--     | ExpectMultiline String
--     | ExpectStringExactly String
--     | ExpectDate Time.Posix
--     | ExpectTree Expectation Expectation
{-
   General Use of Setters.


   makeCircle default =
       startingWith default
           |> setField "label" (setString "Heres my circle!")
           |> setField "x" (setInt 10)



-}


type ExpError
    = ExpError


{-| -}
startingWith : Expectation -> Result ExpError Expectation
startingWith exp =
    Ok exp


{-| -}
withinBlock :
    (Result ExpError Expectation -> Result ExpError Expectation)
    -> Result ExpError Expectation
    -> Result ExpError Expectation
withinBlock setter resExp =
    resExp


{-| -}
setInt : Int -> Result ExpError Expectation -> Result ExpError Expectation
setInt i resExp =
    case resExp of
        Err _ ->
            resExp

        Ok exp ->
            case exp of
                ExpectInteger _ ->
                    Ok (ExpectInteger i)

                ExpectIntBetween details ->
                    if i >= details.min && i <= details.max then
                        Ok (ExpectIntBetween { details | default = i })

                    else
                        Err ExpError

                _ ->
                    Err ExpError


{-| -}
setString : String -> Result ExpError Expectation -> Result ExpError Expectation
setString str resExp =
    case resExp of
        Err _ ->
            resExp

        Ok exp ->
            case exp of
                ExpectString _ ->
                    Ok (ExpectString str)

                ExpectMultiline _ ->
                    Ok (ExpectMultiline str)

                _ ->
                    Err ExpError


{-| -}
setFloat : Float -> Result ExpError Expectation -> Result ExpError Expectation
setFloat f resExp =
    case resExp of
        Err _ ->
            resExp

        Ok exp ->
            case exp of
                ExpectFloat _ ->
                    Ok (ExpectFloat f)

                ExpectFloatBetween details ->
                    if f >= details.min && f <= details.max then
                        Ok (ExpectFloatBetween { details | default = f })

                    else
                        Err ExpError

                _ ->
                    Err ExpError


{-| -}
setBool : Bool -> Result ExpError Expectation -> Result ExpError Expectation
setBool b resExp =
    case resExp of
        Err _ ->
            resExp

        Ok exp ->
            case exp of
                ExpectBoolean _ ->
                    Ok (ExpectBoolean b)

                _ ->
                    Err ExpError


setField :
    String
    -> (Result ExpError Expectation -> Result ExpError Expectation)
    -> Result ExpError Expectation
    -> Result ExpError Expectation
setField fieldName fieldSetter resExp =
    case resExp of
        Err _ ->
            resExp

        Ok exp ->
            case exp of
                ExpectRecord recordName fields ->
                    case List.foldl (setRecordField fieldName fieldSetter) (NotYet []) fields of
                        Failed err ->
                            Err err

                        NotYet _ ->
                            Err ExpError

                        UpdateMade updatedFields ->
                            Ok
                                (ExpectRecord recordName (List.reverse updatedFields))

                _ ->
                    Err ExpError


type Updated x
    = UpdateMade x
    | NotYet x
    | Failed ExpError


setRecordField :
    String
    ->
        (Result ExpError Expectation
         -> Result ExpError Expectation
        )
    -> ( String, Expectation )
    -> Updated (List ( String, Expectation ))
    -> Updated (List ( String, Expectation ))
setRecordField targetFieldName fieldSetter ( fieldName, fieldExp ) gathered =
    case gathered of
        Failed x ->
            gathered

        UpdateMade fields ->
            UpdateMade (( fieldName, fieldExp ) :: fields)

        NotYet fields ->
            if fieldName == targetFieldName then
                case fieldSetter (Ok fieldExp) of
                    Err err ->
                        Failed err

                    Ok updated ->
                        UpdateMade (( fieldName, updated ) :: fields)

            else
                NotYet (( fieldName, fieldExp ) :: fields)
