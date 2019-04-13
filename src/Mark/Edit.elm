module Mark.Edit exposing
    ( bool, int, float, string, multiline, oneOf
    , update
    , Edit, updateInt, updateFloat, updateString
    , replace, delete, insertAt
    )

{-|


# Editable Blocks

@docs bool, int, float, string, multiline, oneOf


# Making Edits

@docs update

@docs Edit, updateInt, updateFloat, updateString

@docs replace, delete, insertAt

-}

import Mark.Internal.Description exposing (..)
import Mark.Internal.Error as Error
import Mark.Internal.Format as Format
import Mark.Internal.Id as Id exposing (..)
import Mark.Internal.Outcome as Outcome
import Mark.Internal.Parser as Parse
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


{-| -}
type alias Error =
    { message : List Format.Text
    , region : { start : Position, end : Position }
    , title : String
    }


{-| -}
type alias Id kind =
    Id.Id kind


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
type Edit
    = UpdateFloat (Id Float) Float
    | UpdateString (Id String) String
    | UpdateBool (Id Bool) Bool
    | UpdateInt (Id Int) Int
    | ReplaceOneOf (Choice (Id Options) Expectation)
      -- Create an element in a ManyOf
      -- Indexes overflow, so if it's too large, it just puts it at the end.
      -- Indexes that are below 0 and clamped to 0
    | InsertAt Int (Choice (Id ManyOptions) Expectation)
    | Delete (Id ManyOptions) Int


{-| -}
updateInt : Id Int -> Int -> Edit
updateInt =
    UpdateInt


{-| -}
updateString : Id String -> String -> Edit
updateString =
    UpdateString


{-| -}
replace : Choice (Id Options) Expectation -> Edit
replace =
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
update : Edit -> Parsed -> Parsed
update edit (Parsed original) =
    case edit of
        UpdateBool id newBool ->
            Parsed
                { original
                    | found =
                        makeFoundEdit
                            { makeEdit = \i pos desc -> updateFoundBool id newBool desc
                            , indentation = 0
                            }
                            original.found
                }

        UpdateFloat id newFloat ->
            Parsed
                { original
                    | found =
                        makeFoundEdit
                            { makeEdit = \i pos desc -> updateFoundFloat id newFloat desc
                            , indentation = 0
                            }
                            original.found
                }

        UpdateString id newStr ->
            Parsed
                { original
                    | found =
                        makeFoundEdit
                            { makeEdit = \i pos desc -> updateFoundString id newStr desc
                            , indentation = 0
                            }
                            original.found
                }

        UpdateInt id newInt ->
            Parsed
                { original
                    | found =
                        makeFoundEdit
                            { makeEdit = \i pos desc -> updateFoundInt id newInt desc
                            , indentation = 0
                            }
                            original.found
                }

        ReplaceOneOf (Choice id expectation) ->
            Parsed
                { original
                    | found =
                        makeFoundEdit
                            { makeEdit =
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
                            { makeEdit =
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
                            { makeEdit = \i pos desc -> makeDeleteBlock id index desc
                            , indentation = 0
                            }
                            original.found
                }


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

        DescribeNothing ->
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
    { makeEdit : Int -> Position -> Description -> Maybe Description
    , indentation : Int
    }


makeFoundEdit : EditCursor -> Found Description -> Found Description
makeFoundEdit cursor foundDesc =
    case foundDesc of
        Found range desc ->
            case cursor.makeEdit cursor.indentation range.start desc of
                Nothing ->
                    Found range (makeEdit cursor desc)

                Just newDesc ->
                    Found range newDesc

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
            case cursor.makeEdit cursor.indentation many.range.start desc of
                Just newDesc ->
                    -- replace current description
                    newDesc

                Nothing ->
                    -- dive further
                    ManyOf
                        { many
                            | children = List.map (makeFoundEdit cursor) many.children
                        }

        StartsWith range fst snd ->
            StartsWith range
                { fst | found = makeEdit cursor fst.found }
                { snd | found = makeEdit cursor snd.found }

        DescribeTree details ->
            -- TODO
            desc

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

        DescribeNothing ->
            desc


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
        , position = many.range.start
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

        StartsWith _ fst snd ->
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

        DescribeNothing ->
            True


{-| -}
getContainingDescriptions : Description -> { start : Int, end : Int } -> List Description
getContainingDescriptions description offset =
    case description of
        DescribeNothing ->
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
int : ({ id : Id Int, range : Range } -> Int -> a) -> Block a
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
float : ({ id : Id Float, range : Range } -> ( String, Float ) -> a) -> Block a
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
string : ({ id : Id String, range : Range } -> String -> a) -> Block a
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
multiline : ({ id : Id String, range : Range } -> String -> a) -> Block a
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
bool : ({ id : Id Bool, range : Range } -> Bool -> a) -> Block a
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
    ({ id : Id Options
     , range : Range
     , options : List (Choice (Id Options) Expectation)
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
                                    { choices = List.map (Choice parentId) expectations
                                    , child = Found range found
                                    , id = parentId
                                    }

                            Err ( pos, unexpected ) ->
                                OneOf
                                    { choices = List.map (Choice parentId) expectations
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



-- {-| Many blocks that are all at the same indentation level.
-- -}
-- manyOf :
--     { view :
--         { parent : Id ManyOptions
--         , index : Int
--         , options : List (Choice (Id ManyOptions) Expectation)
--         }
--         -> a
--         -> b
--     , merge :
--         { parent : Id ManyOptions
--         }
--         -> List b
--         -> final
--     }
--     -> List (Block a)
--     -> Block (List final)
-- manyOf manyOfDetails blocks =
--     let
--         expectations =
--             List.map getBlockExpectation blocks
--     in
--     Value
--         { expect = ExpectManyOf expectations
--         , converter =
--             \desc ->
--                 Failure NoMatch
--         -- let
--         --     applyDesc description blck found =
--         --         case found of
--         --             Nothing ->
--         --                 case renderBlock blck description of
--         --                     Failure _ ->
--         --                         found
--         --                     Success rendered ->
--         --                         Just rendered
--         --                     _ ->
--         --                         found
--         --             _ ->
--         --                 found
--         --     -- getRendered : Found Description -> Result AstError (List a) -> Result AstError (List a)
--         --     getRendered id choices found ( existingResult, index ) =
--         --         case existingResult of
--         --             Err err ->
--         --                 ( Err err, index + 1 )
--         --             Ok existing ->
--         --                 case found of
--         --                     Unexpected unexpected ->
--         --                         ( uncertain unexpected
--         --                             (manyOfDetails.error
--         --                                 { parent = id
--         --                                 , options = choices
--         --                                 , index = index
--         --                                 , range = unexpected.range
--         --                                 , problem = unexpected.problem
--         --                                 }
--         --                                 :: existing
--         --                             )
--         --                         , index + 1
--         --                         )
--         --                     Found range child ->
--         --                         case List.foldl (applyDesc child) Nothing blocks of
--         --                             Nothing ->
--         --                                 ( Failure NoMatch, index + 1 )
--         --                             Just (Success result) ->
--         --                                 ( Success
--         --                                     (manyOfDetails.view
--         --                                         { parent = id
--         --                                         , options = choices
--         --                                         , index = index
--         --                                         }
--         --                                         result
--         --                                         :: existing
--         --                                     )
--         --                                 , index + 1
--         --                                 )
--         --                             Just (Almost (Recovered err result)) ->
--         --                                 ( Almost
--         --                                     (Recovered err
--         --                                         (manyOfDetails.view
--         --                                             { parent = id
--         --                                             , options = choices
--         --                                             , index = index
--         --                                             }
--         --                                             result
--         --                                             :: existing
--         --                                         )
--         --                                     )
--         --                                 , index + 1
--         --                                 )
--         --                             Just (Almost (Uncertain err)) ->
--         --                                 ( Almost (Uncertain err)
--         --                                 , index + 1
--         --                                 )
--         --                             Just (Failure err) ->
--         --                                 ( Failure err
--         --                                 , index + 1
--         --                                 )
--         -- in
--         -- case desc of
--         --     ManyOf many ->
--         --         List.foldl (getRendered many.id many.choices) ( Ok [], 0 ) many.children
--         --             |> Tuple.first
--         --             |> Result.map
--         --                 (\items ->
--         --                     Success
--         --                         (manyOfDetails.merge
--         --                             { parent = many.id
--         --                             , options = many.choices
--         --                             }
--         --                             (List.reverse items)
--         --                         )
--         --                 )
--         --     _ ->
--         --         Failure NoMatch
--         , parser =
--             \seed ->
--                 let
--                     ( parentId, newSeed ) =
--                         Id.step seed
--                     ( _, childStart ) =
--                         Id.step newSeed
--                     reseeded =
--                         Id.reseed childStart
--                 in
--                 ( reseeded
--                 , Parser.succeed
--                     (\( range, results ) ->
--                         ManyOf
--                             { choices = List.map (Choice parentId) expectations
--                             , id = parentId
--                             , range = range
--                             , children = List.map resultToFound results
--                             }
--                     )
--                     |= Parse.withRange
--                         (Parser.getIndent
--                             |> Parser.andThen
--                                 (\indentation ->
--                                     Parser.loop
--                                         { parsedSomething = False
--                                         , found = []
--                                         , seed = childStart
--                                         }
--                                         (blocksOrNewlines
--                                             indentation
--                                             blocks
--                                         )
--                                 )
--                         )
--                 )
--         }


{-| Many blocks that are all at the same indentation level.
-}
manyOf :
    ({ id : Id Options
     , range : Range
     }
     -> List a
     -> b
    )
    -> List (Block a)
    -> Block (List b)
manyOf blocks =
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
                                    (List.foldl (matchBlock child) (Outcome.Failure NoMatch) blocks)
                                    existingResult
                                , index + 1
                                )
                in
                case desc of
                    ManyOf many ->
                        List.foldl (getRendered many.id many.choices) ( Outcome.Success [], 0 ) many.children
                            |> Tuple.first
                            |> mapSuccessAndRecovered List.reverse

                    _ ->
                        Outcome.Failure NoMatch
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
                            { choices = List.map (Choice parentId) expectations
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
