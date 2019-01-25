module Mark.Advanced exposing
    ( Document, Description(..), TextDescription(..), InlineDescription(..)
    , parse, compile, convert, Outcome(..), Parsed
    , ErrorMessage, errorToString, errorToHtml, Theme(..)
    , Block, Found(..)
    , document
    , block, stub
    , string, exactly, int, float, floatBetween, intBetween, bool, date, multiline
    , oneOf, manyOf, startWith, nested
    , record2, field, Field
    , Text(..), Style(..), text, replacement, balanced, Replacement
    , Inline, inline, inlineString, inlineText, mapInline
    , map
    , focus, parent, getDesc, getDescription, toString
    , Range, Position, Nested(..), foldNested, foldNestedList, replaceNested
    , update, Edit, updateFloat, updateInt, updateString, replaceOneOf, deleteBlock, insertAt
    )

{-|

@docs Document, Description, TextDescription, InlineDescription

@docs parse, compile, convert, Outcome, Parsed

@docs ErrorMessage, errorToString, errorToHtml, Theme

@docs Block, Found

@docs document

@docs block, stub

@docs string, exactly, int, float, floatBetween, intBetween, bool, date, multiline

@docs oneOf, manyOf, startWith, nested

@docs record2, field, Field

@docs Text, Style, text, replacement, balanced, Replacement

@docs Inline, inline, inlineString, inlineText, mapInline

@docs map

@docs focus, parent, getDesc, getDescription, toString

@docs Range, Position, Nested, foldNested, foldNestedList, replaceNested

@docs update, Edit, updateFloat, updateInt, updateString, replaceOneOf, deleteBlock, insertAt

-}

import Html
import Html.Attributes
import Iso8601
import Mark.Format as Format
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)
import Time



{- INTERFACE -}


type Outcome failure almost success
    = Success success
    | Almost almost
    | Failure failure


{-| -}
parse :
    Document data
    -> String
    -> Outcome ErrorMessage (Partial Parsed) Parsed
parse (Document blocks) source =
    case Parser.run blocks.parser source of
        Ok ((Parsed parsedDetails) as parsed) ->
            case parsedDetails.errors of
                [] ->
                    Success parsed

                _ ->
                    Almost
                        { errors = parsedDetails.errors
                        , result = parsed
                        }

        Err deadEnds ->
            Failure <|
                renderError
                    source
                    { problem = MsgParsingIssue deadEnds
                    , range =
                        case List.head deadEnds of
                            Nothing ->
                                { start =
                                    { offset = 0
                                    , line = 0
                                    , column = 0
                                    }
                                , end =
                                    { offset = 0
                                    , line = 0
                                    , column = 0
                                    }
                                }

                            Just deadEnd ->
                                { start =
                                    { offset = 0
                                    , line = deadEnd.row
                                    , column = deadEnd.col
                                    }
                                , end =
                                    { offset = 0
                                    , line = deadEnd.row
                                    , column = deadEnd.col
                                    }
                                }
                    }


{-| -}
type Parsed
    = Parsed
        { errors : List ErrorMessage
        , found : Found Description
        , expected : Expectation
        , focus : Maybe Position
        }


{-| -}
type alias Partial data =
    { errors : List ErrorMessage
    , result : data
    }


{-| -}
startDocRange : Range
startDocRange =
    { start =
        { offset = 0
        , line = 0
        , column = 0
        }
    , end =
        { offset = 0
        , line = 0
        , column = 0
        }
    }


{-| -}
compile : Document data -> String -> Outcome ErrorMessage (Partial data) data
compile (Document blocks) source =
    case Parser.run blocks.parser source of
        Ok ((Parsed parsedDetails) as parsed) ->
            case parsedDetails.errors of
                [] ->
                    case blocks.converter parsed of
                        Ok rendered ->
                            Success rendered

                        Err noMatch ->
                            -- Invalid Ast.
                            -- This should never happen because
                            -- we definitely have the same document in both parsing and converting.
                            Failure
                                (renderError source
                                    { problem = MsgDocumentMismatch
                                    , range = startDocRange
                                    }
                                )

                _ ->
                    case blocks.converter parsed of
                        Ok rendered ->
                            Almost
                                { errors = parsedDetails.errors
                                , result = rendered
                                }

                        Err noMatch ->
                            -- Invalid Ast.
                            -- This should never happen because
                            -- we definitely have the same document in both parsing and converting.
                            Failure
                                (renderError source
                                    { problem = MsgDocumentMismatch
                                    , range = startDocRange
                                    }
                                )

        Err deadEnds ->
            Failure <|
                renderError
                    source
                    { problem = MsgParsingIssue deadEnds
                    , range =
                        case List.head deadEnds of
                            Nothing ->
                                startDocRange

                            Just deadEnd ->
                                { start =
                                    { offset = 0
                                    , line = deadEnd.row
                                    , column = deadEnd.col
                                    }
                                , end =
                                    { offset = 0
                                    , line = deadEnd.row
                                    , column = deadEnd.col
                                    }
                                }
                    }


{-| -}
convert : Document data -> Parsed -> Outcome ErrorMessage (Partial data) data
convert (Document blocks) ((Parsed parsedDetails) as parsed) =
    case parsedDetails.errors of
        [] ->
            case blocks.converter parsed of
                Ok rendered ->
                    Success rendered

                Err noMatch ->
                    Failure
                        (renderError ""
                            { problem = MsgDocumentMismatch
                            , range = startDocRange
                            }
                        )

        _ ->
            case blocks.converter parsed of
                Ok rendered ->
                    Almost
                        { errors = parsedDetails.errors
                        , result = rendered
                        }

                Err noMatch ->
                    Failure
                        (renderError ""
                            { problem = MsgDocumentMismatch
                            , range = startDocRange
                            }
                        )


type Id category
    = Id Range


type ManyOptions
    = ManyOptions


type Options
    = Options



{- All the above ids are opaque, so we know they can't be spoofed.

   The editing commands all require one of these opaque values to be constructed.

   An id captures:

       1. The coordinates of a specific point
       2. What operations can be performed at that point
       3. A valid payload




-}
-- {-| -}
-- type Edit
--     = UpdateFloat (Id Float) Float
--     | UpdateString (Id String) String
--     | UpdateInt (Id Int) Int
--     | ReplaceOneOf (Id Options) Description
--     | InsertAt (Id ManyOptions) Int Description
--     | DeleteBlock (Id ManyOptions) Int


updateInt =
    UpdateInt


updateString =
    UpdateString


replaceOneOf =
    ReplaceOneOf


deleteBlock =
    DeleteBlock


insertAt =
    InsertAt


updateFloat =
    UpdateFloat


{-| -}
type Edit
    = UpdateFloat Range Float
    | UpdateString Range String
    | UpdateInt Range Int
    | ReplaceOneOf Range Description
    | InsertAt Range Int Description
    | DeleteBlock Range Int



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


{-| -}
update : Edit -> Parsed -> Parsed
update edit (Parsed original) =
    case edit of
        UpdateFloat id newFloat ->
            Parsed
                { original
                    | found =
                        makeFoundEdit
                            id
                            (updateFoundFloat id newFloat)
                            original.found
                }

        UpdateString id newStr ->
            Parsed
                { original
                    | found =
                        makeFoundEdit
                            id
                            (updateFoundString id newStr)
                            original.found
                }

        UpdateInt id newInt ->
            Parsed
                { original
                    | found =
                        makeFoundEdit
                            id
                            (updateFoundInt id newInt)
                            original.found
                }

        ReplaceOneOf id desc ->
            Parsed
                { original
                    | found =
                        makeFoundEdit
                            id
                            (replaceOption id desc)
                            original.found
                }

        InsertAt id index desc ->
            Parsed
                { original
                    | found =
                        makeFoundEdit
                            id
                            (insertAtIndex id index desc)
                            original.found
                }

        DeleteBlock id index ->
            Parsed
                { original
                    | found =
                        makeFoundEdit
                            id
                            (makeDeleteBlock id index)
                            original.found
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



-- TODO: return coordinate adjustment


makeInsertAt index new list =
    List.foldl
        (\item found ->
            if found.index == index then
                { index = found.index + 1
                , inserted = True
                , list = item :: Found startDocRange new :: found.list
                }

            else
                { index = found.index + 1
                , inserted = found.inserted
                , list = item :: found.list
                }
        )
        { index = 0
        , inserted = False
        , list = []
        }
        list
        |> (\found ->
                if found.inserted then
                    found.list

                else
                    Found startDocRange new :: found.list
           )
        |> List.reverse


makeDeleteBlock id index desc =
    case desc of
        ManyOf expectations range found ->
            if id == range then
                Just (ManyOf expectations range (removeByIndex index found))

            else
                Nothing

        _ ->
            Nothing


insertAtIndex id index new desc =
    case desc of
        ManyOf expectations range found ->
            if id == range then
                Just (ManyOf expectations range (makeInsertAt index new found))

            else
                Nothing

        _ ->
            Nothing


replaceOption id new desc =
    case desc of
        OneOf expectations found ->
            case found of
                Found range val ->
                    if id == range then
                        Just (OneOf expectations (Found range new))

                    else
                        Nothing

                Unexpected unexpected ->
                    Nothing

        _ ->
            Nothing


updateFoundFloat id newFloat desc =
    case desc of
        DescribeFloatBetween bottom top found ->
            case found of
                Found floatRng fl ->
                    if floatRng == id then
                        if newFloat >= bottom && newFloat <= top then
                            Just
                                (DescribeFloatBetween
                                    bottom
                                    top
                                    (Found floatRng
                                        ( String.fromFloat newFloat, newFloat )
                                    )
                                )

                        else
                            Just
                                (DescribeFloatBetween
                                    bottom
                                    top
                                    (Unexpected
                                        { range = floatRng
                                        , problem =
                                            MsgFloatOutOfRange
                                                { found = newFloat
                                                , min = bottom
                                                , max = top
                                                }
                                        }
                                    )
                                )

                    else
                        Nothing

                Unexpected unexpected ->
                    if unexpected.range == id then
                        if newFloat >= bottom && newFloat <= top then
                            Just
                                (DescribeFloatBetween
                                    bottom
                                    top
                                    (Found unexpected.range
                                        ( String.fromFloat newFloat, newFloat )
                                    )
                                )

                        else
                            Just
                                (DescribeFloatBetween
                                    bottom
                                    top
                                    (Unexpected
                                        { range = unexpected.range
                                        , problem =
                                            MsgFloatOutOfRange
                                                { found = newFloat
                                                , min = bottom
                                                , max = top
                                                }
                                        }
                                    )
                                )

                    else
                        Nothing

        DescribeFloat found ->
            case found of
                Found floatRng fl ->
                    if floatRng == id then
                        Just
                            (DescribeFloat
                                (Found floatRng
                                    ( String.fromFloat newFloat, newFloat )
                                )
                            )

                    else
                        Nothing

                Unexpected unexpected ->
                    if unexpected.range == id then
                        Just
                            (DescribeFloat
                                (Found unexpected.range
                                    ( String.fromFloat newFloat, newFloat )
                                )
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
        DescribeIntBetween bottom top found ->
            case found of
                Found floatRng fl ->
                    if floatRng == id then
                        if newInt >= bottom && newInt <= top then
                            Just
                                (DescribeIntBetween
                                    bottom
                                    top
                                    (Found floatRng
                                        newInt
                                    )
                                )

                        else
                            Just
                                (DescribeIntBetween
                                    bottom
                                    top
                                    (Unexpected
                                        { range = floatRng
                                        , problem =
                                            MsgIntOutOfRange
                                                { found = newInt
                                                , min = bottom
                                                , max = top
                                                }
                                        }
                                    )
                                )

                    else
                        Nothing

                Unexpected unexpected ->
                    if unexpected.range == id then
                        if newInt >= bottom && newInt <= top then
                            Just
                                (DescribeIntBetween
                                    bottom
                                    top
                                    (Found unexpected.range
                                        newInt
                                    )
                                )

                        else
                            Just
                                (DescribeIntBetween
                                    bottom
                                    top
                                    (Unexpected
                                        { range = unexpected.range
                                        , problem =
                                            MsgIntOutOfRange
                                                { found = newInt
                                                , min = bottom
                                                , max = top
                                                }
                                        }
                                    )
                                )

                    else
                        Nothing

        DescribeInteger found ->
            case found of
                Found floatRng fl ->
                    if floatRng == id then
                        Just
                            (DescribeInteger
                                (Found floatRng
                                    newInt
                                )
                            )

                    else
                        Nothing

                Unexpected unexpected ->
                    if unexpected.range == id then
                        Just
                            (DescribeInteger
                                (Found unexpected.range
                                    newInt
                                )
                            )

                    else
                        Nothing

        _ ->
            Nothing


replacePrimitive fn desc =
    case fn desc of
        Just newDesc ->
            newDesc

        Nothing ->
            desc


makeFoundEdit id fn foundDesc =
    case foundDesc of
        Found range desc ->
            if within id range then
                case fn desc of
                    Nothing ->
                        Found range (makeEdit id fn desc)

                    Just newDesc ->
                        Found range newDesc

            else
                foundDesc

        Unexpected unexpected ->
            foundDesc


makeEdit : Range -> (Description -> Maybe Description) -> Description -> Description
makeEdit id fn desc =
    case desc of
        DescribeBlock name details ->
            case fn desc of
                Just newDesc ->
                    -- replace current description
                    newDesc

                Nothing ->
                    -- dive further
                    case details.found of
                        Found rng child ->
                            DescribeBlock name
                                { details
                                    | found = Found rng (makeEdit id fn child)
                                }

                        Unexpected unexpected ->
                            desc

        Record name details ->
            case fn desc of
                Just newDesc ->
                    newDesc

                Nothing ->
                    case details.found of
                        Found rng fields ->
                            if within id rng then
                                Record name
                                    { details
                                        | found =
                                            Found rng
                                                (List.map (Tuple.mapSecond (makeFoundEdit id fn)) fields)
                                    }

                            else
                                desc

                        Unexpected unexpected ->
                            desc

        OneOf expected found ->
            case fn desc of
                Just newDesc ->
                    -- replace current description
                    newDesc

                Nothing ->
                    -- dive further
                    case found of
                        Found rng child ->
                            OneOf expected
                                (Found rng (makeEdit id fn child))

                        Unexpected unexpected ->
                            desc

        ManyOf expected range foundList ->
            if within id range then
                ManyOf expected
                    range
                    (List.map (makeFoundEdit id fn) foundList)

            else
                desc

        StartsWith range fst snd ->
            -- if id is within range
            if within id range then
                -- TODO
                desc

            else
                desc

        DescribeTree details ->
            desc

        -- Primitives
        DescribeStub name found ->
            replacePrimitive fn desc

        DescribeBoolean found ->
            replacePrimitive fn desc

        DescribeInteger found ->
            replacePrimitive fn desc

        DescribeFloat found ->
            replacePrimitive fn desc

        DescribeFloatBetween top bottom found ->
            replacePrimitive fn desc

        DescribeIntBetween top bottom found ->
            replacePrimitive fn desc

        DescribeText rng textNodes ->
            replacePrimitive fn desc

        DescribeString rng str ->
            replacePrimitive fn desc

        DescribeMultiline rng str ->
            replacePrimitive fn desc

        DescribeStringExactly rng str ->
            replacePrimitive fn desc

        DescribeDate found ->
            replacePrimitive fn desc


{-| -}
type Document data
    = Document
        { converter : Parsed -> Result AstError data
        , expect : Expectation
        , parser : Parser Context Problem Parsed
        }


{-| A `Block data` is just a parser that results in `data`.

You'll be building up your `Document` in terms of the `Blocks`.

A block starts with `|` and has a name(already built into the parser)

A value is just a raw parser.

-}
type Block data
    = Block
        String
        { converter : Description -> Result AstError (Found data)
        , expect : Expectation
        , parser : Parser Context Problem Description
        }
    | Value
        { converter : Description -> Result AstError (Found data)
        , expect : Expectation
        , parser : Parser Context Problem Description
        }


{-| -}
type Found item
    = Found Range item
    | Unexpected
        { range : Range
        , problem : ProblemMessage
        }


{-| -}
type alias UnexpectedDetails =
    { range : Range
    , problem : ProblemMessage
    }


getParser : Block data -> Parser Context Problem Description
getParser fromBlock =
    case fromBlock of
        Block name { parser } ->
            Parser.succeed identity
                |. Parser.token (Parser.Token "|" (ExpectingBlockName name))
                |. Parser.chompIf (\c -> c == ' ') Space
                |= parser

        Value { parser } ->
            parser


renderBlock : Block data -> Description -> Result AstError (Found data)
renderBlock fromBlock =
    case fromBlock of
        Block name { converter } ->
            converter

        Value { converter } ->
            converter


getBlockExpectation fromBlock =
    case fromBlock of
        Block name { expect } ->
            expect

        Value { expect } ->
            expect


type AstError
    = InvalidAst
    | NoMatch


type Description
    = DescribeBlock
        String
        { found : Found Description
        , expected : Expectation
        }
    | Record
        String
        { found : Found (List ( String, Found Description ))
        , expected : Expectation
        }
    | OneOf (List Expectation) (Found Description)
    | ManyOf (List Expectation) Range (List (Found Description))
    | StartsWith
        Range
        { found : Description
        , expected : Expectation
        }
        { found : Description
        , expected : Expectation
        }
    | DescribeTree
        { found : ( Range, List (Nested ( Description, List Description )) )
        , expected : Expectation
        }
      -- Primitives
    | DescribeStub String (Found String)
    | DescribeBoolean (Found Bool)
    | DescribeInteger (Found Int)
    | DescribeFloat (Found ( String, Float ))
    | DescribeFloatBetween Float Float (Found ( String, Float ))
    | DescribeIntBetween Int Int (Found Int)
    | DescribeText Range (List TextDescription)
    | DescribeString Range String
    | DescribeMultiline Range String
    | DescribeStringExactly Range String
    | DescribeDate (Found ( String, Time.Posix ))


type TextDescription
    = Styled Range Text
    | DescribeInline String Range (List InlineDescription)
    | UnexpectedInline UnexpectedDetails


type InlineDescription
    = DescribeInlineString String Range String
    | DescribeInlineText Range (List Text)


{-| A text fragment with some styling.
-}
type Text
    = Text (List Style) String


type Expectation
    = ExpectBlock String Expectation
    | ExpectStub String
    | ExpectRecord String (List ( String, Expectation ))
    | ExpectOneOf (List Expectation)
    | ExpectManyOf (List Expectation)
    | ExpectStartsWith Expectation Expectation
    | ExpectBoolean
    | ExpectInteger
    | ExpectFloat
    | ExpectFloatBetween Float Float
    | ExpectIntBetween Int Int
    | ExpectText (List InlineExpectation)
    | ExpectString
    | ExpectMultiline
    | ExpectStringExactly String
    | ExpectDate
    | ExpectTree Expectation Expectation


getInlineName (InlineExpectation name _) =
    name


type InlineExpectation
    = InlineExpectation String (List InlineValueExpectation)


type InlineValueExpectation
    = ExpectInlineString String
    | ExpectInlineText


{-| -}
focus : Position -> Parsed -> Parsed
focus pos (Parsed parsed) =
    Parsed { parsed | focus = Just pos }


{-| -}
parent : Parsed -> Parsed
parent parsed =
    -- TODO: implement
    Debug.todo "implement!"


getDescription (Parsed parsed) =
    parsed.found


{-| -}
getDesc : { start : Int, end : Int } -> Parsed -> List Description
getDesc offset (Parsed parsed) =
    getWithinFound offset parsed.found


within rangeOne rangeTwo =
    withinOffsetRange { start = rangeOne.start.offset, end = rangeOne.end.offset } rangeTwo


withinOffsetRange offset range =
    range.start.offset <= offset.start && range.end.offset >= offset.end


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
        DescribeBlock name details ->
            False

        Record name details ->
            False

        OneOf expected found ->
            False

        ManyOf expected rng foundList ->
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

        DescribeFloatBetween _ _ found ->
            True

        DescribeIntBetween _ _ found ->
            True

        DescribeText rng textNodes ->
            True

        DescribeString rng str ->
            True

        DescribeMultiline rng str ->
            True

        DescribeStringExactly rng str ->
            True

        DescribeDate found ->
            True


getUnexpecteds : Description -> List UnexpectedDetails
getUnexpecteds description =
    case description of
        DescribeBlock name details ->
            spelunkUnexpectedsFromFound details.found

        Record name details ->
            case details.found of
                Found _ fields ->
                    List.concatMap
                        (Tuple.second >> spelunkUnexpectedsFromFound)
                        fields

                Unexpected unexpected ->
                    [ unexpected ]

        OneOf expected found ->
            spelunkUnexpectedsFromFound found

        ManyOf expected rng foundList ->
            List.concatMap spelunkUnexpectedsFromFound foundList

        StartsWith _ fst snd ->
            getUnexpecteds fst.found ++ getUnexpecteds snd.found

        DescribeTree details ->
            List.concatMap getNestedUnexpecteds (Tuple.second details.found)

        -- Primitives
        DescribeStub name found ->
            unexpectedFromFound found

        DescribeBoolean found ->
            unexpectedFromFound found

        DescribeInteger found ->
            unexpectedFromFound found

        DescribeFloat found ->
            unexpectedFromFound found

        DescribeFloatBetween _ _ found ->
            unexpectedFromFound found

        DescribeIntBetween _ _ found ->
            unexpectedFromFound found

        DescribeText rng textNodes ->
            []

        DescribeString rng str ->
            []

        DescribeMultiline rng str ->
            []

        DescribeStringExactly rng str ->
            []

        DescribeDate found ->
            unexpectedFromFound found


getNestedUnexpecteds (Nested nest) =
    case nest.content of
        ( desc, items ) ->
            getUnexpecteds desc
                ++ List.concatMap
                    getUnexpecteds
                    items


spelunkUnexpectedsFromFound found =
    case found of
        Found _ desc ->
            getUnexpecteds desc

        Unexpected unexpected ->
            [ unexpected ]


unexpectedFromFound found =
    case found of
        Found _ _ ->
            []

        Unexpected unexpected ->
            [ unexpected ]


{-| -}
getContainingDescriptions : Description -> { start : Int, end : Int } -> List Description
getContainingDescriptions description offset =
    case description of
        DescribeBlock name details ->
            getWithinFound offset details.found

        Record name details ->
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

        OneOf expected found ->
            getWithinFound offset found

        ManyOf expected rng foundList ->
            List.concatMap (getWithinFound offset) foundList

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

        DescribeBoolean found ->
            if withinFoundLeaf offset found then
                [ description ]

            else
                []

        DescribeInteger found ->
            if withinFoundLeaf offset found then
                [ description ]

            else
                []

        DescribeFloat found ->
            if withinFoundLeaf offset found then
                [ description ]

            else
                []

        DescribeFloatBetween _ _ found ->
            if withinFoundLeaf offset found then
                [ description ]

            else
                []

        DescribeIntBetween _ _ found ->
            if withinFoundLeaf offset found then
                [ description ]

            else
                []

        DescribeText rng textNodes ->
            if withinOffsetRange offset rng then
                [ description ]

            else
                []

        DescribeString rng str ->
            if withinOffsetRange offset rng then
                [ description ]

            else
                []

        DescribeMultiline rng str ->
            if withinOffsetRange offset rng then
                [ description ]

            else
                []

        DescribeStringExactly rng str ->
            if withinOffsetRange offset rng then
                [ description ]

            else
                []

        DescribeDate found ->
            if withinFoundLeaf offset found then
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


type alias PrintCursor =
    { indent : Int
    , position : Position
    , printed : String
    }


{-| -}
toString : Parsed -> String
toString (Parsed parsed) =
    writeFound writeDescription
        parsed.found
        { indent = 0
        , position = { line = 1, column = 1, offset = 0 }
        , printed = ""
        }
        |> .printed


write : String -> PrintCursor -> PrintCursor
write str cursor =
    { cursor
        | printed = cursor.printed ++ str
        , position =
            (\pos ->
                { pos
                    | offset = pos.offset + String.length str
                    , column = pos.column + String.length str
                }
            )
                cursor.position
    }


writeNewline : PrintCursor -> PrintCursor
writeNewline cursor =
    { cursor
        | printed = cursor.printed ++ "\n"
        , position =
            (\pos ->
                { pos
                    | offset = pos.offset + 1
                    , column = 1
                    , line = pos.line + 1
                }
            )
                cursor.position
    }


writeNewlines : Int -> PrintCursor -> PrintCursor
writeNewlines n cursor =
    { cursor
        | printed = cursor.printed ++ String.repeat n "\n"
        , position =
            (\pos ->
                { pos
                    | offset = pos.offset + n
                    , column = 1
                    , line = pos.line + n
                }
            )
                cursor.position
    }


{-| Add spaces and newlines in order to make up the discrepancy between cursor and target
-}
advanceTo : Range -> PrintCursor -> PrintCursor
advanceTo target cursor =
    let
        lineDiff =
            abs (target.start.line - cursor.position.line)
    in
    if target.start == cursor.position then
        cursor

    else if lineDiff == 0 then
        write (String.repeat (target.start.column - cursor.position.column) " ") cursor

    else
        cursor
            |> writeNewlines lineDiff
            |> write (String.repeat (target.start.column - 1) " ")


writeIndent : PrintCursor -> PrintCursor
writeIndent cursor =
    write (String.repeat (cursor.indent * 4) " ") cursor


writeLine line cursor =
    cursor
        |> write line
        |> writeNewline


indent : PrintCursor -> PrintCursor
indent cursor =
    { cursor | indent = cursor.indent + 1 }


dedent : PrintCursor -> PrintCursor
dedent cursor =
    { cursor | indent = max 0 cursor.indent - 1 }


{-| -}
writeDescription : Description -> PrintCursor -> PrintCursor
writeDescription description cursor =
    case description of
        DescribeBlock name details ->
            cursor
                |> write ("| " ++ name)
                |> indent
                |> writeFound writeDescription details.found
                |> dedent

        DescribeStub name found ->
            cursor
                |> write "|"
                |> writeFound (writeWith identity) found

        Record name details ->
            writeIndent cursor
                |> write ("| " ++ name)
                |> indent
                |> writeFound
                    (\fields curs -> List.foldr writeField curs fields)
                    details.found
                |> dedent

        OneOf expected found ->
            cursor
                |> writeFound writeDescription found

        ManyOf expected range found ->
            List.foldl
                (writeFound writeDescription)
                cursor
                found

        StartsWith range start end ->
            cursor
                |> writeDescription start.found
                |> writeDescription end.found

        DescribeBoolean foundBoolean ->
            writeFound (writeWith boolToString) foundBoolean cursor

        DescribeInteger found ->
            writeFound (writeWith String.fromInt) found cursor

        DescribeFloat found ->
            writeFound (writeWith Tuple.first) found cursor

        DescribeFloatBetween low high found ->
            writeFound (writeWith Tuple.first) found cursor

        DescribeIntBetween low high found ->
            writeFound (writeWith String.fromInt) found cursor

        DescribeText range textNodes ->
            cursor
                |> advanceTo range
                |> (\c -> List.foldl writeTextDescription c textNodes)

        DescribeString range str ->
            cursor
                |> advanceTo range
                |> write str

        DescribeMultiline range str ->
            let
                indented =
                    String.lines str
                        |> List.indexedMap
                            (\i s ->
                                if s == "" || i == 0 then
                                    s

                                else
                                    String.repeat (cursor.indent * 4) " " ++ s
                            )

                numLines =
                    List.length indented
            in
            cursor
                |> advanceTo range
                |> (\curs ->
                        List.foldl
                            (\line ( i, advancedCurs ) ->
                                if i == numLines then
                                    ( i + 1, write line advancedCurs )

                                else
                                    ( i + 1, writeLine line advancedCurs )
                            )
                            ( 1, curs )
                            indented
                   )
                |> Tuple.second

        DescribeStringExactly range str ->
            cursor
                |> advanceTo range
                |> write str

        DescribeDate foundPosix ->
            writeFound (writeWith Tuple.first) foundPosix cursor

        DescribeTree tree ->
            case tree.found of
                ( range, nestedItems ) ->
                    cursor
                        |> advanceTo range
                        |> (\curs -> List.foldl writeNested curs nestedItems)


writeNested (Nested node) cursor =
    cursor
        |> writeDescription (Tuple.first node.content)
        |> (\curs -> List.foldl writeDescription curs (Tuple.second node.content))
        |> indent
        |> (\curs -> List.foldl writeNested curs node.children)
        |> dedent


textDescriptionToString txt =
    case txt of
        Styled range t ->
            textToString t

        DescribeInline name range inlineDesc ->
            "{" ++ name ++ String.join "" (List.map inlineDescToString inlineDesc) ++ "}"

        UnexpectedInline unexpected ->
            ""


inlineDescToString : InlineDescription -> String
inlineDescToString inlineDesc =
    case inlineDesc of
        DescribeInlineString name range value ->
            name ++ " = " ++ value

        DescribeInlineText range txts ->
            String.join "" (List.map textToString txts)


writeTextDescription desc curs =
    write (textDescriptionToString desc) curs


writeTextNode node curs =
    write (textToString node) curs


textToString : Text -> String
textToString (Text styles txt) =
    txt


boolToString : Bool -> String
boolToString b =
    if b then
        "True"

    else
        "False"


writeWith toStr a cursor =
    write (toStr a) cursor


writeFound : (a -> PrintCursor -> PrintCursor) -> Found a -> PrintCursor -> PrintCursor
writeFound fn found cursor =
    case found of
        Found range fnd ->
            cursor
                |> advanceTo range
                |> fn fnd

        Unexpected unexpected ->
            cursor


writeField : ( String, Found Description ) -> PrintCursor -> PrintCursor
writeField ( name, foundVal ) cursor =
    case foundVal of
        Found rng fnd ->
            cursor
                |> advanceTo rng
                |> write (name ++ " = ")
                |> writeDescription fnd

        Unexpected unexpected ->
            cursor


type alias ErrorMessage =
    { message : List Format.Text
    , region : { start : Position, end : Position }
    , title : String
    }


{-| -}
type ProblemMessage
    = MsgDocumentMismatch
    | MsgParsingIssue (List (Parser.DeadEnd Context Problem))
    | MsgUnknownBlock (List String)
    | MsgUnknownInline (List InlineExpectation)
    | MsgMissingFields (List String)
    | MsgNonMatchingFields
        { expecting : List String
        , found : List String
        }
    | MsgUnexpectedField
        { found : String
        , options : List String
        , recordName : String
        }
    | MsgExpectingIndent Int
    | MsgCantStartTextWithSpace
    | MsgUnclosedStyle (List Style)
    | MsgBadDate String
    | MsgBadFloat String
    | MsgBadInt String
    | MsgBadBool String
    | MsgIntOutOfRange
        { found : Int
        , min : Int
        , max : Int
        }
    | MsgFloatOutOfRange
        { found : Float
        , min : Float
        , max : Float
        }


type alias Position =
    { offset : Int
    , line : Int
    , column : Int
    }


type alias Range =
    { start : Position
    , end : Position
    }


{-| -}
type Style
    = Bold
    | Italic
    | Strike


{-| -}
type Nested item
    = Nested
        { content : item
        , children :
            List (Nested item)
        }



{- BLOCKS -}


{-| -}
document :
    ({ found : Found child
     , expected : Expectation
     }
     -> result
    )
    -> Block child
    -> Document result
document renderer child =
    let
        expectation =
            getBlockExpectation child
    in
    Document
        { expect = expectation
        , converter =
            \(Parsed parsed) ->
                case parsed.found of
                    Found range childDesc ->
                        case renderBlock child childDesc of
                            Err err ->
                                Err err

                            Ok renderedChild ->
                                Ok
                                    (renderer
                                        { found = renderedChild
                                        , expected = expectation
                                        }
                                    )

                    Unexpected unexpected ->
                        Ok
                            (renderer
                                { expected = expectation
                                , found = Unexpected unexpected
                                }
                            )
        , parser =
            Parser.succeed
                (\source ( range, val ) ->
                    Parsed
                        { errors = List.map (renderError source) (getUnexpecteds val)
                        , found = Found range val
                        , expected = getBlockExpectation child
                        , focus = Nothing
                        }
                )
                |= Parser.getSource
                |= withRange
                    (Parser.withIndent 0 (getParser child))
        }


{-| Change the result of a block by applying a function to it.
-}
map : (a -> b) -> Block a -> Block b
map fn child =
    case child of
        Block name details ->
            Block name
                { converter = Result.map (mapFound fn) << details.converter
                , parser = details.parser
                , expect = details.expect
                }

        Value details ->
            Value
                { converter = Result.map (mapFound fn) << details.converter
                , parser = details.parser
                , expect = details.expect
                }


{-| -}
mapFound : (a -> b) -> Found a -> Found b
mapFound fn found =
    case found of
        Found range item ->
            Found range (fn item)

        Unexpected unexp ->
            Unexpected unexp


{-| -}
stub : String -> (Range -> result) -> Block result
stub name renderer =
    Block name
        { expect = ExpectStub name
        , converter =
            \desc ->
                case desc of
                    DescribeStub actualBlockName found ->
                        if actualBlockName == name then
                            case found of
                                Found range _ ->
                                    Ok (Found range (renderer range))

                                Unexpected unexpected ->
                                    Ok (Unexpected unexpected)

                        else
                            Err InvalidAst

                    _ ->
                        Err InvalidAst
        , parser =
            Parser.map
                (\( range, _ ) ->
                    DescribeStub name (Found range name)
                )
                (withRange
                    (Parser.succeed ()
                        |. Parser.keyword (Parser.Token name (ExpectingBlockName name))
                        |. Parser.chompWhile (\c -> c == ' ')
                    )
                )
        }


{-| -}
block :
    String
    ->
        ({ found : Found child
         , expected : Expectation
         }
         -> result
        )
    -> Block child
    -> Block result
block name renderer child =
    Block name
        { expect = ExpectBlock name (getBlockExpectation child)
        , converter =
            \desc ->
                case desc of
                    DescribeBlock actualBlockName blockDetails ->
                        if actualBlockName == name then
                            case blockDetails.found of
                                Found range found ->
                                    case renderBlock child found of
                                        Err err ->
                                            -- However it's not obvious when this would occur compared to
                                            -- just being Unexpected.
                                            -- I guess if a block, had a containing block and the names didn't match.
                                            -- In Which case, what would the error state be?
                                            Err err

                                        Ok renderedChild ->
                                            Ok
                                                (Found range
                                                    (renderer
                                                        { found =
                                                            -- Found renderedChild
                                                            renderedChild
                                                        , expected = blockDetails.expected
                                                        }
                                                    )
                                                )

                                Unexpected unexpected ->
                                    Ok
                                        (Found unexpected.range
                                            (renderer
                                                { found =
                                                    Unexpected unexpected
                                                , expected = blockDetails.expected
                                                }
                                            )
                                        )

                        else
                            -- The NoMatch error, allows the parent to decide what to do.
                            -- oneOf would go to the next option
                            -- document could default to an error state
                            Err NoMatch

                    _ ->
                        Err NoMatch
        , parser =
            Parser.map
                (\( range, valueResult ) ->
                    case valueResult of
                        Ok value ->
                            DescribeBlock name
                                { found = Found range value
                                , expected = ExpectBlock name (getBlockExpectation child)
                                }

                        Err ( pos, errorMessage ) ->
                            DescribeBlock name
                                { found =
                                    Unexpected
                                        { range = pos
                                        , problem = errorMessage
                                        }
                                , expected = ExpectBlock name (getBlockExpectation child)
                                }
                )
            <|
                withRange
                    (Parser.getIndent
                        |> Parser.andThen
                            (\indentation ->
                                Parser.succeed identity
                                    |. Parser.keyword (Parser.Token name (ExpectingBlockName name))
                                    |. Parser.chompWhile (\c -> c == ' ')
                                    |. skipBlankLineWith ()
                                    |= Parser.oneOf
                                        [ (Parser.succeed identity
                                            |= getPosition
                                            |. Parser.token (Parser.Token (String.repeat (indentation + 4) " ") (ExpectingIndent (indentation + 4)))
                                          )
                                            |> Parser.andThen
                                                (\start ->
                                                    Parser.oneOf
                                                        -- If there's st
                                                        [ Parser.succeed
                                                            (\end ->
                                                                Err
                                                                    ( { start = start, end = end }
                                                                    , MsgExpectingIndent (indentation + 4)
                                                                    )
                                                            )
                                                            |. Parser.chompIf (\c -> c == ' ') Space
                                                            |. Parser.chompWhile (\c -> c == ' ')
                                                            |= getPosition
                                                            |. Parser.loop "" (raggedIndentedStringAbove indentation)
                                                        , Parser.map Ok <|
                                                            Parser.withIndent (indentation + 4) (Parser.inContext (InBlock name) (getParser child))
                                                        ]
                                                )

                                        -- If we're here, it's because the indentation failed.
                                        -- If the child parser failed in some way, it would
                                        -- take care of that itself by returning Unexpected
                                        , Parser.succeed
                                            (\( pos, foundIndent ) ->
                                                Err ( pos, MsgExpectingIndent (indentation + 4) )
                                            )
                                            |= withRange (Parser.chompWhile (\c -> c == ' '))
                                            |. Parser.loop "" (raggedIndentedStringAbove indentation)
                                        ]
                            )
                    )
        }


blockNameParser name =
    Parser.succeed identity
        |. Parser.keyword (Parser.Token name (ExpectingBlockName name))
        |. Parser.chompWhile (\c -> c == ' ')
        |. Parser.chompIf (\c -> c == '\n') Newline


{-| -}
startWith : (Found ( start, rest ) -> result) -> Block start -> Block rest -> Block result
startWith fn startBlock endBlock =
    Value
        { expect = ExpectStartsWith (getBlockExpectation startBlock) (getBlockExpectation endBlock)
        , converter =
            \desc ->
                case desc of
                    StartsWith range start end ->
                        case ( renderBlock startBlock start.found, renderBlock endBlock end.found ) of
                            ( Ok (Found startRange renderedStart), Ok (Found endRange renderedEnd) ) ->
                                Ok <|
                                    Found
                                        range
                                        (fn (Found range ( renderedStart, renderedEnd )))

                            ( Ok (Unexpected unexpected), _ ) ->
                                Ok (Found range (fn (Unexpected unexpected)))

                            ( _, Ok (Unexpected unexpected) ) ->
                                Ok (Found range (fn (Unexpected unexpected)))

                            _ ->
                                Err InvalidAst

                    _ ->
                        Err InvalidAst
        , parser =
            Parser.succeed
                (\( range, ( begin, end ) ) ->
                    StartsWith range
                        { found = begin
                        , expected = getBlockExpectation startBlock
                        }
                        { found = end
                        , expected = getBlockExpectation endBlock
                        }
                )
                |= withRange
                    (Parser.succeed Tuple.pair
                        |= getParser startBlock
                        |. Parser.loop 0 manyBlankLines
                        |= getParser endBlock
                    )
        }


{-| Skip all blank lines.
-}
manyBlankLines lineCount =
    Parser.oneOf
        [ skipBlankLineWith (Parser.Loop (lineCount + 1))
        , Parser.succeed (Parser.Done ())
        ]


{-| -}
oneOf : (UnexpectedDetails -> a) -> List (Block a) -> Block a
oneOf renderUnexpected blocks =
    let
        gatherParsers myBlock ( names, blks, vals ) =
            case myBlock of
                Block name { parser } ->
                    ( name :: names, Parser.map Ok parser :: blks, vals )

                Value { parser } ->
                    ( names, blks, Parser.map Ok parser :: vals )

        ( blockNames, childBlocks, childValues ) =
            List.foldl gatherParsers ( [], [], [] ) blocks

        blockParser =
            Parser.succeed identity
                |. Parser.token (Parser.Token "|" BlockStart)
                |. Parser.oneOf
                    [ Parser.chompIf (\c -> c == ' ') Space
                    , Parser.succeed ()
                    ]
                |= Parser.oneOf
                    (List.reverse childBlocks
                        ++ [ Parser.getIndent
                                |> Parser.andThen
                                    (\indentation ->
                                        Parser.succeed
                                            (\( pos, foundWord ) ->
                                                Err ( pos, MsgUnknownBlock blockNames )
                                            )
                                            |= withRange word
                                            |. newline
                                            |. Parser.loop "" (raggedIndentedStringAbove indentation)
                                    )
                           ]
                    )

        applyDesc description blck found =
            case found of
                Nothing ->
                    case renderBlock blck description of
                        Err err ->
                            found

                        Ok rendered ->
                            Just rendered

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
                    OneOf expected foundResult ->
                        case foundResult of
                            Found rng found ->
                                case List.foldl (applyDesc found) Nothing blocks of
                                    Nothing ->
                                        Err InvalidAst

                                    Just result ->
                                        Ok result

                            Unexpected unexpected ->
                                Ok (Found unexpected.range (renderUnexpected unexpected))

                    _ ->
                        Err InvalidAst
        , parser =
            Parser.succeed
                (\( range, result ) ->
                    case result of
                        Ok found ->
                            OneOf expectations (Found range found)

                        Err ( pos, unexpected ) ->
                            OneOf expectations
                                (Unexpected
                                    { range = pos
                                    , problem = unexpected
                                    }
                                )
                )
                |= withRange
                    (Parser.oneOf
                        (blockParser :: List.reverse childValues
                         -- TODO :: What sort of error would occur if this point was reached?
                        )
                    )
        }


{-| Many blocks that are all at the same indentation level.
-}
manyOf : (UnexpectedDetails -> a) -> List (Block a) -> Block (List a)
manyOf renderUnexpected blocks =
    let
        gatherParsers myBlock ( names, blks, vals ) =
            case myBlock of
                Block name { parser } ->
                    ( name :: names, Parser.map Ok parser :: blks, vals )

                Value { parser } ->
                    ( names, blks, Parser.map Ok (withRange parser) :: vals )

        ( blockNames, childBlocks, childValues ) =
            List.foldl gatherParsers ( [], [], [] ) blocks

        blockParser =
            Parser.map
                (\( pos, result ) ->
                    result
                        |> Result.map (\desc -> ( pos, desc ))
                )
                (withRange
                    (Parser.succeed identity
                        |. Parser.token (Parser.Token "|" BlockStart)
                        |. Parser.oneOf
                            [ Parser.chompIf (\c -> c == ' ') Space
                            , Parser.succeed ()
                            ]
                        |= Parser.oneOf
                            (List.reverse childBlocks
                                ++ [ Parser.getIndent
                                        |> Parser.andThen
                                            (\indentation ->
                                                Parser.succeed
                                                    (\( pos, foundWord ) ->
                                                        Err ( pos, MsgUnknownBlock blockNames )
                                                    )
                                                    |= withRange word
                                                    |. newline
                                                    |. Parser.loop "" (raggedIndentedStringAbove indentation)
                                            )
                                   ]
                            )
                    )
                )

        expectations =
            List.map getBlockExpectation blocks
    in
    Value
        { expect = ExpectManyOf expectations
        , converter =
            \desc ->
                let
                    applyDesc description blck found =
                        case found of
                            Nothing ->
                                case renderBlock blck description of
                                    Err err ->
                                        found

                                    Ok rendered ->
                                        Just rendered

                            _ ->
                                found

                    getRendered : Found Description -> Result AstError (List a) -> Result AstError (List a)
                    getRendered found existingResult =
                        case existingResult of
                            Err err ->
                                Err err

                            Ok existing ->
                                case found of
                                    Unexpected unexpected ->
                                        Ok (renderUnexpected unexpected :: existing)

                                    Found range child ->
                                        case List.foldl (applyDesc child) Nothing blocks of
                                            Nothing ->
                                                Err InvalidAst

                                            Just (Found _ result) ->
                                                Ok (result :: existing)

                                            Just (Unexpected unexpected) ->
                                                Ok (renderUnexpected unexpected :: existing)
                in
                case desc of
                    ManyOf _ range found ->
                        List.foldl getRendered (Ok []) found
                            |> Result.map (\items -> Found range (List.reverse items))

                    _ ->
                        Err NoMatch
        , parser =
            Parser.succeed
                (\( range, results ) ->
                    ManyOf expectations range (List.map resultToFound results)
                )
                |= withRange
                    (Parser.getIndent
                        |> Parser.andThen
                            (\indentation ->
                                Parser.loop ( False, [] )
                                    (blocksOrNewlines (Parser.oneOf (blockParser :: List.reverse childValues)) indentation)
                            )
                    )
        }


{-| It can be useful to parse a tree structure. For example, here's a nested list.
| List

  - item one
  - item two
  - nested item two
    additional text for nested item two
  - item three
  - nested item three
    In order to parse the above, you could define a block as
    Mark.block "List"
    ((Nested nested) ->
    -- Do something with nested.content and nested.children
    )
    (Mark.nested
    { item = text
    , start = Mark.exactly "-" ()
    }
    )
    **Note** the indentation is always a multiple of 4.
    **Another Note** `text` in the above code is defined elsewhere.

-}
nested :
    { item : Block item
    , start : Block icon
    }
    -> Block (List (Nested ( icon, List item )))
nested config =
    let
        expectation =
            ExpectTree (getBlockExpectation config.start) (getBlockExpectation config.item)
    in
    Value
        { expect = expectation
        , converter =
            \description ->
                case description of
                    DescribeTree details ->
                        case details.found of
                            ( pos, nestedDescriptors ) ->
                                case reduceRender (renderTreeNodeSmall config) nestedDescriptors of
                                    Err invalidAst ->
                                        Err invalidAst

                                    Ok (Err unexpectedDetails) ->
                                        Ok (Unexpected unexpectedDetails)

                                    Ok (Ok list) ->
                                        Ok (Found pos list)

                    _ ->
                        Err InvalidAst
        , parser =
            Parser.getIndent
                |> Parser.andThen
                    (\baseIndent ->
                        Parser.map
                            (\( pos, result ) ->
                                DescribeTree
                                    { found = ( pos, buildTree baseIndent result )
                                    , expected = expectation
                                    }
                            )
                            (withRange
                                (Parser.loop
                                    ( { base = baseIndent
                                      , prev = baseIndent
                                      }
                                    , []
                                    )
                                    (indentedBlocksOrNewlines config.start config.item)
                                )
                            )
                    )
        }


mapNested : (a -> b) -> Nested a -> Nested b
mapNested fn (Nested node) =
    Debug.todo "map it"


{-| -}
type alias Index =
    List Int


replaceNested : (Index -> a -> List b -> b) -> Nested a -> b
replaceNested =
    replaceNestedHelper []


replaceNestedHelper : Index -> (Index -> a -> List b -> b) -> Nested a -> b
replaceNestedHelper index fn (Nested node) =
    let
        newIndex =
            1 :: index

        children =
            List.foldl
                (\child ( i, gathered ) ->
                    ( i + 1, replaceNestedHelper (i :: newIndex) fn child :: gathered )
                )
                ( 1, [] )
                node.children
    in
    fn newIndex node.content (Tuple.second children)


{-| -}
foldNestedList : (Index -> a -> b -> b) -> b -> List (Nested a) -> b
foldNestedList fn accum nodes =
    List.foldl
        (\child ( i, gathered ) ->
            ( i + 1, foldNestedHelper [ i ] fn gathered child )
        )
        ( 1, accum )
        nodes
        |> Tuple.second


{-| -}
foldNested : (Index -> a -> b -> b) -> b -> Nested a -> b
foldNested fn accum node =
    foldNestedHelper [ 1 ] fn accum node


foldNestedHelper : Index -> (Index -> a -> b -> b) -> b -> Nested a -> b
foldNestedHelper index fn accum (Nested node) =
    let
        newIndex =
            1 :: index

        advanced =
            fn newIndex node.content accum
    in
    List.foldl
        (\child ( i, gathered ) ->
            ( i + 1, foldNestedHelper (i :: newIndex) fn gathered child )
        )
        ( 1, advanced )
        node.children
        |> Tuple.second


{-| -}
record2 :
    String
    -> (Range -> one -> two -> data)
    -> (UnexpectedDetails -> data)
    -> Field one
    -> Field two
    -> Block data
record2 recordName renderer renderUnexpected field1 field2 =
    let
        expectations =
            ExpectRecord recordName [ fieldExpectation field1, fieldExpectation field2 ]
    in
    Block recordName
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record name fields ->
                        if name == recordName then
                            case fields.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok (renderer pos))
                                        |> Result.map2 applyField (getField field1 fieldDescriptions)
                                        |> Result.map2 applyField (getField field2 fieldDescriptions)
                                        |> renderRecordResult renderUnexpected pos

                                Unexpected unexpected ->
                                    Ok (Found unexpected.range (renderUnexpected unexpected))

                        else
                            Err NoMatch

                    _ ->
                        Err NoMatch
        , parser =
            parseRecord recordName expectations [ fieldParser field1, fieldParser field2 ]
        }



{- TEXT BLOCKS -}


{-| Handling formatted text is a little more involved than may be initially apparent.

Text styling can be overlapped such as

    /My italicized sentence can have *bold*/

In order to render this, the above sentence is chopped up into `Text` fragments that can have multiple styles active.

  - `view` is the function to render an individual fragment.
  - `inlines` are custom inline blocks. These are how links are implemented in `Mark.Default`!
  - `replacements` will replace characters before rendering. For example, we can replace `...` with the real ellipses unicode character, `…`.

**Note** check out `Mark.Default.text` to see an example.

-}
text :
    { view : Range -> Text -> rendered
    , error : UnexpectedDetails -> rendered
    , inlines : List (Inline rendered)
    , replacements : List Replacement
    }
    -> Block (List rendered)
text options =
    Value
        { expect = ExpectText (List.map getInlineExpectation options.inlines)
        , converter = renderText options
        , parser =
            getPosition
                |> Parser.andThen
                    (\pos ->
                        styledTextParser
                            { inlines = List.map getInlineExpectation options.inlines
                            , replacements = options.replacements
                            }
                            pos
                            []
                            []
                    )
        }


renderText :
    { view : Range -> Text -> rendered
    , error : UnexpectedDetails -> rendered
    , inlines : List (Inline rendered)
    , replacements : List Replacement
    }
    -> Description
    -> Result AstError (Found (List rendered))
renderText options description =
    case description of
        DescribeText range textNodes ->
            List.foldl (renderTextComponent options) [] textNodes
                |> (Found range << List.reverse)
                |> Ok

        _ ->
            Err InvalidAst


renderTextComponent options comp found =
    case comp of
        Styled range textEl ->
            options.view range textEl :: found

        DescribeInline name range foundInline ->
            case List.foldl (renderInline name range (List.reverse foundInline)) (Err InvalidAst) options.inlines of
                Err err ->
                    options.error
                        { range = range
                        , problem = MsgUnknownInline (List.map getInlineExpectation options.inlines)
                        }
                        :: found

                Ok list ->
                    list ++ found

        UnexpectedInline details ->
            options.error details :: found


renderInline name range pieces (Inline inlineName details) found =
    case found of
        Ok _ ->
            found

        Err error ->
            if name == inlineName then
                -- inlineRenderer
                details.converter pieces

            else
                found


{-| -}
type Replacement
    = Replacement String String
    | Balanced
        { start : ( String, String )
        , end : ( String, String )
        }


{-| -}
type Inline data
    = Inline
        String
        { converter : List InlineDescription -> Result AstError (List data)
        , expect : InlineExpectation
        }


getInlineExpectation (Inline name details) =
    details.expect



{- Errors for inlines.

   UnknownInline
   InlineUnexpectedFormat
       -> Needs Text Value
       -> Needs String Value


-}


{-| -}
mapInline : (a -> b) -> Inline a -> Inline b
mapInline fn (Inline name details) =
    Inline name
        { converter =
            \desc ->
                Result.map (List.map fn) (details.converter desc)
        , expect =
            details.expect
        }


{-| -}
inline : String -> result -> Inline result
inline name result =
    Inline name
        { converter =
            \descriptor ->
                Ok [ result ]
        , expect =
            InlineExpectation name []
        }


{-| -}
inlineString : String -> Inline (String -> result) -> Inline result
inlineString name (Inline inlineName details) =
    Inline inlineName
        { converter =
            \descriptors ->
                case descriptors of
                    [] ->
                        Err InvalidAst

                    (DescribeInlineString key range str) :: remaining ->
                        case details.converter remaining of
                            Err err ->
                                Err err

                            Ok renderers ->
                                Ok (List.map (\x -> x str) renderers)

                    _ ->
                        Err InvalidAst
        , expect =
            case details.expect of
                InlineExpectation parentName vals ->
                    InlineExpectation parentName
                        (vals ++ [ ExpectInlineString name ])
        }


{-| -}
inlineText : Inline (List Text -> result) -> Inline result
inlineText (Inline inlineName details) =
    Inline inlineName
        { converter =
            \descriptors ->
                case descriptors of
                    [] ->
                        Err InvalidAst

                    (DescribeInlineText range str) :: remaining ->
                        case details.converter remaining of
                            Err err ->
                                Err err

                            Ok renderers ->
                                Ok (List.map (\x -> x str) renderers)

                    _ ->
                        Err InvalidAst
        , expect =
            case details.expect of
                InlineExpectation parentName vals ->
                    InlineExpectation parentName (vals ++ [ ExpectInlineText ])
        }



{- PRIMITIVE BLOCKS -}


{-| -}
multiline : Block String
multiline =
    Value
        { expect = ExpectMultiline
        , converter =
            \desc ->
                case desc of
                    DescribeMultiline range str ->
                        Ok (Found range str)

                    _ ->
                        Err InvalidAst
        , parser =
            Parser.map
                (\( pos, str ) ->
                    DescribeMultiline pos str
                )
                (withRange
                    (Parser.getIndent
                        |> Parser.andThen
                            (\indentation ->
                                Parser.loop "" (indentedString indentation)
                            )
                    )
                )
        }


{-| -}
string : Block String
string =
    Value
        { expect = ExpectString
        , converter =
            \desc ->
                case desc of
                    DescribeString range str ->
                        Ok (Found range str)

                    _ ->
                        Err InvalidAst
        , parser =
            Parser.succeed
                (\start val end ->
                    DescribeString { start = start, end = end } val
                )
                |= getPosition
                |= Parser.getChompedString
                    (Parser.chompWhile
                        (\c -> c /= '\n')
                    )
                |= getPosition
        }


{-| Parse an ISO-8601 date string.

Format: `YYYY-MM-DDTHH:mm:ss.SSSZ`

Though you don't need to specify all segments, so `YYYY-MM-DD` works as well.

Results in a `Posix` integer, which works well with [elm/time](https://package.elm-lang.org/packages/elm/time/latest/).

-}
date : Block Time.Posix
date =
    Value
        { expect = ExpectDate
        , converter =
            \desc ->
                case desc of
                    DescribeDate found ->
                        Ok (mapFound Tuple.second found)

                    _ ->
                        Err InvalidAst
        , parser =
            Parser.map
                (\( pos, parsedPosix ) ->
                    case parsedPosix of
                        Err str ->
                            DescribeDate
                                (Unexpected
                                    { range = pos
                                    , problem = MsgBadDate str
                                    }
                                )

                        Ok ( str, posix ) ->
                            DescribeDate (Found pos ( str, posix ))
                )
                (withRange
                    (Parser.getChompedString
                        (Parser.chompWhile
                            (\c -> c /= '\n')
                        )
                        |> Parser.andThen
                            (\str ->
                                case Iso8601.toTime str of
                                    Err err ->
                                        Parser.succeed (Err str)

                                    Ok parsedPosix ->
                                        Parser.succeed (Ok ( str, parsedPosix ))
                            )
                    )
                )
        }


foundToResult found err =
    case found of
        Found _ b ->
            Ok b

        _ ->
            Err err


{-| -}
exactly : String -> value -> Block value
exactly key value =
    Value
        { expect = ExpectStringExactly key
        , converter =
            \desc ->
                case desc of
                    DescribeStringExactly range existingKey ->
                        if key == existingKey then
                            Ok (Found range value)

                        else
                            Err NoMatch

                    _ ->
                        Err NoMatch
        , parser =
            Parser.succeed
                (\start _ end ->
                    DescribeStringExactly { start = start, end = end } key
                )
                |= getPosition
                |= Parser.token (Parser.Token key (Expecting key))
                |= getPosition
        }


{-| Parse either `True` or `False`.
-}
bool : Block Bool
bool =
    Value
        { expect = ExpectBoolean
        , converter =
            \desc ->
                case desc of
                    DescribeBoolean found ->
                        Ok found

                    _ ->
                        Err NoMatch
        , parser =
            Parser.map
                (\( range, boolResult ) ->
                    DescribeBoolean <|
                        case boolResult of
                            Err err ->
                                Unexpected
                                    { range = range
                                    , problem = MsgBadBool err
                                    }

                            Ok b ->
                                Found range b
                )
                (withRange
                    (Parser.oneOf
                        [ Parser.token (Parser.Token "True" (Expecting "True"))
                            |> Parser.map (always (Ok True))
                        , Parser.token (Parser.Token "False" (Expecting "False"))
                            |> Parser.map (always (Ok False))
                        , Parser.map Err word
                        ]
                    )
                )
        }


int : Block Int
int =
    Value
        { converter =
            \desc ->
                case desc of
                    DescribeInteger found ->
                        Ok found

                    _ ->
                        Err NoMatch
        , expect = ExpectInteger
        , parser = Parser.map DescribeInteger integer
        }


float : Block Float
float =
    Value
        { converter =
            \desc ->
                case desc of
                    DescribeFloat found ->
                        Ok (mapFound Tuple.second found)

                    _ ->
                        Err NoMatch
        , expect = ExpectFloat
        , parser = Parser.map DescribeFloat floating
        }


{-| -}
intBetween : Int -> Int -> Block Int
intBetween one two =
    let
        top =
            max one two

        bottom =
            min one two
    in
    Value
        { expect = ExpectIntBetween bottom top
        , converter =
            \desc ->
                case desc of
                    DescribeIntBetween low high found ->
                        Ok found

                    _ ->
                        Err InvalidAst
        , parser =
            Parser.map
                (\found ->
                    DescribeIntBetween bottom top <|
                        case found of
                            Found rng i ->
                                if i >= bottom && i <= top then
                                    found

                                else
                                    Unexpected
                                        { range = rng
                                        , problem =
                                            MsgIntOutOfRange
                                                { found = i
                                                , min = bottom
                                                , max = top
                                                }
                                        }

                            _ ->
                                found
                )
                integer
        }


{-| -}
floatBetween : Float -> Float -> Block Float
floatBetween one two =
    let
        top =
            max one two

        bottom =
            min one two
    in
    Value
        { expect = ExpectFloatBetween bottom top
        , converter =
            \desc ->
                case desc of
                    DescribeFloatBetween low high found ->
                        Ok (mapFound Tuple.second found)

                    _ ->
                        Err InvalidAst
        , parser =
            Parser.map
                (\found ->
                    DescribeFloatBetween bottom top <|
                        case found of
                            Found rng ( str, i ) ->
                                if i >= bottom && i <= top then
                                    found

                                else
                                    Unexpected
                                        { range = rng
                                        , problem =
                                            MsgFloatOutOfRange
                                                { found = i
                                                , min = bottom
                                                , max = top
                                                }
                                        }

                            _ ->
                                found
                )
                floating
        }



{- Parser Heleprs -}


{-| -}
type Context
    = InBlock String
    | InInline String
    | InRecord String
    | InRecordField String


{-| -}
type Problem
    = ExpectingIndent Int
    | InlineStart
    | InlineEnd
    | BlockStart
    | Expecting String
    | ExpectingBlockName String
    | ExpectingInlineName String
    | ExpectingFieldName String
    | Escape
    | EscapedChar
    | Newline
    | Space
    | End
    | Integer
    | FloatingPoint
    | InvalidNumber


{-| -}
raggedIndentedStringAbove : Int -> String -> Parser Context Problem (Parser.Step String String)
raggedIndentedStringAbove indentation found =
    Parser.oneOf
        [ Parser.succeed
            (\extra ->
                Parser.Loop <|
                    if extra then
                        found ++ "\n\n"

                    else
                        found ++ "\n"
            )
            |. Parser.token (Parser.Token "\n" Newline)
            |= Parser.oneOf
                [ Parser.succeed True
                    |. Parser.backtrackable (Parser.chompWhile (\c -> c == ' '))
                    |. Parser.backtrackable (Parser.token (Parser.Token "\n" Newline))
                , Parser.succeed False
                ]
        , Parser.succeed
            (\indentCount str ->
                Parser.Loop (found ++ String.repeat indentCount " " ++ str)
            )
            |= Parser.oneOf
                (indentationBetween (indentation + 1) (indentation + 4))
            |= Parser.getChompedString
                (Parser.chompWhile
                    (\c -> c /= '\n')
                )
        , Parser.succeed (Parser.Done found)
        ]


{-| Parse any indentation between two bounds, inclusive.
-}
indentationBetween : Int -> Int -> List (Parser Context Problem Int)
indentationBetween lower higher =
    let
        bottom =
            min lower higher

        top =
            max lower higher
    in
    List.reverse
        (List.map
            (\numSpaces ->
                Parser.succeed numSpaces
                    |. Parser.token
                        (Parser.Token (String.repeat numSpaces " ")
                            (ExpectingIndent numSpaces)
                        )
            )
            (List.range bottom top)
        )


{-| -}
indentedString : Int -> String -> Parser Context Problem (Parser.Step String String)
indentedString indentation found =
    Parser.oneOf
        -- First line, indentation is already handled by the block constructor.
        [ Parser.succeed (Parser.Done found)
            |. Parser.end End
        , Parser.succeed
            (\extra ->
                Parser.Loop <|
                    if extra then
                        found ++ "\n\n"

                    else
                        found ++ "\n"
            )
            |. newline
            |= Parser.oneOf
                [ Parser.succeed True
                    |. Parser.backtrackable (Parser.chompWhile (\c -> c == ' '))
                    |. Parser.backtrackable (Parser.token (Parser.Token "\n" Newline))
                , Parser.succeed False
                ]
        , if found == "" then
            Parser.succeed (\str -> Parser.Loop (found ++ str))
                |= Parser.getChompedString
                    (Parser.chompWhile
                        (\c -> c /= '\n')
                    )

          else
            Parser.succeed
                (\str ->
                    Parser.Loop (found ++ str)
                )
                |. Parser.token (Parser.Token (String.repeat indentation " ") (ExpectingIndent indentation))
                |= Parser.getChompedString
                    (Parser.chompWhile
                        (\c -> c /= '\n')
                    )
        , Parser.succeed (Parser.Done found)
        ]


{-| -}
blocksOrNewlines : Parser Context Problem thing -> Int -> ( Bool, List thing ) -> Parser Context Problem (Parser.Step ( Bool, List thing ) (List thing))
blocksOrNewlines myParser indentation ( parsedSomething, existing ) =
    Parser.oneOf
        [ Parser.end End
            |> Parser.map
                (\_ ->
                    Parser.Done (List.reverse existing)
                )
        , Parser.succeed (Parser.Loop ( True, existing ))
            |. newline
        , if not parsedSomething then
            -- First thing already has indentation accounted for.
            myParser
                |> Parser.map
                    (\foundBlock ->
                        Parser.Loop ( True, foundBlock :: existing )
                    )

          else
            Parser.oneOf
                [ Parser.succeed
                    (\foundBlock ->
                        Parser.Loop ( True, foundBlock :: existing )
                    )
                    |. Parser.token (Parser.Token (String.repeat indentation " ") (ExpectingIndent indentation))
                    |= myParser
                , Parser.succeed (Parser.Loop ( True, existing ))
                    |. Parser.backtrackable (Parser.chompWhile (\c -> c == ' '))
                    |. Parser.backtrackable newline

                -- We reach here because the indentation parsing was not successful,
                -- meaning the indentation has been lowered and the block is done
                , Parser.succeed (Parser.Done (List.reverse existing))
                ]

        -- Whitespace Line
        , Parser.succeed (Parser.Loop ( True, existing ))
            |. Parser.chompWhile (\c -> c == ' ')
            |. newline
        ]


skipBlankLineWith : thing -> Parser Context Problem thing
skipBlankLineWith x =
    Parser.succeed x
        |. Parser.token (Parser.Token "\n" Newline)
        |. Parser.oneOf
            [ Parser.succeed ()
                |. Parser.backtrackable (Parser.chompWhile (\c -> c == ' '))
                |. Parser.backtrackable (Parser.token (Parser.Token "\n" Newline))
            , Parser.succeed ()
            ]


integer : Parser Context Problem (Found Int)
integer =
    Parser.map
        (\( pos, intResult ) ->
            case intResult of
                Ok i ->
                    Found pos i

                Err str ->
                    Unexpected
                        { range = pos
                        , problem = MsgBadInt str
                        }
        )
        (withRange
            (Parser.oneOf
                [ Parser.succeed
                    (\i str ->
                        if str == "" then
                            Ok (negate i)

                        else
                            Err (String.fromInt i ++ str)
                    )
                    |. Parser.token (Parser.Token "-" (Expecting "-"))
                    |= Parser.int Integer InvalidNumber
                    |= Parser.getChompedString (Parser.chompWhile (\c -> c /= ' ' && c /= '\n'))
                , Parser.succeed
                    (\i str ->
                        if str == "" then
                            Ok i

                        else
                            Err (String.fromInt i ++ str)
                    )
                    |= Parser.int Integer InvalidNumber
                    |= Parser.getChompedString (Parser.chompWhile (\c -> c /= ' ' && c /= '\n'))
                , Parser.succeed Err
                    |= word
                ]
            )
        )


{-| Parses a float and must end with whitespace, not additional characters.
-}
floating : Parser Context Problem (Found ( String, Float ))
floating =
    Parser.map
        (\( pos, floatResult ) ->
            case floatResult of
                Ok f ->
                    Found pos f

                Err str ->
                    Unexpected
                        { range = pos
                        , problem = MsgBadFloat str
                        }
        )
        (withRange
            (Parser.oneOf
                [ Parser.succeed
                    (\start fl end src extra ->
                        if extra == "" then
                            Ok ( String.slice start end src, negate fl )

                        else
                            Err (String.fromFloat fl ++ extra)
                    )
                    |= Parser.getOffset
                    |. Parser.token (Parser.Token "-" (Expecting "-"))
                    |= Parser.float FloatingPoint InvalidNumber
                    |= Parser.getOffset
                    |= Parser.getSource
                    |= Parser.getChompedString (Parser.chompWhile (\c -> c /= ' ' && c /= '\n'))
                , Parser.succeed
                    (\start fl end src extra ->
                        if extra == "" then
                            Ok ( String.slice start end src, fl )

                        else
                            Err (String.fromFloat fl ++ extra)
                    )
                    |= Parser.getOffset
                    |= Parser.float FloatingPoint InvalidNumber
                    |= Parser.getOffset
                    |= Parser.getSource
                    |= Parser.getChompedString (Parser.chompWhile (\c -> c /= ' ' && c /= '\n'))
                , Parser.succeed Err
                    |= word
                ]
            )
        )



{- PARSER HELPERS -}


withRange : Parser Context Problem thing -> Parser Context Problem ( Range, thing )
withRange parser =
    Parser.succeed
        (\start val end ->
            ( { start = start, end = end }, val )
        )
        |= getPosition
        |= parser
        |= getPosition


word : Parser Context Problem String
word =
    Parser.chompWhile Char.isAlphaNum
        |> Parser.getChompedString


peek : String -> Parser c p thing -> Parser c p thing
peek name parser =
    Parser.succeed
        (\start val end src ->
            let
                highlightParsed =
                    String.repeat (start.column - 1) " " ++ String.repeat (max 0 (end.column - start.column)) "^"

                fullLine =
                    String.slice (max 0 (start.offset - start.column)) end.offset src

                _ =
                    Debug.log name
                        -- fullLine
                        (String.slice start.offset end.offset src)

                -- _ =
                --     Debug.log name
                --         highlightParsed
            in
            val
        )
        |= getPosition
        |= parser
        |= getPosition
        |= Parser.getSource


getPosition : Parser c p Position
getPosition =
    Parser.succeed
        (\offset ( row, col ) ->
            { offset = offset
            , line = row
            , column = col
            }
        )
        |= Parser.getOffset
        |= Parser.getPosition



{- RECORD HELPERS -}


{-| -}
type Field value
    = Field String (Block value)


{-| -}
field : String -> Block value -> Field value
field name child =
    Field name child


fieldParser : Field value -> ( String, Parser Context Problem ( String, Found Description ) )
fieldParser (Field name myBlock) =
    ( name
    , withFieldName
        name
        (getParser myBlock)
    )


fieldName : Field v -> String
fieldName (Field name _) =
    name


fieldExpectation (Field name fieldBlock) =
    ( name, getBlockExpectation fieldBlock )



{- RECORD PARSER HELPERS -}


parseRecord :
    String
    -> Expectation
    -> List ( String, Parser Context Problem ( String, Found Description ) )
    -> Parser Context Problem Description
parseRecord recordName expectations fields =
    Parser.succeed
        (\( pos, foundFields ) ->
            case foundFields of
                Ok ok ->
                    Record recordName
                        { expected = expectations
                        , found =
                            Found pos ok
                        }

                Err ( maybePosition, problem ) ->
                    Record recordName
                        { expected = expectations
                        , found =
                            Unexpected
                                --unexpected
                                { range = Maybe.withDefault pos maybePosition
                                , problem = problem
                                }
                        }
        )
        |= withRange
            (Parser.getIndent
                |> Parser.andThen
                    (\indentation ->
                        Parser.succeed identity
                            |. Parser.keyword (Parser.Token recordName (ExpectingBlockName recordName))
                            |. Parser.chompWhile (\c -> c == ' ')
                            |. Parser.chompIf (\c -> c == '\n') Newline
                            |= Parser.withIndent (indentation + 4)
                                (Parser.loop
                                    { remaining = fields
                                    , found = Ok []
                                    }
                                    (parseFields recordName (List.map Tuple.first fields))
                                )
                    )
            )


withFieldName : String -> Parser Context Problem Description -> Parser Context Problem ( String, Found Description )
withFieldName name parser =
    Parser.getIndent
        |> Parser.andThen
            (\indentation ->
                Parser.map
                    (\( pos, description ) ->
                        ( name, Found pos description )
                    )
                <|
                    withRange
                        (Parser.succeed identity
                            |. Parser.keyword (Parser.Token name (ExpectingFieldName name))
                            |. Parser.chompWhile (\c -> c == ' ')
                            |. Parser.chompIf (\c -> c == '=') (Expecting "=")
                            |. Parser.chompWhile (\c -> c == ' ')
                            |= Parser.withIndent (indentation + 4) (Parser.inContext (InRecordField name) parser)
                        )
            )


unexpectedField recordName options =
    Parser.getIndent
        |> Parser.andThen
            (\indentation ->
                Parser.map
                    (\( ( range, name ), content ) ->
                        ( name
                        , Unexpected
                            { range = range
                            , problem =
                                MsgUnexpectedField
                                    { found = name
                                    , options = options
                                    , recordName = recordName
                                    }
                            }
                        )
                    )
                    (Parser.succeed Tuple.pair
                        |= withRange (Parser.getChompedString (Parser.chompWhile Char.isAlphaNum))
                        |. Parser.chompWhile (\c -> c == ' ')
                        |. Parser.chompIf (\c -> c == '=') (Expecting "=")
                        |. Parser.chompWhile (\c -> c == ' ')
                        -- TODO: parse multiline string
                        |= Parser.withIndent (indentation + 4) (Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n')))
                     -- |. newline
                     -- |. Parser.map (Debug.log "unexpected capture") (Parser.loop "" (raggedIndentedStringAbove (indent - 4)))
                    )
            )


resultToFound result =
    case result of
        Ok ( range, desc ) ->
            Found range desc

        Err ( range, problem ) ->
            Unexpected
                { range = range
                , problem = problem
                }


renderRecordResult renderUnexpected pos result =
    case result of
        Ok parsedCorrectly ->
            case parsedCorrectly of
                Ok rendered ->
                    Ok (Found pos rendered)

                Err unexpected ->
                    Ok
                        (Found
                            pos
                            (renderUnexpected unexpected)
                        )

        Err problem ->
            Ok
                (Found pos
                    (renderUnexpected
                        { problem = problem
                        , range = pos
                        }
                    )
                )


type alias RecordFields =
    { remaining : List ( String, Parser Context Problem ( String, Found Description ) )
    , found : Result ( Maybe Range, ProblemMessage ) (List ( String, Found Description ))
    }


type Indented thing
    = Indented thing
    | WeirdIndent Int
    | EmptyLine


{-| Either:

    1. Parses indent ++ parser ++ newline
        -> Success!
    2. Parses many spaces ++ newline
        -> Ignore completely
    3. Parses some number of spaces ++ some not newlines ++ newline
        -> Is improperly indented

-}
indentOrSkip indentation successParser =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.token (Parser.Token (String.repeat indentation " ") (ExpectingIndent indentation))
            |= Parser.oneOf
                [ Parser.map (always EmptyLine) newline
                , Parser.succeed
                    (\foundIndent content ->
                        if content /= "" then
                            WeirdIndent (String.length foundIndent)

                        else
                            EmptyLine
                    )
                    |. Parser.chompIf (\c -> c == ' ') Space
                    |= Parser.getChompedString (Parser.chompWhile (\c -> c == ' '))
                    |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n'))
                    |. newline
                , Parser.succeed Indented
                    |= successParser
                    |. newline
                ]
        , Parser.succeed
            (\foundIndent hasContent ->
                if hasContent then
                    WeirdIndent (String.length foundIndent)

                else
                    EmptyLine
            )
            |= Parser.getChompedString (Parser.chompWhile (\c -> c == ' '))
            |= Parser.oneOf
                [ Parser.map (always False) newline
                , Parser.succeed True
                    |. Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n'))
                    |. newline
                ]
        ]


newline =
    Parser.token (Parser.Token "\n" Newline)


{-| -}
parseFields :
    String
    -> List String
    -> RecordFields
    -> Parser Context Problem (Parser.Step RecordFields (Result ( Maybe Range, ProblemMessage ) (List ( String, Found Description ))))
parseFields recordName fieldNames fields =
    case fields.remaining of
        [] ->
            Parser.succeed (Parser.Done fields.found)

        _ ->
            case fields.found of
                Ok found ->
                    Parser.getIndent
                        |> Parser.andThen
                            (\indentation ->
                                Parser.oneOf
                                    [ indentOrSkip indentation (captureField found recordName fields fieldNames)
                                        |> Parser.map
                                            (\indentedField ->
                                                case indentedField of
                                                    Indented thing ->
                                                        thing

                                                    EmptyLine ->
                                                        Parser.Loop fields

                                                    WeirdIndent i ->
                                                        Parser.Loop
                                                            { found =
                                                                Err ( Nothing, MsgExpectingIndent indentation )
                                                            , remaining =
                                                                fields.remaining
                                                            }
                                            )

                                    -- We've reached here because:
                                    -- 1. We still have expected fields, but we didn't parse them.
                                    -- 2. No other errors occurred.
                                    -- 3. We did not find the correct indentation
                                    -- 4. And This is not a blank line
                                    -- So, the only thing left is that we have some fields that we didn't parse
                                    , Parser.succeed
                                        (Parser.Done
                                            (Err
                                                ( Nothing, MsgMissingFields (List.map Tuple.first fields.remaining) )
                                            )
                                        )
                                    ]
                            )

                Err unexpected ->
                    -- We've encountered an error, but we still need to parse
                    -- the entire indented block.  so that the parser can continue.
                    Parser.getIndent
                        |> Parser.andThen
                            (\indentation ->
                                Parser.succeed (Parser.Done fields.found)
                                    |. Parser.loop "" (raggedIndentedStringAbove (indentation - 4))
                            )


captureField found recordName fields fieldNames =
    Parser.map
        (\maybeField ->
            case maybeField of
                Nothing ->
                    Parser.Loop fields

                Just ( foundFieldname, fieldValue ) ->
                    case fieldValue of
                        Found _ _ ->
                            Parser.Loop
                                { found = Ok (( foundFieldname, fieldValue ) :: found)
                                , remaining =
                                    List.filter
                                        (\( fieldParserName, _ ) -> fieldParserName /= foundFieldname)
                                        fields.remaining
                                }

                        Unexpected unexpected ->
                            Parser.Loop
                                { found = Err ( Just unexpected.range, unexpected.problem )
                                , remaining =
                                    List.filter
                                        (\( fieldParserName, _ ) -> fieldParserName /= foundFieldname)
                                        fields.remaining
                                }
        )
        (Parser.oneOf
            (List.map (Parser.map Just << Tuple.second) fields.remaining
                ++ [ Parser.map Just (unexpectedField recordName fieldNames)
                   ]
            )
        )



{- RECORD RENDERER HELPERS -}


applyField : Found a -> Result UnexpectedDetails (a -> b) -> Result UnexpectedDetails b
applyField foundField possiblyFn =
    case possiblyFn of
        Err err ->
            Err err

        Ok fn ->
            case foundField of
                Found pos desc ->
                    Ok (fn desc)

                Unexpected unexpected ->
                    Err unexpected


getField : Field value -> List ( String, Found Description ) -> Result ProblemMessage (Found value)
getField (Field name fieldBlock) fields =
    List.foldl (matchField name fieldBlock) (Err (MsgMissingFields [ name ])) fields


matchField : String -> Block value -> ( String, Found Description ) -> Result ProblemMessage (Found value) -> Result ProblemMessage (Found value)
matchField targetName targetBlock ( name, foundDescription ) existing =
    case existing of
        Ok _ ->
            existing

        Err err ->
            if name == targetName then
                case foundDescription of
                    Found rng description ->
                        case renderBlock targetBlock description of
                            Ok rendered ->
                                Ok rendered

                            Err invalidAst ->
                                Err err

                    Unexpected unexpected ->
                        Ok (Unexpected unexpected)

            else
                existing



{- NESTED LIST HELPERS -}
{- Nested Lists -}


{-| = indentLevel icon space content
| indentLevel content

Where the second variation can only occur if the indentation is larger than the previous one.

A list item started with a list icon.

    If indent stays the same
    -> add to items at the current stack

    if ident increases
    -> create a new level in the stack

    if ident decreases
    -> close previous group
    ->

    <list>
        <*item>
            <txt> -> add to head sections
            <txt> -> add to head sections
            <item> -> add to head sections
            <item> -> add to head sections
                <txt> -> add to content
                <txt> -> add to content
                <item> -> add to content
                <item> -> add to content
            <item> -> add to content

        <*item>
        <*item>

    Section
        [ IconSection
            { icon = *
            , sections =
                [ Text
                , Text
                , IconSection Text
                , IconSection
                    [ Text
                    , Text
                    , item
                    , item
                    ]
                ]
            }
        , Icon -> Content
        , Icon -> Content
        ]

-}
type TreeBuilder item
    = TreeBuilder
        { previousIndent : Int
        , levels :
            -- (mostRecent :: remaining)
            List (Level item)
        }


{-| -}
type Level item
    = Level (List (Nested item))


emptyTreeBuilder : TreeBuilder item
emptyTreeBuilder =
    TreeBuilder
        { previousIndent = 0
        , levels = []
        }


reduceRender : (thing -> Result AstError (Found other)) -> List thing -> Result AstError (Result UnexpectedDetails (List other))
reduceRender fn list =
    List.foldl
        (\x gathered ->
            case gathered of
                Err _ ->
                    gathered

                Ok (Err unexpected) ->
                    gathered

                Ok (Ok remain) ->
                    case fn x of
                        Err err ->
                            Err err

                        Ok foundSuccess ->
                            case foundSuccess of
                                Found _ success ->
                                    Ok (Ok (success :: remain))

                                Unexpected unexpected ->
                                    Ok (Err unexpected)
        )
        (Ok (Ok []))
        list
        |> Result.map (Result.map List.reverse)


renderTreeNodeSmall :
    { item : Block item
    , start : Block icon
    }
    -> Nested ( Description, List Description )
    -> Result AstError (Found (Nested ( icon, List item )))
renderTreeNodeSmall config (Nested cursor) =
    let
        renderedChildren =
            reduceRender (renderTreeNodeSmall config) cursor.children

        renderedContent =
            case cursor.content of
                ( icon, content ) ->
                    ( renderBlock config.start icon
                    , reduceRender (renderBlock config.item) content
                    )
    in
    case renderedContent of
        ( Ok (Found pos icon), Ok (Ok content) ) ->
            case renderedChildren of
                Err err ->
                    Err err

                Ok (Ok successfullyRenderedChildren) ->
                    Ok
                        (Found pos <|
                            Nested
                                { content = ( icon, content )
                                , children = successfullyRenderedChildren
                                }
                        )

                Ok (Err unexpected) ->
                    Ok (Unexpected unexpected)

        ( Ok (Unexpected unexpected), _ ) ->
            Ok (Unexpected unexpected)

        ( _, Ok (Err unexpected) ) ->
            Ok (Unexpected unexpected)

        ( Err err, _ ) ->
            Err err

        ( _, Err err ) ->
            Err err


buildTree baseIndent items =
    let
        gather ( indentation, icon, item ) (TreeBuilder builder) =
            addItem (indentation - baseIndent) ( icon, item ) (TreeBuilder builder)

        groupByIcon ( indentation, maybeIcon, item ) maybeCursor =
            case maybeCursor of
                Nothing ->
                    case maybeIcon of
                        Just icon ->
                            Just
                                { indent = indentation
                                , icon = icon
                                , items = [ item ]
                                , accumulated = []
                                }

                        Nothing ->
                            -- Because of how the code runs, we have a tenuous guarantee that this branch won't execute.
                            -- Not entirely sure how to make the types work to eliminate this.
                            Nothing

                Just cursor ->
                    Just <|
                        case maybeIcon of
                            Nothing ->
                                { indent = cursor.indent
                                , icon = cursor.icon
                                , items = item :: cursor.items
                                , accumulated = cursor.accumulated
                                }

                            Just icon ->
                                { indent = indentation
                                , icon = icon
                                , items = [ item ]
                                , accumulated =
                                    ( cursor.indent, cursor.icon, cursor.items )
                                        :: cursor.accumulated
                                }

        finalizeGrouping maybeCursor =
            case maybeCursor of
                Nothing ->
                    []

                Just cursor ->
                    case cursor.items of
                        [] ->
                            cursor.accumulated

                        _ ->
                            ( cursor.indent, cursor.icon, cursor.items )
                                :: cursor.accumulated

        tree =
            items
                |> List.foldl groupByIcon Nothing
                |> finalizeGrouping
                |> List.reverse
                |> List.foldl gather emptyTreeBuilder
    in
    case tree of
        TreeBuilder builder ->
            renderLevels builder.levels


type alias NestedIndex =
    { base : Int
    , prev : Int
    }


{-| Results in a flattened version of the parsed list.

    ( 0, (), [ "item one" ] )

    ( 0, (), [ "item two" ] )

    ( 4, (), [ "nested item two", "additional text for nested item two" ] )

    ( 0, (), [ "item three" ] )

    ( 4, (), [ "nested item three" ] )

-}
indentedBlocksOrNewlines :
    Block icon
    -> Block thing
    -> ( NestedIndex, List ( Int, Maybe Description, Description ) )
    -> Parser Context Problem (Parser.Step ( NestedIndex, List ( Int, Maybe Description, Description ) ) (List ( Int, Maybe Description, Description )))
indentedBlocksOrNewlines icon item ( indentation, existing ) =
    Parser.oneOf
        [ Parser.end End
            |> Parser.map
                (\_ ->
                    Parser.Done (List.reverse existing)
                )

        -- Whitespace Line
        , skipBlankLineWith (Parser.Loop ( indentation, existing ))
        , case existing of
            [] ->
                -- Indent is already parsed by the block constructor for first element, skip it
                Parser.succeed
                    (\foundIcon foundBlock ->
                        let
                            newIndex =
                                { prev = indentation.base
                                , base = indentation.base
                                }
                        in
                        Parser.Loop ( newIndex, ( indentation.base, Just foundIcon, foundBlock ) :: existing )
                    )
                    |= getParser icon
                    |= getParser item

            _ ->
                Parser.oneOf
                    [ -- block with required indent
                      expectIndentation indentation.base indentation.prev
                        |> Parser.andThen
                            (\newIndent ->
                                -- If the indent has changed, then the delimiter is required
                                Parser.withIndent newIndent <|
                                    Parser.oneOf
                                        ((Parser.succeed
                                            (\iconResult itemResult ->
                                                let
                                                    newIndex =
                                                        { prev = newIndent
                                                        , base = indentation.base
                                                        }
                                                in
                                                Parser.Loop
                                                    ( newIndex
                                                    , ( newIndent, Just iconResult, itemResult ) :: existing
                                                    )
                                            )
                                            |= getParser icon
                                            |= getParser item
                                         )
                                            :: (if newIndent - 4 == indentation.prev then
                                                    [ getParser item
                                                        |> Parser.map
                                                            (\foundBlock ->
                                                                let
                                                                    newIndex =
                                                                        { prev = indentation.prev
                                                                        , base = indentation.base
                                                                        }
                                                                in
                                                                Parser.Loop
                                                                    ( newIndex
                                                                    , ( indentation.prev, Nothing, foundBlock ) :: existing
                                                                    )
                                                            )
                                                    ]

                                                else
                                                    []
                                               )
                                        )
                            )

                    -- We reach here because the indentation parsing was not successful,
                    -- This means any issues are handled by whatever parser comes next.
                    , Parser.succeed (Parser.Done (List.reverse existing))
                    ]
        ]


{-| We only expect nearby indentations.

We can't go below the `base` indentation.

Based on the previous indentation:

  - previous - 4
  - previous
  - previous + 4

If we don't match the above rules, we might want to count the mismatched number.

-}
expectIndentation : Int -> Int -> Parser Context Problem Int
expectIndentation base previous =
    Parser.succeed Tuple.pair
        |= Parser.oneOf
            ([ Parser.succeed (previous + 4)
                |. Parser.token (Parser.Token (String.repeat (previous + 4) " ") (ExpectingIndent (previous + 4)))
             , Parser.succeed previous
                |. Parser.token (Parser.Token (String.repeat previous " ") (ExpectingIndent previous))
             ]
                ++ descending base previous
            )
        |= Parser.getChompedString (Parser.chompWhile (\c -> c == ' '))
        |> Parser.andThen
            (\( indentLevel, extraSpaces ) ->
                if extraSpaces == "" then
                    Parser.succeed indentLevel

                else
                    Parser.problem
                        (ExpectingIndent (base + indentLevel))
            )


{-| Parse all indentation levels between `prev` and `base` in increments of 4.
-}
descending : Int -> Int -> List (Parser Context Problem Int)
descending base prev =
    if prev <= base then
        []

    else
        List.reverse
            (List.map
                (\x ->
                    let
                        level =
                            base + (x * 4)
                    in
                    Parser.succeed level
                        |. Parser.token (Parser.Token (String.repeat level " ") (ExpectingIndent level))
                )
                (List.range 0 (((prev - 4) - base) // 4))
            )


{-| A list item started with a list icon.

If indent stays the same
-> add to items at the current stack

if ident increases
-> create a new level in the stack

if ident decreases
-> close previous group
->

    1 Icon
        1.1 Content
        1.2 Icon
        1.3 Icon
           1.3.1 Icon

        1.4

    2 Icon

    Steps =
    []

    [ Level [ Item 1. [] ]
    ]

    [ Level [ Item 1.1 ]
    , Level [ Item 1. [] ]
    ]

    [ Level [ Item 1.2, Item 1.1 ]
    , Level [ Item 1. [] ]
    ]

    [ Level [ Item 1.3, Item 1.2, Item 1.1 ]
    , Level [ Item 1. [] ]
    ]

    [ Level [ Item 1.3.1 ]
    , Level [ Item 1.3, Item 1.2, Item 1.1 ]
    , Level [ Item 1. [] ]
    ]


    [ Level [ Item 1.4, Item 1.3([ Item 1.3.1 ]), Item 1.2, Item 1.1 ]
    , Level [ Item 1. [] ]
    ]

    [ Level [ Item 2., Item 1. (Level [ Item 1.4, Item 1.3([ Item 1.3.1 ]), Item 1.2, Item 1.1 ]) ]
    ]

-}
addItem :
    Int
    -> node
    -> TreeBuilder node
    -> TreeBuilder node
addItem indentation content (TreeBuilder builder) =
    let
        newItem =
            Nested
                { children = []
                , content = content
                }

        deltaLevel =
            indentation
                - builder.previousIndent

        addToLevel brandNewItem levels =
            case levels of
                [] ->
                    [ Level
                        [ brandNewItem ]
                    ]

                (Level lvl) :: remaining ->
                    Level (newItem :: lvl)
                        :: remaining
    in
    case builder.levels of
        [] ->
            TreeBuilder
                { previousIndent = indentation
                , levels =
                    [ Level
                        [ newItem ]
                    ]
                }

        (Level lvl) :: remaining ->
            if deltaLevel == 0 then
                -- add to current level
                TreeBuilder
                    { previousIndent = indentation
                    , levels =
                        Level (newItem :: lvl)
                            :: remaining
                    }

            else if deltaLevel > 0 then
                -- add new level
                TreeBuilder
                    { previousIndent = indentation
                    , levels =
                        Level [ newItem ]
                            :: Level lvl
                            :: remaining
                    }

            else
                -- We've dedented, so we need to first collapse the current level
                -- into the one below, then add an item to that level
                TreeBuilder
                    { previousIndent = indentation
                    , levels =
                        collapseLevel (abs deltaLevel // 4) builder.levels
                            |> addToLevel newItem
                    }


{-|

    1.
        1.1
    2.


    Steps =
    []

    [ Level [ Item 1. [] ]
    ]

    [ Level [ Item 1.1 ]
    , Level [ Item 1. [] ]
    ]

    -- collapse into lower level
    [ Level [ Item 1. [ Item 1.1 ] ]
    ]

    -- add new item
    [ Level [ Item 2, Item 1. [ Item 1.1 ] ]
    ]

-}
collapseLevel : Int -> List (Level item) -> List (Level item)
collapseLevel num levels =
    if num == 0 then
        levels

    else
        case levels of
            [] ->
                levels

            (Level topLevel) :: (Level ((Nested lowerItem) :: lower)) :: remaining ->
                collapseLevel (num - 1) <|
                    Level
                        (Nested
                            { lowerItem
                                | children = topLevel ++ lowerItem.children
                            }
                            :: lower
                        )
                        :: remaining

            _ ->
                levels


renderLevels levels =
    case levels of
        [] ->
            []

        _ ->
            case collapseLevel (List.length levels - 1) levels of
                [] ->
                    []

                (Level top) :: ignore ->
                    -- We just collapsed everything down to the top level.
                    List.foldl rev [] top


reverseTree (Nested nest) =
    Nested
        { content = Tuple.mapSecond List.reverse nest.content
        , children =
            List.foldl rev [] nest.children
        }


rev nest found =
    reverseTree nest :: found


{-| Replace a string with another string. This can be useful to have shortcuts to unicode characters.

For example, in `Mark.Default`, this is used to replace `...` with the unicode ellipses character: `…`.

-}
replacement : String -> String -> Replacement
replacement =
    Replacement


{-| A balanced replacement. This is used in `Mark.Default` to do auto-curly quotes.

    Mark.balanced
        { start = ( "\"", "“" )
        , end = ( "\"", "”" )
        }

-}
balanced :
    { start : ( String, String )
    , end : ( String, String )
    }
    -> Replacement
balanced =
    Balanced



{- TEXT HELPERS -}


{-| -}
type TextCursor
    = TextCursor
        { current : Text
        , start : Position
        , found : List TextDescription
        , balancedReplacements : List String
        }


styledTextParser :
    { inlines : List InlineExpectation
    , replacements : List Replacement
    }
    -> Position
    -> List Style
    -> List Char
    -> Parser Context Problem Description
styledTextParser options startingPos inheritedStyles until =
    let
        vacantText =
            TextCursor
                { current = Text inheritedStyles ""
                , found = []
                , start = startingPos
                , balancedReplacements = []
                }

        untilStrings =
            List.map String.fromChar until

        meaningful =
            '\\' :: '\n' :: until ++ stylingChars ++ replacementStartingChars options.replacements
    in
    Parser.oneOf
        [ -- Parser.chompIf (\c -> c == ' ') CantStartTextWithSpace
          -- -- TODO: return error description
          -- |> Parser.andThen
          --     (\_ ->
          --         Parser.problem CantStartTextWithSpace
          --     )
          Parser.map
            (\( pos, textNodes ) ->
                DescribeText pos textNodes
            )
            (withRange
                (Parser.loop vacantText
                    (styledTextParserLoop options meaningful untilStrings)
                )
            )
        ]


empty : Text
empty =
    Text [] ""


{-| -}
styledTextParserLoop :
    { inlines : List InlineExpectation
    , replacements : List Replacement
    }
    -> List Char
    -> List String
    -> TextCursor
    -> Parser Context Problem (Parser.Step TextCursor (List TextDescription))
styledTextParserLoop options meaningful untilStrings found =
    Parser.oneOf
        [ Parser.oneOf (replace options.replacements found)
            |> Parser.map Parser.Loop

        -- If a char matches the first character of a replacement,
        -- but didn't match the full replacement captured above,
        -- then stash that char.
        , Parser.oneOf (almostReplacement options.replacements found)
            |> Parser.map Parser.Loop

        -- Capture style command characters
        , Parser.succeed
            (Parser.Loop << changeStyle options found)
            |= Parser.oneOf
                [ Parser.map (always (Just Italic)) (Parser.token (Parser.Token "/" (Expecting "/")))
                , Parser.map (always (Just Strike)) (Parser.token (Parser.Token "~" (Expecting "~")))
                , Parser.map (always (Just Bold)) (Parser.token (Parser.Token "*" (Expecting "*")))
                ]

        -- Custom inline block
        , Parser.succeed
            (\start maybeRendered end ->
                let
                    current =
                        case changeStyle options found Nothing of
                            TextCursor accum ->
                                accum
                in
                Parser.Loop
                    (TextCursor
                        { found =
                            case maybeRendered of
                                Nothing ->
                                    UnexpectedInline
                                        { range =
                                            { start = start
                                            , end = end
                                            }
                                        , problem =
                                            MsgUnknownInline options.inlines
                                        }
                                        :: current.found

                                Just rendered ->
                                    rendered :: current.found
                        , start = end

                        -- TODO: This should inherit formatting from the inline parser
                        , current = empty
                        , balancedReplacements = current.balancedReplacements
                        }
                    )
            )
            |= getPosition
            |. Parser.token
                (Parser.Token "{" InlineStart)
            |= Parser.oneOf
                [ Parser.succeed
                    (\maybeInlineResult hasEnd ->
                        if hasEnd then
                            maybeInlineResult

                        else
                            Nothing
                    )
                    |= Parser.oneOf
                        (List.map
                            (\(InlineExpectation inlineName inlineComponents) ->
                                Parser.inContext
                                    (InInline inlineName)
                                    (parseInline inlineName inlineComponents)
                            )
                            options.inlines
                        )
                    |= Parser.oneOf
                        [ Parser.map (always True) (Parser.token (Parser.Token "}" InlineEnd))
                        , Parser.succeed False
                            |. parseTillEnd
                        ]
                , Parser.succeed Nothing
                    |. parseTillEnd
                ]
            |= getPosition
        , -- chomp until a meaningful character
          Parser.chompWhile
            (\c ->
                not (List.member c meaningful)
            )
            |> Parser.getChompedString
            |> Parser.andThen
                (\new ->
                    if new == "" || new == "\n" then
                        case changeStyle options found Nothing of
                            TextCursor txt ->
                                let
                                    styling =
                                        case txt.current of
                                            Text s _ ->
                                                s
                                in
                                -- TODO: What to do on unclosed styling?
                                -- if List.isEmpty styling then
                                Parser.succeed (Parser.Done (List.reverse txt.found))
                        -- else
                        -- Parser.problem (UnclosedStyles styling)

                    else
                        Parser.succeed (Parser.Loop (addText new found))
                )
        ]


parseInline : String -> List InlineValueExpectation -> Parser Context Problem (Maybe TextDescription)
parseInline name components =
    case components of
        [] ->
            Parser.succeed (\( range, _ ) -> Just (DescribeInline name range []))
                |= withRange (Parser.keyword (Parser.Token name (ExpectingInlineName name)))

        _ ->
            Parser.succeed
                (\( range, maybeFoundComponents ) ->
                    case maybeFoundComponents of
                        Nothing ->
                            Nothing

                        Just foundComponents ->
                            Just (DescribeInline name range foundComponents)
                )
                |= withRange
                    (Parser.succeed identity
                        |. Parser.keyword (Parser.Token name (ExpectingInlineName name))
                        |. Parser.chompWhile (\c -> c == ' ')
                        |= Parser.loop ( components, [] ) parseInlineComponents
                    )


parseInlineComponents :
    ( List InlineValueExpectation, List InlineDescription )
    -> Parser Context Problem (Parser.Step ( List InlineValueExpectation, List InlineDescription ) (Maybe (List InlineDescription)))
parseInlineComponents ( components, found ) =
    case components of
        [] ->
            Parser.succeed (Parser.Done (Just (List.reverse found)))

        current :: remaining ->
            -- Returning `Nothing` will return an Error describing what this inline should look like.
            -- When `Nothing` is returned, the parser needs to consume everything until either `}` or `\n`
            case current of
                -- If `|` fails -> Nothing
                -- if Keyword fails -> Nothing
                -- if `=` fails -> Nothing
                ExpectInlineString inlineName ->
                    Parser.oneOf
                        [ (Parser.succeed
                            (\start hasName hasEquals ->
                                if hasName && hasEquals then
                                    -- Parser.Loop
                                    --     ( remaining
                                    --     , DescribeInlineString
                                    --         inlineName
                                    --         { start = start
                                    --         , end = end
                                    --         }
                                    --         str
                                    --         :: found
                                    --     )
                                    ( True, start )

                                else
                                    ( False, start )
                            )
                            |. Parser.chompIf (\c -> c == '|') (Expecting "|")
                            |. Parser.chompWhile (\c -> c == ' ')
                            |= getPosition
                            |= Parser.oneOf
                                [ Parser.map (always True)
                                    (Parser.keyword
                                        (Parser.Token
                                            inlineName
                                            (ExpectingFieldName inlineName)
                                        )
                                    )
                                , Parser.succeed False
                                ]
                            |. Parser.chompWhile (\c -> c == ' ')
                            |= Parser.oneOf
                                [ Parser.map (always True) (Parser.chompIf (\c -> c == '=') (Expecting "="))
                                , Parser.succeed False
                                ]
                          )
                            |> Parser.andThen
                                (\( continue, start ) ->
                                    if continue then
                                        Parser.succeed
                                            (\str end ->
                                                Parser.Loop
                                                    ( remaining
                                                    , DescribeInlineString
                                                        inlineName
                                                        { start = start
                                                        , end = end
                                                        }
                                                        str
                                                        :: found
                                                    )
                                            )
                                            |. Parser.chompWhile (\c -> c == ' ')
                                            |= Parser.getChompedString
                                                (Parser.chompWhile (\c -> c /= '|' && c /= '}' && c /= '\n'))
                                            |= getPosition

                                    else
                                        Parser.succeed
                                            (\end ->
                                                Parser.Done Nothing
                                            )
                                            |. parseTillEnd
                                            |= getPosition
                                )
                        , Parser.succeed
                            (\start end ->
                                Parser.Done Nothing
                            )
                            |= getPosition
                            |. parseTillEnd
                            |= getPosition
                        ]

                ExpectInlineText ->
                    -- if `|` fails,
                    -- parseInline Text cannot fail.  (though we could make it fail on unclosed formatting.)
                    Parser.oneOf
                        [ Parser.succeed
                            (\start str end ->
                                Parser.Loop
                                    ( remaining
                                    , DescribeInlineText
                                        { start = start
                                        , end = end
                                        }
                                        str
                                        :: found
                                    )
                            )
                            |. Parser.chompIf (\c -> c == '|') (Expecting "|")
                            |. Parser.chompWhile (\c -> c == ' ')
                            |= getPosition
                            |= Parser.loop [] (parseInlineText { replacements = [] })
                            |= getPosition
                        , Parser.succeed
                            (\start end ->
                                Parser.Done Nothing
                            )
                            |= getPosition
                            |. parseTillEnd
                            |= getPosition
                        ]


parseTillEnd =
    Parser.succeed
        (\str endsWithBracket ->
            endsWithBracket
        )
        |= Parser.chompWhile (\c -> c /= '\n' && c /= '}')
        |= Parser.oneOf
            [ Parser.map (always True) (Parser.token (Parser.Token "}" InlineEnd))
            , Parser.succeed False
            ]


{-| -}
parseInlineText :
    { replacements : List Replacement
    }
    -> List Text
    -> Parser Context Problem (Parser.Step (List Text) (List Text))
parseInlineText options found =
    Parser.oneOf
        [ --     Parser.oneOf (replace options.replacements found)
          --     |> Parser.map Parser.Loop
          -- -- If a char matches the first character of a replacement,
          -- -- but didn't match the full replacement captured above,
          -- -- then stash that char.
          -- , Parser.oneOf (almostReplacement options.replacements found)
          --     |> Parser.map Parser.Loop
          -- Capture style command characters
          Parser.succeed
            (Parser.Loop << changeStyleOnText found)
            |= Parser.oneOf
                [ Parser.map (always Italic) (Parser.token (Parser.Token "/" (Expecting "/")))

                -- , Parser.map (always rline)) (Parser.token (Parser.Token "_" (Expecting "_")))
                , Parser.map (always Strike) (Parser.token (Parser.Token "~" (Expecting "~")))
                , Parser.map (always Bold) (Parser.token (Parser.Token "*" (Expecting "*")))

                -- , Parser.map (always (Just Code)) (Parser.token (Parser.Token "`" (Expecting "`")))
                ]
        , -- chomp until a meaningful character
          Parser.chompWhile
            (\c ->
                not (List.member c [ '}', '/', '|', '*', '~', '\n' ])
            )
            |> Parser.getChompedString
            |> Parser.andThen
                (\new ->
                    if new == "" || new == "\n" then
                        -- TODO: Warning about unclosed styles
                        Parser.succeed (Parser.Done (List.reverse found))

                    else
                        Parser.succeed (Parser.Loop (addTextToText new found))
                )
        ]


{-| -}
almostReplacement : List Replacement -> TextCursor -> List (Parser Context Problem TextCursor)
almostReplacement replacements existing =
    let
        captureChar char =
            Parser.succeed
                (\c ->
                    addText c existing
                )
                |= Parser.getChompedString
                    (Parser.chompIf (\c -> c == char && char /= '{' && char /= '*' && char /= '/') EscapedChar)

        first repl =
            case repl of
                Replacement x y ->
                    firstChar x

                Balanced range ->
                    firstChar (Tuple.first range.start)

        allFirstChars =
            List.filterMap first replacements
    in
    List.map captureChar allFirstChars


{-| **Reclaimed typography**

This function will replace certain characters with improved typographical ones.
Escaping a character will skip the replacement.

    -> "<>" -> a non-breaking space.
        - This can be used to glue words together so that they don't break
        - It also avoids being used for spacing like `&nbsp;` because multiple instances will collapse down to one.
    -> "--" -> "en-dash"
    -> "---" -> "em-dash".
    -> Quotation marks will be replaced with curly quotes.
    -> "..." -> ellipses

-}
replace : List Replacement -> TextCursor -> List (Parser Context Problem TextCursor)
replace replacements existing =
    let
        -- Escaped characters are captured as-is
        escaped =
            Parser.succeed
                (\esc ->
                    existing
                        |> addText esc
                )
                |. Parser.token
                    (Parser.Token "\\" Escape)
                |= Parser.getChompedString
                    (Parser.chompIf (always True) EscapedChar)

        replaceWith repl =
            case repl of
                Replacement x y ->
                    Parser.succeed
                        (\_ ->
                            addText y existing
                        )
                        |. Parser.token (Parser.Token x (Expecting x))
                        |= Parser.succeed ()

                Balanced range ->
                    let
                        balanceCache =
                            case existing of
                                TextCursor cursor ->
                                    cursor.balancedReplacements

                        id =
                            balanceId range
                    in
                    -- TODO: implement range replacement
                    if List.member id balanceCache then
                        case range.end of
                            ( x, y ) ->
                                Parser.succeed
                                    (addText y existing
                                        |> removeBalance id
                                    )
                                    |. Parser.token (Parser.Token x (Expecting x))

                    else
                        case range.start of
                            ( x, y ) ->
                                Parser.succeed
                                    (addText y existing
                                        |> addBalance id
                                    )
                                    |. Parser.token (Parser.Token x (Expecting x))
    in
    escaped :: List.map replaceWith replacements


balanceId balance =
    let
        join ( x, y ) =
            x ++ y
    in
    join balance.start ++ join balance.end


stylingChars =
    [ '~'

    -- , '_'
    , '/'
    , '*'
    , '\n'
    , '{'

    -- , '`'
    ]


firstChar str =
    case String.uncons str of
        Nothing ->
            Nothing

        Just ( fst, _ ) ->
            Just fst


replacementStartingChars replacements =
    let
        first repl =
            case repl of
                Replacement x y ->
                    firstChar x

                Balanced range ->
                    firstChar (Tuple.first range.start)
    in
    List.filterMap first replacements


addBalance id (TextCursor cursor) =
    TextCursor <|
        { cursor | balancedReplacements = id :: cursor.balancedReplacements }


removeBalance id (TextCursor cursor) =
    TextCursor <|
        { cursor | balancedReplacements = List.filter ((/=) id) cursor.balancedReplacements }


addTextToText newString textNodes =
    case textNodes of
        [] ->
            [ Text [] newString ]

        (Text styles txt) :: remaining ->
            Text styles (txt ++ newString) :: remaining


addText newTxt (TextCursor cursor) =
    case cursor.current of
        Text styles txt ->
            TextCursor { cursor | current = Text styles (txt ++ newTxt) }


changeStyle options (TextCursor cursor) maybeStyleToken =
    let
        cursorText =
            case cursor.current of
                Text _ txt ->
                    txt

        newText =
            case maybeStyleToken of
                Nothing ->
                    cursor.current

                Just sty ->
                    case sty of
                        Bold ->
                            flipStyle Bold cursor.current

                        Italic ->
                            flipStyle Italic cursor.current

                        Strike ->
                            flipStyle Strike cursor.current
    in
    if cursorText == "" then
        TextCursor
            { found = cursor.found
            , current = newText
            , start = cursor.start
            , balancedReplacements = cursor.balancedReplacements
            }

    else
        let
            end =
                measure cursor.start cursorText
        in
        TextCursor
            { found =
                Styled
                    { start = cursor.start
                    , end = end
                    }
                    cursor.current
                    :: cursor.found
            , start = end
            , current = newText
            , balancedReplacements = cursor.balancedReplacements
            }


changeStyleOnText textNodes styleToken =
    case textNodes of
        [] ->
            case styleToken of
                Bold ->
                    [ Text [ Bold ] "" ]

                Italic ->
                    [ Text [ Italic ] "" ]

                Strike ->
                    [ Text [ Strike ] "" ]

        current :: remaining ->
            let
                newText =
                    case styleToken of
                        Bold ->
                            flipStyle Bold current

                        Italic ->
                            flipStyle Italic current

                        Strike ->
                            flipStyle Strike current
            in
            newText :: current :: remaining


flipStyle newStyle textStyle =
    case textStyle of
        Text styles str ->
            if List.member newStyle styles then
                Text (List.filter ((/=) newStyle) styles) ""

            else
                Text (newStyle :: styles) ""


measure start textStr =
    let
        len =
            String.length textStr
    in
    { start
        | offset = start.offset + len
        , column = start.column + len
    }



{- Error Rendering -}


renderError : String -> UnexpectedDetails -> ErrorMessage
renderError source current =
    case current.problem of
        MsgDocumentMismatch ->
            { title = "DOCUMENT MISMATCH"
            , region =
                current.range
            , message =
                [ Format.text "Your "
                , Format.yellow (Format.text "document")
                , Format.text " and your "
                , Format.yellow (Format.text "Parsed")
                , Format.text " structure don't match for some reason.\n\n"
                , Format.text "This usually occurs because you've stored the "
                , Format.yellow (Format.text "Parsed")
                , Format.text " data somewhere and then made a breaking change to your document."
                ]
            }

        MsgParsingIssue issues ->
            { title = "PARSING ISSUE"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I ran into an issue parsing your document.\n\n" ]
                    , renderParserIssue issues
                    , [ Format.text "\n\n" ]
                    ]
            }

        MsgUnknownBlock expecting ->
            let
                target =
                    String.slice current.range.start.offset current.range.end.offset source
            in
            { title = "UNKNOWN BLOCK"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I don't recognize this block name.\n\n" ]
                    , highlight current.range source
                    , [ Format.text "Do you mean one of these instead?\n\n"
                      , expecting
                            |> List.sortBy (\exp -> 0 - similarity target exp)
                            |> List.map (addIndent 4)
                            |> String.join "\n"
                            |> Format.text
                            |> Format.yellow
                      ]
                    ]
            }

        MsgUnknownInline expecting ->
            let
                target =
                    String.slice current.range.start.offset current.range.end.offset source
            in
            { title = "UNKNOWN INLINE"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I ran into an unexpected inline name.\n\n" ]
                    , highlight current.range source
                    , [ Format.text "But I was expecting one of these instead:\n\n"
                      , expecting
                            |> List.map getInlineName
                            |> List.sortBy (\exp -> 0 - similarity target exp)
                            |> List.map (addIndent 4)
                            |> String.join "\n"
                            |> Format.text
                            |> Format.yellow
                      ]
                    ]
            }

        MsgExpectingIndent indentation ->
            { title = "MISMATCHED INDENTATION"
            , region = current.range
            , message =
                [ Format.text ("I was expecting " ++ String.fromInt indentation ++ " spaces of indentation.\n\n")
                ]
                    ++ highlight current.range source
                    ++ hint "All indentation in `elm-markup` is a multiple of 4."
            }

        MsgCantStartTextWithSpace ->
            let
                line =
                    String.slice current.range.start.offset current.range.end.offset source
            in
            { title = "TOO MUCH SPACE"
            , region = current.range
            , message =
                List.concat
                    [ [ Format.text "This line of text starts with extra space.\n\n" ]
                    , highlight current.range source
                    , [ Format.text "Beyond the required indentation, text should start with non-whitespace characters." ]
                    ]
            }

        MsgUnclosedStyle styles ->
            let
                line =
                    String.slice current.range.start.offset current.range.end.offset source
            in
            { title = "UNCLOSED STYLE"
            , region = current.range
            , message =
                List.concat
                    [ [ Format.text (styleNames styles ++ " still open.  Add " ++ String.join " and " (List.map styleChars styles) ++ " to close it.\n\n") ] ]
                    ++ highlight current.range source
                    ++ hint "`*` is used for bold and `/` is used for italic."
            }

        MsgUnexpectedField msgUnexpectedField ->
            let
                target =
                    String.slice current.range.start.offset current.range.end.offset source
            in
            { title = "UNKNOWN FIELD"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I ran into an unexpected field name for a "
                      , Format.text msgUnexpectedField.recordName
                            |> Format.yellow
                      , Format.text " record\n\n"
                      ]
                    , highlight current.range source
                    , [ Format.text "\nDo you mean one of these instead?\n\n"
                      , msgUnexpectedField.options
                            |> List.sortBy (\exp -> 0 - similarity target exp)
                            |> List.map (addIndent 4)
                            |> String.join "\n"
                            |> Format.text
                            |> Format.yellow
                      ]
                    ]
            }

        MsgBadDate found ->
            { title = "BAD DATE"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I was trying to parse a date, but this format looks off.\n\n" ]
                    , highlight current.range source
                    , [ Format.text "Dates should be in ISO 8601 format:\n\n"
                      , Format.text (addIndent 4 "YYYY-MM-DDTHH:mm:ss.SSSZ")
                            |> Format.yellow
                      ]
                    ]
            }

        MsgBadFloat found ->
            { title = "BAD FLOAT"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I was trying to parse a float, but this format looks off.\n\n" ]
                    , highlight current.range source
                    ]
            }

        MsgBadInt found ->
            { title = "BAD INT"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I was trying to parse an integer, but this format looks off.\n\n" ]
                    , highlight current.range source
                    ]
            }

        MsgBadBool found ->
            { title = "BAD INT"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I was trying to parse a boolean, but this format looks off.\n\n" ]
                    , highlight current.range source
                    ]
            }

        MsgIntOutOfRange found ->
            { title = "INTEGER OUT OF RANGE"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I was expecting an "
                      , Format.yellow (Format.text "Int")
                      , Format.text " between "
                      , Format.text (String.fromInt found.min)
                            |> Format.yellow
                      , Format.text " and "
                      , Format.text (String.fromInt found.max)
                            |> Format.yellow
                      , Format.text ", but found:\n\n"
                      ]
                    , highlight current.range source
                    ]
            }

        MsgFloatOutOfRange found ->
            { title = "FLOAT OUT OF RANGE"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I was expecting a "
                      , Format.yellow (Format.text "Float")
                      , Format.text " between "
                      , Format.text (String.fromFloat found.min)
                            |> Format.yellow
                      , Format.text " and "
                      , Format.text (String.fromFloat found.max)
                            |> Format.yellow
                      , Format.text ", but found:\n\n"
                      ]
                    , highlight current.range source
                    ]
            }

        MsgNonMatchingFields fields ->
            let
                line =
                    String.slice current.range.start.offset current.range.end.offset source

                remaining =
                    List.filter
                        (\f -> not <| List.member f fields.found)
                        fields.expecting
            in
            { title = "MISSING FIELD"
            , region = current.range
            , message =
                -- TODO: Highlight entire record section
                -- TODO: mention record name
                case remaining of
                    [] ->
                        -- TODO: This should never happen actually.
                        --  Maybe error should be a nonempty list?
                        [ Format.text "It looks like a field is missing." ]

                    [ single ] ->
                        [ Format.text "It looks like a field is missing.\n\n"
                        , Format.text "You need to add the "
                        , Format.yellow (Format.text single)
                        , Format.text " field."
                        ]

                    multiple ->
                        [ Format.text "It looks like a field is missing.\n\n"
                        , Format.text "You still need to add:\n"
                        , remaining
                            |> List.sortBy (\exp -> 0 - similarity line exp)
                            |> List.map (addIndent 4)
                            |> String.join "\n"
                            |> Format.text
                            |> Format.yellow
                        ]
            }

        MsgMissingFields remaining ->
            let
                line =
                    String.slice current.range.start.offset current.range.end.offset source
            in
            { title = "MISSING FIELD"
            , region = current.range
            , message =
                -- TODO: Highlight entire record section
                -- TODO: mention record name
                case remaining of
                    [] ->
                        -- TODO: This should never happen actually.
                        --  Maybe error should be a nonempty list?
                        [ Format.text "It looks like a field is missing." ]

                    [ single ] ->
                        [ Format.text "It looks like a field is missing.\n\n"
                        , Format.text "You need to add the "
                        , Format.yellow (Format.text single)
                        , Format.text " field."
                        ]

                    multiple ->
                        [ Format.text "It looks like a field is missing.\n\n"
                        , Format.text "You still need to add:\n"
                        , remaining
                            |> List.sortBy (\exp -> 0 - similarity line exp)
                            |> List.map (addIndent 4)
                            |> String.join "\n"
                            |> Format.text
                            |> Format.yellow
                        ]
            }


renderParserIssue deadends =
    List.map
        (Format.yellow
            << Format.text
            << addIndent 4
            << parsingProblemToString
            << .problem
        )
        deadends


parsingProblemToString prob =
    case prob of
        ExpectingIndent i ->
            "ExpectingIndent i" ++ "\n"

        InlineStart ->
            "InlineStart\n"

        InlineEnd ->
            "InlineEnd\n"

        BlockStart ->
            "BlockStart\n"

        Expecting str ->
            "Expecting " ++ str ++ "\n"

        ExpectingBlockName name ->
            "ExpectingBlockName " ++ name ++ "\n"

        ExpectingInlineName name ->
            "ExpectingInlineName " ++ name ++ "\n"

        ExpectingFieldName name ->
            "ExpectingFieldName " ++ name ++ "\n"

        Escape ->
            "Escape\n"

        EscapedChar ->
            "EscapedChar\n"

        Newline ->
            "Newline\n"

        Space ->
            "Space\n"

        End ->
            "End\n"

        Integer ->
            "Integer\n"

        FloatingPoint ->
            "FloatingPoint\n"

        InvalidNumber ->
            "InvalidNumber\n"


formatNewline =
    { text = "\n"
    , color = Nothing
    , underline = False
    , bold = False
    }



-- highlight : Range -> String -> List ()


highlight range source =
    if range.start.line == range.end.line then
        -- single line
        let
            lineStart =
                range.start.offset - (range.start.column - 1)

            line =
                String.slice lineStart (range.end.offset + 20) source
                    |> String.lines
                    |> List.head
                    |> Maybe.withDefault ""

            lineNumber =
                String.fromInt range.start.line
                    ++ (if String.startsWith "|" line then
                            ""

                        else
                            "|"
                       )
        in
        [ Format.text (lineNumber ++ line ++ "\n")
        , Format.red
            (Format.text
                (String.repeat
                    (range.start.column - 1 + String.length lineNumber)
                    " "
                    ++ String.repeat (range.end.column - range.start.column) "^"
                    ++ "\n"
                )
            )
        ]

    else
        -- multiline
        let
            snippet =
                String.slice range.start.offset range.end.offset source

            indented =
                String.slice (range.start.offset + 1 - range.start.column) range.start.offset source

            lines =
                String.lines (indented ++ snippet)
                    |> List.indexedMap
                        (\i str ->
                            ( i + range.start.line - 1, str )
                        )
        in
        List.concatMap highlightLine lines


highlightLine ( index, line ) =
    [ Format.text (String.fromInt index)
    , Format.red (Format.text ">")
    , Format.text (line ++ "\n")
    ]


highlightPreviousWord cursor line =
    let
        rowNumLength =
            String.length (String.fromInt cursor.row)

        start =
            String.length line - String.length (String.trimLeft line)

        highlightLength =
            line
                |> String.dropRight (String.length line - (cursor.col - 2))
                |> String.trimLeft
                |> String.length
    in
    Format.red <| Format.text (String.repeat (rowNumLength + start + 1) " " ++ String.repeat highlightLength "^" ++ "\n")


highlightWord cursor line =
    let
        rowNumLength =
            String.length (String.fromInt cursor.row)

        highlightLength =
            line
                |> String.dropLeft (cursor.col - rowNumLength)
                |> String.words
                |> List.head
                |> Maybe.map String.length
                |> Maybe.withDefault 1
    in
    Format.red <| Format.text (String.repeat (rowNumLength + cursor.col - 1) " " ++ String.repeat highlightLength "^" ++ "\n")


addIndent x str =
    String.repeat x " " ++ str


singleLine row line =
    Format.text <|
        String.fromInt row
            ++ (if String.startsWith "|" line then
                    ""

                else
                    "|"
               )
            ++ line


hint str =
    [ Format.text "Hint"
        |> Format.underline
    , Format.text (": " ++ str)
    ]


getLine row lines =
    case List.head (List.drop (row - 1) lines) of
        Nothing ->
            "Empty"

        Just l ->
            l


highlightUntil end cursor line =
    let
        rowNumLength =
            String.length (String.fromInt cursor.row)

        highlightLength =
            line
                |> String.dropLeft (cursor.col - rowNumLength)
                |> String.split (String.fromChar end)
                |> List.head
                |> Maybe.map (\str -> String.length str + 1)
                |> Maybe.withDefault 1
    in
    Format.red <| Format.text (String.repeat (rowNumLength + cursor.col - 1) " " ++ String.repeat highlightLength "^" ++ "\n")


getWord cursor line =
    let
        rowNumLength =
            String.length (String.fromInt cursor.row)

        highlightLength =
            line
                |> String.dropLeft (cursor.col - rowNumLength)
                |> String.words
                |> List.head
                |> Maybe.map String.length
                |> Maybe.withDefault 1

        end =
            cursor.col + highlightLength
    in
    String.slice (cursor.col - 1) end line


getPrevWord cursor line =
    let
        start =
            String.length line - String.length (String.trimLeft line)

        highlightLength =
            line
                |> String.dropRight (String.length line - (cursor.col - 2))
                |> String.trimLeft
                |> String.length
    in
    String.slice start (start + highlightLength) line


type alias Similarity =
    Int


similarity : String -> String -> Similarity
similarity source target =
    let
        lenSimilarity =
            0 - min 2 (abs (String.length source - String.length target))

        addCompared ( x, y ) total =
            if x == y then
                total + 1

            else
                total
    in
    List.map2 Tuple.pair (String.toList source) (String.toList target)
        |> List.foldl addCompared 0
        |> (+) lenSimilarity


highlightSpace col line =
    let
        start =
            String.dropLeft (col - 1) line

        trimmed =
            String.trimLeft start

        highlightLength =
            String.length start
                - String.length trimmed
                |> max 1
    in
    Format.red <| Format.text (" " ++ String.repeat col " " ++ String.repeat highlightLength "^" ++ "\n")


styleChars style =
    case style of
        Bold ->
            "*"

        Italic ->
            "/"

        Strike ->
            "~"


styleNames styles =
    let
        italic =
            List.any ((==) Italic) styles

        isBold =
            List.any ((==) Bold) styles

        strike =
            List.any ((==) Strike) styles
    in
    case ( italic, isBold, strike ) of
        ( False, False, False ) ->
            "Some formatting is"

        ( True, True, False ) ->
            "Italic and bold formatting are"

        ( True, True, True ) ->
            "Italic, strike, and bold formatting are"

        ( True, False, True ) ->
            "Italic and strike formatting are"

        ( False, True, True ) ->
            "Strike, and bold formatting are"

        ( True, False, False ) ->
            "Italic formatting is"

        ( False, True, False ) ->
            "Bold formatting is"

        ( False, False, True ) ->
            "Strike formatting is"


{-| -}
errorToString : ErrorMessage -> String
errorToString msg =
    formatErrorString msg


formatErrorString error =
    String.toUpper error.title
        ++ "\n\n"
        ++ String.join "" (List.map .text error.message)


type Theme
    = Dark
    | Light


{-| -}
errorToHtml : Theme -> ErrorMessage -> List (Html.Html msg)
errorToHtml theme error =
    formatErrorHtml theme error


formatErrorHtml theme error =
    Html.span [ Html.Attributes.style "color" (foregroundClr theme) ]
        [ Html.text
            (String.toUpper error.title
                ++ "\n\n"
            )
        ]
        :: List.map (renderMessageHtml theme) error.message


foregroundClr theme =
    case theme of
        Dark ->
            "#eeeeec"

        Light ->
            "rgba(16,16,16, 0.9)"


renderMessageHtml theme message =
    Html.span
        (List.filterMap identity
            [ if message.bold then
                Just (Html.Attributes.style "font-weight" "bold")

              else
                Nothing
            , if message.underline then
                Just (Html.Attributes.style "text-decoration" "underline")

              else
                Nothing
            , case message.color of
                Nothing ->
                    Just <| Html.Attributes.style "color" (foregroundClr theme)

                Just "red" ->
                    Just <| Html.Attributes.style "color" (redClr theme)

                Just "yellow" ->
                    Just <| Html.Attributes.style "color" (yellowClr theme)

                _ ->
                    Nothing
            ]
        )
        [ Html.text message.text ]


redClr theme =
    case theme of
        Dark ->
            "#ef2929"

        Light ->
            "#cc0000"


yellowClr theme =
    case theme of
        Dark ->
            "#edd400"

        Light ->
            "#c4a000"
