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
    , update, Edit, updateFloat, updateInt, updateString, replaceOneOf, deleteBlock, insertAt, prove, Proved(..)
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

@docs update, Edit, updateFloat, updateInt, updateString, replaceOneOf, deleteBlock, insertAt, prove, Proved

-}

import Html
import Html.Attributes
import Iso8601
import Mark.Format as Format
import Mark.Internal.Error as Error exposing (Context(..), Problem(..))
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
                Error.render
                    source
                    { problem = Error.ParsingIssue deadEnds
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
                                (Error.render source
                                    { problem = Error.DocumentMismatch
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
                                (Error.render source
                                    { problem = Error.DocumentMismatch
                                    , range = startDocRange
                                    }
                                )

        Err deadEnds ->
            Failure <|
                Error.render
                    source
                    { problem = Error.ParsingIssue deadEnds
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
                        (Error.render ""
                            { problem = Error.DocumentMismatch
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
                        (Error.render ""
                            { problem = Error.DocumentMismatch
                            , range = startDocRange
                            }
                        )



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


move =
    Move


type Id category
    = Id Range


{-| -}
getRange : Id anything -> Range
getRange (Id range) =
    range


{-| -}
manyOptionId : Range -> Id ManyOptions
manyOptionId =
    Id


{-| -}
optionId : Range -> Id Options
optionId =
    Id


type ManyOptions
    = ManyOptions


type Options
    = Options



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
                                        ( newPos, newDesc ) =
                                            create i pos expectation
                                    in
                                    replaceOption id newDesc desc
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
                                \i pos desc ->
                                    let
                                        ( newPos, newDesc ) =
                                            create i pos expectation
                                    in
                                    insertAtIndex id index newDesc desc
                            , indentation = 0
                            }
                            original.found
                }

        DeleteBlock id index ->
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


{-| -}
createField : Int -> ( String, Expectation ) -> ( Position, List ( String, Found Description ) ) -> ( Position, List ( String, Found Description ) )
createField currentIndent ( name, exp ) ( base, existingFields ) =
    let
        ( end, childField ) =
            create (currentIndent + 1)
                (base
                    |> moveColumn ((currentIndent + 1) * 4)
                )
                exp
    in
    ( moveNewline end
    , ( name
      , Found
            { start = base
            , end = end
            }
            childField
      )
        :: existingFields
    )


{-|

    `Position` is the starting position for a block.

    The same rules for indentaition as they apply everywhere.

        - Primitives do not handle their indentation.
        - Block, Record, and Tree elements handle the indentation of their children.

-}
create : Int -> Position -> Expectation -> ( Position, Description )
create currentIndent base expectation =
    case expectation of
        ExpectBlock name childExpectation ->
            let
                ( end, childDescription ) =
                    create (currentIndent + 1)
                        (base
                            |> moveNewline
                            |> moveColumn ((currentIndent + 1) * 4)
                        )
                        childExpectation
            in
            ( moveNewline end
            , DescribeBlock
                { name = name
                , found =
                    Found
                        { start = base
                        , end = end
                        }
                        childDescription
                , expected = expectation
                }
            )

        ExpectStub name ->
            let
                end =
                    moveColumn (String.length name) base
            in
            ( moveNewline end
            , DescribeStub name
                (Found
                    { start = base
                    , end = end
                    }
                    name
                )
            )

        ExpectRecord name fields ->
            let
                ( end, renderedFields ) =
                    List.foldl (createField (currentIndent + 1)) ( moveNewline base, [] ) fields
            in
            ( moveNewline end
            , Record
                { name = name
                , found =
                    Found
                        { start = base
                        , end = end
                        }
                        renderedFields
                , expected = expectation
                }
            )

        ExpectTree icon content ->
            let
                range =
                    { start = base, end = base }

                items =
                    []
            in
            ( moveNewline base
            , DescribeTree
                { found = ( range, items )
                , expected = expectation
                }
            )

        ExpectOneOf choices ->
            let
                id =
                    Id
                        { start = base
                        , end = end
                        }

                -- TODO: handle case of empty OneOf
                ( end, childDescription ) =
                    create (currentIndent + 1)
                        (base
                            |> moveNewline
                            |> moveColumn ((currentIndent + 1) * 4)
                        )
                        (Maybe.withDefault (ExpectStub "Unknown") (List.head choices))
            in
            ( moveNewline base
            , OneOf
                { id = id
                , choices = List.map (Choice id) choices
                , child =
                    Found { start = base, end = end } childDescription
                }
            )

        ExpectManyOf choices ->
            let
                id =
                    Id { start = base, end = base }
            in
            ( moveNewline base
            , ManyOf
                { id = id
                , choices = List.map (Choice id) choices
                , children = []
                }
            )

        ExpectStartsWith start remaining ->
            let
                ( startEnd, startChildDescription ) =
                    create currentIndent
                        base
                        start

                ( remainingEnd, remainingDescription ) =
                    create currentIndent
                        (moveNewline startEnd)
                        remaining
            in
            ( moveNewline remainingEnd
            , StartsWith
                { start = base
                , end = remainingEnd
                }
                { found = startChildDescription
                , expected = start
                }
                { found = remainingDescription
                , expected = remaining
                }
            )

        -- Primitives
        ExpectBoolean b ->
            let
                boolString =
                    boolToString b

                end =
                    moveColumn (String.length boolString) base
            in
            ( moveNewline end
            , DescribeBoolean
                (Found
                    { start = base
                    , end = end
                    }
                    b
                )
            )

        ExpectInteger i ->
            let
                end =
                    moveColumn
                        (String.length (String.fromInt i))
                        base

                pos =
                    { start = base
                    , end = end
                    }
            in
            ( moveNewline end
            , DescribeInteger
                { id = Id pos
                , found = Found pos i
                }
            )

        ExpectFloat f ->
            let
                end =
                    moveColumn
                        (String.length (String.fromFloat f))
                        base

                pos =
                    { start = base
                    , end =
                        end
                    }
            in
            ( moveNewline end
            , DescribeFloat
                { id = Id pos
                , found = Found pos ( String.fromFloat f, f )
                }
            )

        ExpectFloatBetween details ->
            let
                end =
                    moveColumn
                        (String.length (String.fromFloat details.default))
                        base

                pos =
                    { start = base
                    , end =
                        end
                    }
            in
            ( moveNewline end
            , DescribeFloatBetween
                { id = Id pos
                , min = details.min
                , max = details.max
                , found =
                    Found pos
                        ( String.fromFloat details.default
                        , details.default
                        )
                }
            )

        ExpectIntBetween details ->
            let
                end =
                    moveColumn (String.length (String.fromInt details.default)) base

                pos =
                    { start = base
                    , end = end
                    }
            in
            ( moveNewline end
            , DescribeIntBetween
                { id = Id pos
                , min = details.min
                , max = details.max
                , found = Found pos details.default
                }
            )

        -- ExpectText textNodes ->
        --     True
        ExpectString str ->
            let
                end =
                    moveColumn (String.length str) base

                pos =
                    { start = base
                    , end = end
                    }
            in
            ( moveNewline end
            , DescribeString
                (Id pos)
                str
            )

        ExpectMultiline str ->
            let
                end =
                    moveColumn (String.length str) base

                -- TODO: This position is not correct!
                -- Account for newlines
                pos =
                    { start = base
                    , end = end
                    }
            in
            ( moveNewline end
            , DescribeMultiline (Id pos) str
            )

        ExpectStringExactly str ->
            let
                end =
                    moveColumn (String.length str) base

                pos =
                    { start = base
                    , end = end
                    }
            in
            ( moveNewline end
            , DescribeStringExactly pos str
            )

        -- ExpectDate ->
        _ ->
            let
                end =
                    moveColumn (String.length "True") base
            in
            ( moveNewline end
            , DescribeBoolean
                (Found
                    { start = base
                    , end = end
                    }
                    True
                )
            )


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


{-| TODO: return coordinate adjustment
-}
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


insertAtIndex id index new desc =
    case desc of
        ManyOf many ->
            if id == many.id then
                Just
                    (ManyOf
                        { many
                            | children = makeInsertAt index new many.children
                        }
                    )

            else
                Nothing

        _ ->
            Nothing


replaceOption id new desc =
    case desc of
        OneOf one ->
            if id == one.id then
                case one.child of
                    Found range val ->
                        Just (OneOf { one | child = Found range new })

                    Unexpected unexpected ->
                        Nothing

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


replacePrimitive cursor startingPos desc =
    case cursor.makeEdit cursor.indentation startingPos desc of
        Just newDesc ->
            newDesc

        Nothing ->
            desc


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
                                    | found = Found rng (makeEdit cursor child)
                                }

                        Unexpected unexpected ->
                            desc

        Record details ->
            case cursor.makeEdit cursor.indentation (foundStart details.found) desc of
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
                                                (List.map (Tuple.mapSecond (makeFoundEdit cursor)) fields)
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
                ManyOf
                    { many
                        | children = List.map (makeFoundEdit cursor) many.children
                    }

            else
                desc

        StartsWith range fst snd ->
            -- if id is within range
            if within cursor.targetRange range then
                -- TODO
                desc

            else
                desc

        DescribeTree details ->
            -- TODO
            desc

        -- Primitives
        DescribeStub name found ->
            replacePrimitive cursor (foundStart found) desc

        DescribeBoolean found ->
            replacePrimitive cursor (foundStart found) desc

        DescribeInteger found ->
            replacePrimitive cursor (foundStart found.found) desc

        DescribeFloat found ->
            replacePrimitive cursor (foundStart found.found) desc

        DescribeFloatBetween details ->
            replacePrimitive cursor (foundStart details.found) desc

        DescribeIntBetween details ->
            replacePrimitive cursor (foundStart details.found) desc

        DescribeText rng textNodes ->
            replacePrimitive cursor rng.start desc

        DescribeString id str ->
            replacePrimitive cursor (.start (getRange id)) desc

        DescribeMultiline id str ->
            replacePrimitive cursor (.start (getRange id)) desc

        DescribeStringExactly rng str ->
            replacePrimitive cursor rng.start desc

        DescribeDate found ->
            replacePrimitive cursor (foundStart found) desc


foundStart found =
    case found of
        Found rng _ ->
            rng.start

        Unexpected unexpected ->
            unexpected.range.start


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
        , problem : Error.Error
        }


{-| -}
type alias UnexpectedDetails =
    { range : Range
    , problem : Error.Error
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
    = NoMatch



{-


       CREATION : Expectation -> Description
       MOVEMENT :



   Move Within List -> y
   Move From one List to another -> y

   Add List to Subsection ->
       ExpectManyOf -> Description -> replace content



-}


{-| -}
type Edit
    = UpdateFloat (Id Float) Float
    | UpdateString (Id String) String
    | UpdateInt (Id Int) Int
    | ReplaceOneOf (Choice (Id Options))
      -- For singular movement, Choice has to be an existing Description
      --, not an Expectation -> Description
    | Move
        { targetIndex : Int

        -- Can we make is so the payload is proved to be valid ?
        , payload : Proved

        -- List (Found Description)
        }
      -- Create an element in a ManyOf
      -- Indexes overflow, so if it's too large, it just puts it at the end.
      -- Indexes taht are below 0 and clamped to 0
    | InsertAt Int (Choice (Id ManyOptions))
    | DeleteBlock (Id ManyOptions) Int


{-| -}
type Choice id
    = Choice id Expectation


{-| -}
type Proved
    = Proved (Id ManyOptions) (List (Found Description))


{-| -}
prove : List (Found Description) -> List (Choice (Id ManyOptions)) -> Maybe Proved
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

        DescribeText _ _ ->
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
                ExpectDate ->
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

        ( ExpectDate, ExpectDate ) ->
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


type Description
    = DescribeBlock
        { name : String
        , found : Found Description
        , expected : Expectation
        }
    | Record
        { name : String
        , found : Found (List ( String, Found Description ))
        , expected : Expectation
        }
    | OneOf
        { id : Id Options
        , choices : List (Choice (Id Options))
        , child : Found Description
        }
    | ManyOf
        { id : Id ManyOptions
        , choices : List (Choice (Id ManyOptions))
        , children : List (Found Description)
        }
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
    | DescribeInteger
        { id : Id Int
        , found : Found Int
        }
    | DescribeIntBetween
        { max : Int
        , min : Int
        , found : Found Int
        , id : Id Int
        }
    | DescribeFloat
        { id : Id Float
        , found : Found ( String, Float )
        }
    | DescribeFloatBetween
        { max : Float
        , min : Float
        , found : Found ( String, Float )
        , id : Id Float
        }
    | DescribeText Range (List TextDescription)
    | DescribeString (Id String) String
    | DescribeMultiline (Id String) String
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
    | ExpectBoolean Bool
    | ExpectInteger Int
    | ExpectFloat Float
    | ExpectFloatBetween
        { min : Float
        , max : Float
        , default : Float
        }
    | ExpectIntBetween
        { min : Int
        , max : Int
        , default : Int
        }
    | ExpectText (List InlineExpectation)
    | ExpectString String
    | ExpectMultiline String
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
        DescribeBlock details ->
            spelunkUnexpectedsFromFound details.found

        Record details ->
            case details.found of
                Found _ fields ->
                    List.concatMap
                        (Tuple.second >> spelunkUnexpectedsFromFound)
                        fields

                Unexpected unexpected ->
                    [ unexpected ]

        OneOf one ->
            spelunkUnexpectedsFromFound one.child

        ManyOf many ->
            List.concatMap spelunkUnexpectedsFromFound many.children

        StartsWith _ fst snd ->
            getUnexpecteds fst.found ++ getUnexpecteds snd.found

        DescribeTree details ->
            List.concatMap getNestedUnexpecteds (Tuple.second details.found)

        -- Primitives
        DescribeStub name found ->
            unexpectedFromFound found

        DescribeBoolean found ->
            unexpectedFromFound found

        DescribeInteger details ->
            unexpectedFromFound details.found

        DescribeFloat details ->
            unexpectedFromFound details.found

        DescribeFloatBetween details ->
            unexpectedFromFound details.found

        DescribeIntBetween details ->
            unexpectedFromFound details.found

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

        DescribeBoolean found ->
            if withinFoundLeaf offset found then
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

        DescribeText rng textNodes ->
            if withinOffsetRange offset rng then
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
        DescribeBlock details ->
            cursor
                |> write ("| " ++ details.name)
                |> indent
                |> writeFound writeDescription details.found
                |> dedent

        DescribeStub name found ->
            cursor
                |> write "|"
                |> writeFound (writeWith identity) found

        Record details ->
            writeIndent cursor
                |> write ("| " ++ details.name)
                |> indent
                |> writeFound
                    (\fields curs -> List.foldr writeField curs fields)
                    details.found
                |> dedent

        OneOf one ->
            cursor
                |> writeFound writeDescription one.child

        ManyOf many ->
            List.foldl
                (writeFound writeDescription)
                cursor
                many.children

        StartsWith range start end ->
            cursor
                |> writeDescription start.found
                |> writeDescription end.found

        DescribeBoolean foundBoolean ->
            writeFound (writeWith boolToString) foundBoolean cursor

        DescribeInteger details ->
            writeFound (writeWith String.fromInt) details.found cursor

        DescribeFloat details ->
            writeFound (writeWith Tuple.first) details.found cursor

        DescribeFloatBetween details ->
            writeFound (writeWith Tuple.first) details.found cursor

        DescribeIntBetween details ->
            writeFound (writeWith String.fromInt) details.found cursor

        DescribeText range textNodes ->
            cursor
                |> advanceTo range
                |> (\c -> List.foldl writeTextDescription c textNodes)

        DescribeString id str ->
            cursor
                |> advanceTo (getRange id)
                |> write str

        DescribeMultiline id str ->
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
                |> advanceTo (getRange id)
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
                        { errors = List.map (Error.render source) (getUnexpecteds val)
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
                            Err NoMatch

                    _ ->
                        Err NoMatch
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
                    DescribeBlock details ->
                        if details.name == name then
                            case details.found of
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
                                                        , expected = details.expected
                                                        }
                                                    )
                                                )

                                Unexpected unexpected ->
                                    Ok
                                        (Found unexpected.range
                                            (renderer
                                                { found =
                                                    Unexpected unexpected
                                                , expected = details.expected
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
                            DescribeBlock
                                { found = Found range value
                                , name = name
                                , expected = ExpectBlock name (getBlockExpectation child)
                                }

                        Err ( pos, errorMessage ) ->
                            DescribeBlock
                                { name = name
                                , found =
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
                                            |. Parser.token (Parser.Token (String.repeat (indentation + 4) " ") (ExpectingIndentation (indentation + 4)))
                                          )
                                            |> Parser.andThen
                                                (\start ->
                                                    Parser.oneOf
                                                        -- If there's st
                                                        [ Parser.succeed
                                                            (\end ->
                                                                Err
                                                                    ( { start = start, end = end }
                                                                    , Error.ExpectingIndent (indentation + 4)
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
                                                Err ( pos, Error.ExpectingIndent (indentation + 4) )
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
                                Err NoMatch

                    _ ->
                        Err NoMatch
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
                                                Err ( pos, Error.UnknownBlock blockNames )
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
                    OneOf details ->
                        case details.child of
                            Found rng found ->
                                case List.foldl (applyDesc found) Nothing blocks of
                                    Nothing ->
                                        Err NoMatch

                                    Just result ->
                                        Ok result

                            Unexpected unexpected ->
                                Ok (Found unexpected.range (renderUnexpected unexpected))

                    _ ->
                        Err NoMatch
        , parser =
            Parser.succeed
                (\( range, result ) ->
                    case result of
                        Ok found ->
                            OneOf
                                { choices = List.map (Choice (optionId range)) expectations
                                , child = Found range found
                                , id = optionId range
                                }

                        Err ( pos, unexpected ) ->
                            OneOf
                                { choices = List.map (Choice (optionId range)) expectations
                                , child =
                                    Unexpected
                                        { range = pos
                                        , problem = unexpected
                                        }
                                , id = optionId range
                                }
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
                                                        Err ( pos, Error.UnknownBlock blockNames )
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
                                                Err NoMatch

                                            Just (Found rng result) ->
                                                Ok (result :: existing)

                                            Just (Unexpected unexpected) ->
                                                Ok (renderUnexpected unexpected :: existing)
                in
                case desc of
                    ManyOf many ->
                        List.foldl getRendered (Ok []) many.children
                            |> Result.map (\items -> Found (getRange many.id) (List.reverse items))

                    _ ->
                        Err NoMatch
        , parser =
            Parser.succeed
                (\( range, results ) ->
                    ManyOf
                        { choices = List.map (Choice (manyOptionId range)) expectations
                        , id = manyOptionId range
                        , children = List.map resultToFound results
                        }
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
                        Err NoMatch
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
                    Record details ->
                        if details.name == recordName then
                            case details.found of
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
            Err NoMatch


renderTextComponent options comp found =
    case comp of
        Styled range textEl ->
            options.view range textEl :: found

        DescribeInline name range foundInline ->
            case List.foldl (renderInline name range (List.reverse foundInline)) (Err NoMatch) options.inlines of
                Err err ->
                    options.error
                        { range = range
                        , problem = Error.UnknownInline (List.map (getInlineName << getInlineExpectation) options.inlines)
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
                        Err NoMatch

                    (DescribeInlineString key range str) :: remaining ->
                        case details.converter remaining of
                            Err err ->
                                Err err

                            Ok renderers ->
                                Ok (List.map (\x -> x str) renderers)

                    _ ->
                        Err NoMatch
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
                        Err NoMatch

                    (DescribeInlineText range str) :: remaining ->
                        case details.converter remaining of
                            Err err ->
                                Err err

                            Ok renderers ->
                                Ok (List.map (\x -> x str) renderers)

                    _ ->
                        Err NoMatch
        , expect =
            case details.expect of
                InlineExpectation parentName vals ->
                    InlineExpectation parentName (vals ++ [ ExpectInlineText ])
        }



{- PRIMITIVE BLOCKS -}


{-| -}
multiline : String -> Block String
multiline default =
    Value
        { expect = ExpectMultiline default
        , converter =
            \desc ->
                case desc of
                    DescribeMultiline range str ->
                        Ok (Found (getRange range) str)

                    _ ->
                        Err NoMatch
        , parser =
            Parser.map
                (\( pos, str ) ->
                    DescribeMultiline (Id pos) str
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
string : String -> Block String
string default =
    Value
        { expect = ExpectString default
        , converter =
            \desc ->
                case desc of
                    DescribeString id str ->
                        Ok (Found (getRange id) str)

                    _ ->
                        Err NoMatch
        , parser =
            Parser.succeed
                (\start val end ->
                    DescribeString (Id { start = start, end = end }) val
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
                        Err NoMatch
        , parser =
            Parser.map
                (\( pos, parsedPosix ) ->
                    case parsedPosix of
                        Err str ->
                            DescribeDate
                                (Unexpected
                                    { range = pos
                                    , problem = Error.BadDate str
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
bool : Bool -> Block Bool
bool default =
    Value
        { expect = ExpectBoolean default
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
                                    , problem = Error.BadBool err
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


{-| Parse an `Int` block.

Takes a default value that is used when autofilling.

-}
int : Int -> Block Int
int default =
    Value
        { converter =
            \desc ->
                case desc of
                    DescribeInteger details ->
                        Ok details.found

                    _ ->
                        Err NoMatch
        , expect = ExpectInteger default
        , parser =
            Parser.map
                (\( id, foundInt ) ->
                    DescribeInteger
                        { id = id
                        , found = foundInt
                        }
                )
                integer
        }


{-| Takes a default value that is used when autofilling.
-}
float : Float -> Block Float
float default =
    Value
        { converter =
            \desc ->
                case desc of
                    DescribeFloat details ->
                        Ok (mapFound Tuple.second details.found)

                    _ ->
                        Err NoMatch
        , expect = ExpectFloat default
        , parser =
            Parser.map
                (\( id, fl ) ->
                    DescribeFloat
                        { id = id, found = fl }
                )
                floating
        }


{-| -}
intBetween :
    { min : Int
    , max : Int
    , default : Int
    }
    -> Block Int
intBetween bounds =
    let
        top =
            max bounds.min bounds.max

        bottom =
            min bounds.min bounds.max
    in
    Value
        { expect =
            ExpectIntBetween
                { min = bottom
                , max = top
                , default = bounds.default
                }
        , converter =
            \desc ->
                case desc of
                    DescribeIntBetween details ->
                        Ok details.found

                    _ ->
                        Err NoMatch
        , parser =
            Parser.map
                (\( id, found ) ->
                    DescribeIntBetween
                        { min = bottom
                        , max = top
                        , id = id
                        , found =
                            case found of
                                Found rng i ->
                                    if i >= bottom && i <= top then
                                        found

                                    else
                                        Unexpected
                                            { range = rng
                                            , problem =
                                                Error.IntOutOfRange
                                                    { found = i
                                                    , min = bottom
                                                    , max = top
                                                    }
                                            }

                                _ ->
                                    found
                        }
                )
                integer
        }


{-| -}
floatBetween :
    { min : Float
    , max : Float
    , default : Float
    }
    -> Block Float
floatBetween bounds =
    let
        top =
            max bounds.min bounds.max

        bottom =
            min bounds.min bounds.max
    in
    Value
        { expect = ExpectFloatBetween bounds
        , converter =
            \desc ->
                case desc of
                    DescribeFloatBetween details ->
                        Ok (mapFound Tuple.second details.found)

                    _ ->
                        Err NoMatch
        , parser =
            Parser.map
                (\( id, found ) ->
                    DescribeFloatBetween
                        { min = bottom
                        , max = top
                        , id = id
                        , found =
                            case found of
                                Found rng ( str, i ) ->
                                    if i >= bottom && i <= top then
                                        found

                                    else
                                        Unexpected
                                            { range = rng
                                            , problem =
                                                Error.FloatOutOfRange
                                                    { found = i
                                                    , min = bottom
                                                    , max = top
                                                    }
                                            }

                                _ ->
                                    found
                        }
                )
                floating
        }



{- Parser Heleprs -}


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
                            (ExpectingIndentation numSpaces)
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
                |. Parser.token (Parser.Token (String.repeat indentation " ") (ExpectingIndentation indentation))
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
                    |. Parser.token (Parser.Token (String.repeat indentation " ") (ExpectingIndentation indentation))
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


integer : Parser Context Problem ( Id Int, Found Int )
integer =
    Parser.map
        (\( pos, intResult ) ->
            case intResult of
                Ok i ->
                    ( Id pos, Found pos i )

                Err str ->
                    ( Id pos
                    , Unexpected
                        { range = pos
                        , problem = Error.BadInt str
                        }
                    )
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
floating : Parser Context Problem ( Id Float, Found ( String, Float ) )
floating =
    Parser.map
        (\( pos, floatResult ) ->
            case floatResult of
                Ok f ->
                    ( Id pos, Found pos f )

                Err str ->
                    ( Id pos
                    , Unexpected
                        { range = pos
                        , problem = Error.BadFloat str
                        }
                    )
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
            ( { start = start
            , end = end }, val )
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
                    Record
                        { expected = expectations
                        , name = recordName
                        , found =
                            Found pos ok
                        }

                Err ( maybePosition, problem ) ->
                    Record
                        { expected = expectations
                        , name = recordName
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
                                Error.UnexpectedField
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
    , found : Result ( Maybe Range, Error.Error ) (List ( String, Found Description ))
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
            |. Parser.token (Parser.Token (String.repeat indentation " ") (ExpectingIndentation indentation))
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
    -> Parser Context Problem (Parser.Step RecordFields (Result ( Maybe Range, Error.Error ) (List ( String, Found Description ))))
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
                                                                Err ( Nothing, Error.ExpectingIndent indentation )
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
                                                ( Nothing, Error.MissingFields (List.map Tuple.first fields.remaining) )
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


getField : Field value -> List ( String, Found Description ) -> Result Error.Error (Found value)
getField (Field name fieldBlock) fields =
    List.foldl (matchField name fieldBlock) (Err (Error.MissingFields [ name ])) fields


matchField : String -> Block value -> ( String, Found Description ) -> Result Error.Error (Found value) -> Result Error.Error (Found value)
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
                |. Parser.token (Parser.Token (String.repeat (previous + 4) " ") (ExpectingIndentation (previous + 4)))
             , Parser.succeed previous
                |. Parser.token (Parser.Token (String.repeat previous " ") (ExpectingIndentation previous))
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
                        (ExpectingIndentation (base + indentLevel))
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
                        |. Parser.token (Parser.Token (String.repeat level " ") (ExpectingIndentation level))
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
                                            Error.UnknownInline (List.map getInlineName options.inlines)
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
