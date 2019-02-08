module Mark.Advanced exposing
    ( Document
    , Outcome(..)
    , parse, Parsed
    , compile, convert
    , Block
    , document
    , block, stub
    , oneOf, manyOf, startWith, nested
    , field, Field, record2, record3, record4, record5, record6, record7, record8, record9, record10
    , string, exactly, int, float, floatBetween, intBetween, bool, date, multiline
    , text, replacement, balanced, Replacement
    , Inline, inline, inlineString, inlineText, mapInline
    , map, idToRange
    , focus, parent
    , Range, Position, foldNested, foldNestedList, replaceNested
    , Error, errorToString, errorToHtml, Theme(..)
    )

{-| An advanced `Document` gives two new advantages over `Mark.Document`

  - An advanced `Document` can be rendered even if there's been an error. Essentially you provide the logic of what to display for a given block if something unexpected was encountered inside.

  - Instead of failing to render anything if there's an issue, an `Advanced.Document` can render errors in your document and continue to render the document.

  - You can parse a `Document` to a `Description` (from the `Mark.Description` module).

These are both useful for making live editors.

The `Description` can be

1.  Edited via the edits messages in `Mark.Description.Edit`.

Parsing is an intensive process. In the best case the Parser has to go through each character in a string to see if it's valid.

This can be an issue if you're trying to make an editor, because it could mean that every keystroke causes the whole document to be parsed!

A solution to this is to parse a `Document` once to an intermediate data structure, in our case that's a `Description`.

@docs Document

@docs Outcome

@docs parse, Parsed

@docs compile, convert


## Building Documents

@docs Block

@docs document

@docs block, stub

@docs oneOf, manyOf, startWith, nested

@docs field, Field, record2, record3, record4, record5, record6, record7, record8, record9, record10

@docs string, exactly, int, float, floatBetween, intBetween, bool, date, multiline


## Handling Text and Inline

@docs text, replacement, balanced, Replacement

@docs Inline, inline, inlineString, inlineText, mapInline

@docs map, idToRange

@docs focus, parent

@docs Range, Position, foldNested, foldNestedList, replaceNested


## Displaying Errors

@docs Error, errorToString, errorToHtml, Theme

-}

import Html
import Html.Attributes
import Iso8601
import Mark.Description exposing (..)
import Mark.Format as Format
import Mark.Internal.Error as Error exposing (Context(..), Problem(..))
import Mark.Internal.Id exposing (..)
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)
import Time


{-| -}
type alias Parsed =
    Mark.Description.Parsed


idToRange =
    getRange



{- INTERFACE -}


{-| -}
type Outcome failure almost success
    = Success success
    | Almost almost
    | Failure failure


{-| -}
parse :
    Document data
    -> String
    -> Outcome Error (Partial Parsed) Parsed
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
type alias Partial data =
    { errors : List Error
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
compile : Document data -> String -> Outcome Error (Partial data) data
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
convert : Document data -> Parsed -> Outcome Error (Partial data) data
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
focus : Position -> Parsed -> Parsed
focus pos (Parsed parsed) =
    Parsed { parsed | focus = Just pos }


{-| -}
parent : Parsed -> Parsed
parent parsed =
    -- TODO: implement
    Debug.todo "implement!"


within rangeOne rangeTwo =
    withinOffsetRange { start = rangeOne.start.offset, end = rangeOne.end.offset } rangeTwo


withinOffsetRange offset range =
    range.start.offset <= offset.start && range.end.offset >= offset.end


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

        DescribeBoolean details ->
            unexpectedFromFound details.found

        DescribeInteger details ->
            unexpectedFromFound details.found

        DescribeFloat details ->
            unexpectedFromFound details.found

        DescribeFloatBetween details ->
            unexpectedFromFound details.found

        DescribeIntBetween details ->
            unexpectedFromFound details.found

        DescribeText details ->
            []

        DescribeString rng str ->
            []

        DescribeMultiline rng str ->
            []

        DescribeStringExactly rng str ->
            []

        DescribeDate details ->
            unexpectedFromFound details.found


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
type alias Error =
    { message : List Format.Text
    , region : { start : Position, end : Position }
    , title : String
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



{- BLOCKS -}


{-| -}
document :
    { error : UnexpectedDetails -> result
    , view : Range -> child -> result
    }
    -> Block child
    -> Document result
document doc child =
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

                            Ok (Found r renderedChild) ->
                                Ok (doc.view r renderedChild)

                            Ok (Unexpected unexpected) ->
                                Ok (doc.error unexpected)

                    Unexpected unexpected ->
                        Ok (doc.error unexpected)
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
    { name : String
    , view : Range -> child -> result
    , error : UnexpectedDetails -> result
    }
    -> Block child
    -> Block result
block blockDetails child =
    Block blockDetails.name
        { expect = ExpectBlock blockDetails.name (getBlockExpectation child)
        , converter =
            \desc ->
                case desc of
                    DescribeBlock details ->
                        if details.name == blockDetails.name then
                            case details.found of
                                Found range found ->
                                    case renderBlock child found of
                                        Err err ->
                                            -- AST mismatch
                                            Err err

                                        Ok foundRenderedChild ->
                                            Ok
                                                (case foundRenderedChild of
                                                    Found rng renderedChild ->
                                                        Found range
                                                            (blockDetails.view rng renderedChild)

                                                    Unexpected unexpectedDetails ->
                                                        Found unexpectedDetails.range
                                                            (blockDetails.error unexpectedDetails)
                                                )

                                Unexpected unexpected ->
                                    Ok
                                        (Found unexpected.range
                                            (blockDetails.error unexpected)
                                        )

                        else
                            -- This is not the block that was expected.
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
                                , name = blockDetails.name
                                , expected = ExpectBlock blockDetails.name (getBlockExpectation child)
                                }

                        Err ( pos, errorMessage ) ->
                            DescribeBlock
                                { name = blockDetails.name
                                , found =
                                    Unexpected
                                        { range = pos
                                        , problem = errorMessage
                                        }
                                , expected = ExpectBlock blockDetails.name (getBlockExpectation child)
                                }
                )
            <|
                withRange
                    (Parser.getIndent
                        |> Parser.andThen
                            (\indentation ->
                                Parser.succeed identity
                                    |. Parser.keyword (Parser.Token blockDetails.name (ExpectingBlockName blockDetails.name))
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
                                                            Parser.withIndent (indentation + 4) (Parser.inContext (InBlock blockDetails.name) (getParser child))
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
startWith :
    { view : Range -> start -> rest -> result
    , error : UnexpectedDetails -> result
    }
    -> Block start
    -> Block rest
    -> Block result
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
                                        (fn.view range renderedStart renderedEnd)

                            ( Ok (Unexpected unexpected), _ ) ->
                                Ok (Found range (fn.error unexpected))

                            ( _, Ok (Unexpected unexpected) ) ->
                                Ok (Found range (fn.error unexpected))

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
oneOf :
    { view :
        { id : Id Options
        , options : List (Choice (Id Options) Expectation)
        }
        -> a
        -> b
    , error :
        { id : Id Options
        , options : List (Choice (Id Options) Expectation)
        }
        -> UnexpectedDetails
        -> b
    }
    -> List (Block a)
    -> Block b
oneOf oneOfDetails blocks =
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

                                    Just foundResult ->
                                        case foundResult of
                                            Found r child ->
                                                Ok
                                                    (Found r
                                                        (oneOfDetails.view
                                                            { id = details.id
                                                            , options =
                                                                List.map (Choice details.id) expectations
                                                            }
                                                            child
                                                        )
                                                    )

                                            Unexpected unexpected ->
                                                Ok
                                                    (Found unexpected.range
                                                        (oneOfDetails.error
                                                            { id = details.id
                                                            , options =
                                                                List.map (Choice details.id) expectations
                                                            }
                                                            unexpected
                                                        )
                                                    )

                            Unexpected unexpected ->
                                Ok
                                    (Found unexpected.range
                                        (oneOfDetails.error
                                            { id = details.id
                                            , options =
                                                List.map (Choice details.id) expectations
                                            }
                                            unexpected
                                        )
                                    )

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
                        (blockParser :: List.reverse (unexpectedInOneOf expectations :: childValues))
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
                    |= withRange word
            )


humanReadableExpectations expect =
    case expect of
        ExpectBlock blockName exp ->
            "| " ++ blockName

        ExpectStub stubName ->
            "| " ++ stubName

        ExpectRecord name fields ->
            "| " ++ name

        ExpectOneOf expectations ->
            "One of: " ++ String.join ", " (List.map humanReadableExpectations expectations)

        ExpectManyOf expectations ->
            "Many of: " ++ String.join ", " (List.map humanReadableExpectations expectations)

        ExpectStartsWith start remain ->
            humanReadableExpectations start ++ " and then " ++ humanReadableExpectations remain

        ExpectBoolean _ ->
            "A Boolean"

        ExpectInteger _ ->
            "An Int"

        ExpectFloat _ ->
            "A Float"

        ExpectFloatBetween bounds ->
            "A Float between " ++ String.fromFloat bounds.min ++ " and " ++ String.fromFloat bounds.max

        ExpectIntBetween bounds ->
            "An Int between " ++ String.fromInt bounds.min ++ " and " ++ String.fromInt bounds.max

        ExpectText inlines ->
            "Styled Text"

        ExpectString _ ->
            "A String"

        ExpectMultiline _ ->
            "A Multiline String"

        ExpectStringExactly exact ->
            exact

        ExpectDate _ ->
            "A DateTime"

        ExpectTree icon content ->
            "A tree starting with "
                ++ humanReadableExpectations icon
                ++ " and with "
                ++ humanReadableExpectations content
                ++ " content"


{-| Many blocks that are all at the same indentation level.
-}
manyOf :
    { view :
        { parent : Id ManyOptions
        , index : Int
        , options : List (Choice (Id ManyOptions) Expectation)
        }
        -> a
        -> b
    , error :
        { parent : Id ManyOptions
        , index : Int
        , options : List (Choice (Id ManyOptions) Expectation)
        }
        -> UnexpectedDetails
        -> b
    }
    -> List (Block a)
    -> Block (List b)
manyOf manyOfDetails blocks =
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

                    -- getRendered : Found Description -> Result AstError (List a) -> Result AstError (List a)
                    getRendered id choices found ( existingResult, index ) =
                        case existingResult of
                            Err err ->
                                ( Err err, index + 1 )

                            Ok existing ->
                                case found of
                                    Unexpected unexpected ->
                                        ( Ok
                                            (manyOfDetails.error
                                                { parent = id
                                                , options = choices
                                                , index = index
                                                }
                                                unexpected
                                                :: existing
                                            )
                                        , index + 1
                                        )

                                    Found range child ->
                                        case List.foldl (applyDesc child) Nothing blocks of
                                            Nothing ->
                                                ( Err NoMatch, index + 1 )

                                            Just (Found rng result) ->
                                                ( Ok
                                                    (manyOfDetails.view
                                                        { parent = id
                                                        , options = choices
                                                        , index = index
                                                        }
                                                        result
                                                        :: existing
                                                    )
                                                , index + 1
                                                )

                                            Just (Unexpected unexpected) ->
                                                ( Ok
                                                    (manyOfDetails.error
                                                        { parent = id
                                                        , options = choices
                                                        , index = index
                                                        }
                                                        unexpected
                                                        :: existing
                                                    )
                                                , index + 1
                                                )
                in
                case desc of
                    ManyOf many ->
                        List.foldl (getRendered many.id many.choices) ( Ok [], 0 ) many.children
                            |> Tuple.first
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


{-| -}
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
    { name : String
    , view : Range -> one -> two -> data
    , error : UnexpectedDetails -> data
    }
    -> Field one
    -> Field two
    -> Block data
record2 record field1 field2 =
    let
        expectations =
            ExpectRecord record.name
                [ fieldExpectation field1
                , fieldExpectation field2
                ]
    in
    Block record.name
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record details ->
                        if details.name == record.name then
                            case details.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok (record.view pos))
                                        |> Result.map2 applyField (getField field1 fieldDescriptions)
                                        |> Result.map2 applyField (getField field2 fieldDescriptions)
                                        |> renderRecordResult record.error pos

                                Unexpected unexpected ->
                                    Ok (Found unexpected.range (record.error unexpected))

                        else
                            Err NoMatch

                    _ ->
                        Err NoMatch
        , parser =
            parseRecord record.name
                expectations
                [ fieldParser field1
                , fieldParser field2
                ]
        }


{-| -}
record3 :
    { name : String
    , view : Range -> one -> two -> three -> data
    , error : UnexpectedDetails -> data
    }
    -> Field one
    -> Field two
    -> Field three
    -> Block data
record3 record field1 field2 field3 =
    let
        expectations =
            ExpectRecord record.name
                [ fieldExpectation field1
                , fieldExpectation field2
                , fieldExpectation field3
                ]
    in
    Block record.name
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record details ->
                        if details.name == record.name then
                            case details.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok (record.view pos))
                                        |> Result.map2 applyField (getField field1 fieldDescriptions)
                                        |> Result.map2 applyField (getField field2 fieldDescriptions)
                                        |> Result.map2 applyField (getField field3 fieldDescriptions)
                                        |> renderRecordResult record.error pos

                                Unexpected unexpected ->
                                    Ok (Found unexpected.range (record.error unexpected))

                        else
                            Err NoMatch

                    _ ->
                        Err NoMatch
        , parser =
            parseRecord record.name
                expectations
                [ fieldParser field1
                , fieldParser field2
                , fieldParser field3
                ]
        }


{-| -}
record4 :
    { name : String
    , view : Range -> one -> two -> three -> four -> data
    , error : UnexpectedDetails -> data
    }
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Block data
record4 record field1 field2 field3 field4 =
    let
        expectations =
            ExpectRecord record.name
                [ fieldExpectation field1
                , fieldExpectation field2
                , fieldExpectation field3
                , fieldExpectation field4
                ]
    in
    Block record.name
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record details ->
                        if details.name == record.name then
                            case details.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok (record.view pos))
                                        |> Result.map2 applyField (getField field1 fieldDescriptions)
                                        |> Result.map2 applyField (getField field2 fieldDescriptions)
                                        |> Result.map2 applyField (getField field3 fieldDescriptions)
                                        |> Result.map2 applyField (getField field4 fieldDescriptions)
                                        |> renderRecordResult record.error pos

                                Unexpected unexpected ->
                                    Ok (Found unexpected.range (record.error unexpected))

                        else
                            Err NoMatch

                    _ ->
                        Err NoMatch
        , parser =
            parseRecord record.name
                expectations
                [ fieldParser field1
                , fieldParser field2
                , fieldParser field3
                , fieldParser field4
                ]
        }


{-| -}
record5 :
    { name : String
    , view : Range -> one -> two -> three -> four -> five -> data
    , error : UnexpectedDetails -> data
    }
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Field five
    -> Block data
record5 record field1 field2 field3 field4 field5 =
    let
        expectations =
            ExpectRecord record.name
                [ fieldExpectation field1
                , fieldExpectation field2
                , fieldExpectation field3
                , fieldExpectation field4
                ]
    in
    Block record.name
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record details ->
                        if details.name == record.name then
                            case details.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok (record.view pos))
                                        |> Result.map2 applyField (getField field1 fieldDescriptions)
                                        |> Result.map2 applyField (getField field2 fieldDescriptions)
                                        |> Result.map2 applyField (getField field3 fieldDescriptions)
                                        |> Result.map2 applyField (getField field4 fieldDescriptions)
                                        |> Result.map2 applyField (getField field5 fieldDescriptions)
                                        |> renderRecordResult record.error pos

                                Unexpected unexpected ->
                                    Ok (Found unexpected.range (record.error unexpected))

                        else
                            Err NoMatch

                    _ ->
                        Err NoMatch
        , parser =
            parseRecord record.name
                expectations
                [ fieldParser field1
                , fieldParser field2
                , fieldParser field3
                , fieldParser field4
                , fieldParser field5
                ]
        }


{-| -}
record6 :
    { name : String
    , view : Range -> one -> two -> three -> four -> five -> six -> data
    , error : UnexpectedDetails -> data
    }
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Field five
    -> Field six
    -> Block data
record6 record field1 field2 field3 field4 field5 field6 =
    let
        expectations =
            ExpectRecord record.name
                [ fieldExpectation field1
                , fieldExpectation field2
                , fieldExpectation field3
                , fieldExpectation field4
                ]
    in
    Block record.name
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record details ->
                        if details.name == record.name then
                            case details.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok (record.view pos))
                                        |> Result.map2 applyField (getField field1 fieldDescriptions)
                                        |> Result.map2 applyField (getField field2 fieldDescriptions)
                                        |> Result.map2 applyField (getField field3 fieldDescriptions)
                                        |> Result.map2 applyField (getField field4 fieldDescriptions)
                                        |> Result.map2 applyField (getField field5 fieldDescriptions)
                                        |> Result.map2 applyField (getField field6 fieldDescriptions)
                                        |> renderRecordResult record.error pos

                                Unexpected unexpected ->
                                    Ok (Found unexpected.range (record.error unexpected))

                        else
                            Err NoMatch

                    _ ->
                        Err NoMatch
        , parser =
            parseRecord record.name
                expectations
                [ fieldParser field1
                , fieldParser field2
                , fieldParser field3
                , fieldParser field4
                , fieldParser field5
                , fieldParser field6
                ]
        }


{-| -}
record7 :
    { name : String
    , view : Range -> one -> two -> three -> four -> five -> six -> seven -> data
    , error : UnexpectedDetails -> data
    }
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Field five
    -> Field six
    -> Field seven
    -> Block data
record7 record field1 field2 field3 field4 field5 field6 field7 =
    let
        expectations =
            ExpectRecord record.name
                [ fieldExpectation field1
                , fieldExpectation field2
                , fieldExpectation field3
                , fieldExpectation field4
                ]
    in
    Block record.name
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record details ->
                        if details.name == record.name then
                            case details.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok (record.view pos))
                                        |> Result.map2 applyField (getField field1 fieldDescriptions)
                                        |> Result.map2 applyField (getField field2 fieldDescriptions)
                                        |> Result.map2 applyField (getField field3 fieldDescriptions)
                                        |> Result.map2 applyField (getField field4 fieldDescriptions)
                                        |> Result.map2 applyField (getField field5 fieldDescriptions)
                                        |> Result.map2 applyField (getField field6 fieldDescriptions)
                                        |> Result.map2 applyField (getField field7 fieldDescriptions)
                                        |> renderRecordResult record.error pos

                                Unexpected unexpected ->
                                    Ok (Found unexpected.range (record.error unexpected))

                        else
                            Err NoMatch

                    _ ->
                        Err NoMatch
        , parser =
            parseRecord record.name
                expectations
                [ fieldParser field1
                , fieldParser field2
                , fieldParser field3
                , fieldParser field4
                , fieldParser field5
                , fieldParser field6
                , fieldParser field7
                ]
        }


{-| -}
record8 :
    { name : String
    , view : Range -> one -> two -> three -> four -> five -> six -> seven -> eight -> data
    , error : UnexpectedDetails -> data
    }
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Field five
    -> Field six
    -> Field seven
    -> Field eight
    -> Block data
record8 record field1 field2 field3 field4 field5 field6 field7 field8 =
    let
        expectations =
            ExpectRecord record.name
                [ fieldExpectation field1
                , fieldExpectation field2
                , fieldExpectation field3
                , fieldExpectation field4
                ]
    in
    Block record.name
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record details ->
                        if details.name == record.name then
                            case details.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok (record.view pos))
                                        |> Result.map2 applyField (getField field1 fieldDescriptions)
                                        |> Result.map2 applyField (getField field2 fieldDescriptions)
                                        |> Result.map2 applyField (getField field3 fieldDescriptions)
                                        |> Result.map2 applyField (getField field4 fieldDescriptions)
                                        |> Result.map2 applyField (getField field5 fieldDescriptions)
                                        |> Result.map2 applyField (getField field6 fieldDescriptions)
                                        |> Result.map2 applyField (getField field7 fieldDescriptions)
                                        |> Result.map2 applyField (getField field8 fieldDescriptions)
                                        |> renderRecordResult record.error pos

                                Unexpected unexpected ->
                                    Ok (Found unexpected.range (record.error unexpected))

                        else
                            Err NoMatch

                    _ ->
                        Err NoMatch
        , parser =
            parseRecord record.name
                expectations
                [ fieldParser field1
                , fieldParser field2
                , fieldParser field3
                , fieldParser field4
                , fieldParser field5
                , fieldParser field6
                , fieldParser field7
                , fieldParser field8
                ]
        }


{-| -}
record9 :
    { name : String
    , view : Range -> one -> two -> three -> four -> five -> six -> seven -> eight -> nine -> data
    , error : UnexpectedDetails -> data
    }
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Field five
    -> Field six
    -> Field seven
    -> Field eight
    -> Field nine
    -> Block data
record9 record field1 field2 field3 field4 field5 field6 field7 field8 field9 =
    let
        expectations =
            ExpectRecord record.name
                [ fieldExpectation field1
                , fieldExpectation field2
                , fieldExpectation field3
                , fieldExpectation field4
                ]
    in
    Block record.name
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record details ->
                        if details.name == record.name then
                            case details.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok (record.view pos))
                                        |> Result.map2 applyField (getField field1 fieldDescriptions)
                                        |> Result.map2 applyField (getField field2 fieldDescriptions)
                                        |> Result.map2 applyField (getField field3 fieldDescriptions)
                                        |> Result.map2 applyField (getField field4 fieldDescriptions)
                                        |> Result.map2 applyField (getField field5 fieldDescriptions)
                                        |> Result.map2 applyField (getField field6 fieldDescriptions)
                                        |> Result.map2 applyField (getField field7 fieldDescriptions)
                                        |> Result.map2 applyField (getField field8 fieldDescriptions)
                                        |> Result.map2 applyField (getField field9 fieldDescriptions)
                                        |> renderRecordResult record.error pos

                                Unexpected unexpected ->
                                    Ok (Found unexpected.range (record.error unexpected))

                        else
                            Err NoMatch

                    _ ->
                        Err NoMatch
        , parser =
            parseRecord record.name
                expectations
                [ fieldParser field1
                , fieldParser field2
                , fieldParser field3
                , fieldParser field4
                , fieldParser field5
                , fieldParser field6
                , fieldParser field7
                , fieldParser field8
                , fieldParser field9
                ]
        }


{-| -}
record10 :
    { name : String
    , view : Range -> one -> two -> three -> four -> five -> six -> seven -> eight -> nine -> ten -> data
    , error : UnexpectedDetails -> data
    }
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Field five
    -> Field six
    -> Field seven
    -> Field eight
    -> Field nine
    -> Field ten
    -> Block data
record10 record field1 field2 field3 field4 field5 field6 field7 field8 field9 field10 =
    let
        expectations =
            ExpectRecord record.name
                [ fieldExpectation field1
                , fieldExpectation field2
                , fieldExpectation field3
                , fieldExpectation field4
                ]
    in
    Block record.name
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record details ->
                        if details.name == record.name then
                            case details.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok (record.view pos))
                                        |> Result.map2 applyField (getField field1 fieldDescriptions)
                                        |> Result.map2 applyField (getField field2 fieldDescriptions)
                                        |> Result.map2 applyField (getField field3 fieldDescriptions)
                                        |> Result.map2 applyField (getField field4 fieldDescriptions)
                                        |> Result.map2 applyField (getField field5 fieldDescriptions)
                                        |> Result.map2 applyField (getField field6 fieldDescriptions)
                                        |> Result.map2 applyField (getField field7 fieldDescriptions)
                                        |> Result.map2 applyField (getField field8 fieldDescriptions)
                                        |> Result.map2 applyField (getField field9 fieldDescriptions)
                                        |> Result.map2 applyField (getField field10 fieldDescriptions)
                                        |> renderRecordResult record.error pos

                                Unexpected unexpected ->
                                    Ok (Found unexpected.range (record.error unexpected))

                        else
                            Err NoMatch

                    _ ->
                        Err NoMatch
        , parser =
            parseRecord record.name
                expectations
                [ fieldParser field1
                , fieldParser field2
                , fieldParser field3
                , fieldParser field4
                , fieldParser field5
                , fieldParser field6
                , fieldParser field7
                , fieldParser field8
                , fieldParser field9
                , fieldParser field10
                ]
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
        DescribeText details ->
            List.foldl (renderTextComponent options) [] details.text
                |> (Found (getRange details.id) << List.reverse)
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


getInlineName (InlineExpectation name _) =
    name


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
multiline :
    { default : String
    , view : Id String -> String -> a
    }
    -> Block a
multiline details =
    Value
        { expect = ExpectMultiline details.default
        , converter =
            \desc ->
                case desc of
                    DescribeMultiline id str ->
                        Ok
                            (Found (getRange id)
                                (details.view id str)
                            )

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
string :
    { default : String
    , view : Id String -> String -> a
    }
    -> Block a
string details =
    Value
        { expect = ExpectString details.default
        , converter =
            \desc ->
                case desc of
                    DescribeString id str ->
                        Ok
                            (Found (getRange id)
                                (details.view id str)
                            )

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
date :
    { default : Time.Posix
    , view : Id Time.Posix -> Time.Posix -> a
    }
    -> Block a
date details =
    Value
        { expect = ExpectDate details.default
        , converter =
            \desc ->
                case desc of
                    DescribeDate found ->
                        Ok (mapFound (\( str_, fl ) -> details.view found.id fl) found.found)

                    _ ->
                        Err NoMatch
        , parser =
            Parser.map
                (\( pos, parsedPosix ) ->
                    case parsedPosix of
                        Err str ->
                            DescribeDate
                                { id = Id pos
                                , found =
                                    Unexpected
                                        { range = pos
                                        , problem = Error.BadDate str
                                        }
                                }

                        Ok ( str, posix ) ->
                            DescribeDate
                                { id = Id pos
                                , found = Found pos ( str, posix )
                                }
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
bool :
    { default : Bool
    , view : Id Bool -> Bool -> a
    }
    -> Block a
bool { default, view } =
    Value
        { expect = ExpectBoolean default
        , converter =
            \desc ->
                case desc of
                    DescribeBoolean details ->
                        Ok (mapFound (view details.id) details.found)

                    _ ->
                        Err NoMatch
        , parser =
            Parser.map
                (\( range, boolResult ) ->
                    DescribeBoolean
                        { id = Id range
                        , found =
                            case boolResult of
                                Err err ->
                                    Unexpected
                                        { range = range
                                        , problem = Error.BadBool err
                                        }

                                Ok b ->
                                    Found range
                                        b
                        }
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
int :
    { default : Int
    , view : Id Int -> Int -> a
    }
    -> Block a
int { default, view } =
    Value
        { converter =
            \desc ->
                case desc of
                    DescribeInteger details ->
                        Ok (mapFound (view details.id) details.found)

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
float :
    { default : Float
    , view : Id Float -> Float -> a
    }
    -> Block a
float { default, view } =
    Value
        { converter =
            \desc ->
                case desc of
                    DescribeFloat details ->
                        Ok (mapFound (\( str_, fl ) -> view details.id fl) details.found)

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
    , view : Id Int -> Int -> a
    }
    -> Block a
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
                        Ok (mapFound (bounds.view details.id) details.found)

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
    , view : Id Float -> Float -> a
    }
    -> Block a
floatBetween bounds =
    let
        top =
            max bounds.min bounds.max

        bottom =
            min bounds.min bounds.max
    in
    Value
        { expect =
            ExpectFloatBetween
                { min = bounds.min
                , max = bounds.max
                , default = bounds.default
                }
        , converter =
            \desc ->
                case desc of
                    DescribeFloatBetween details ->
                        Ok (mapFound (\( str_, fl ) -> bounds.view details.id fl) details.found)

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
              , end = end
              }
            , val
            )
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
                DescribeText
                    { id = Id pos
                    , text = textNodes
                    }
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
errorToString : Error -> String
errorToString msg =
    formatErrorString msg


formatErrorString error =
    String.toUpper error.title
        ++ "\n\n"
        ++ String.join "" (List.map .text error.message)


{-| -}
type Theme
    = Dark
    | Light


{-| -}
errorToHtml : Theme -> Error -> List (Html.Html msg)
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
