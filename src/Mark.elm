module Mark exposing
    ( Document
    , Outcome(..), Partial
    , parse, Parsed, toString
    , compile, convert
    , document
    , Block, map
    , string, exactly, int, float, floatBetween, intBetween, bool, date, multiline
    , block, stub
    , oneOf, manyOf, startWith, nested
    , field, Field, record2, record3, record4, record5, record6, record7, record8, record9, record10
    , text, replacement, balanced, Replacement
    , Inline, token, annotation, attrString
    , Range, Position
    , Error, errorToString, errorToHtml, Theme(..)
    )

{-| `elm-markup` is about defining what you're expecting in a markup document.

The `elm-markup` language relies heavily on indentation, which is always some multiple of 4 spaces.

These are both useful for making live editors.

The `Description` can be

1.  Edited via the edits messages in `Mark.Description.Edit`.

Parsing is an intensive process. In the best case the Parser has to go through each character in a string to see if it's valid.

This can be an issue if you're trying to make an editor, because it could mean that every keystroke causes the whole document to be parsed!

A solution to this is to parse a `Document` once to an intermediate data structure, in our case that's a `Description`.

@docs Document

@docs Outcome, Partial

@docs parse, Parsed, toString

@docs compile, convert


## Building Documents

@docs document

@docs Block, map


## Primitives

@docs string, exactly, int, float, floatBetween, intBetween, bool, date, multiline

@docs block, stub

@docs oneOf, manyOf, startWith, nested


## Records

@docs field, Field, record2, record3, record4, record5, record6, record7, record8, record9, record10


## Handling Text and Inline

@docs text, replacement, balanced, Replacement

@docs Inline, token, annotation, attrString

@docs Range, Position


## Displaying Errors

@docs Error, errorToString, errorToHtml, Theme

-}

import Html
import Html.Attributes
import Iso8601
import Mark.Format as Format
import Mark.Internal.Description exposing (..)
import Mark.Internal.Error as Error exposing (Context(..), Problem(..))
import Mark.Internal.Id as Id exposing (..)
import Mark.Internal.Parser as Parse
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)
import Time


{-| -}
type alias Parsed =
    Mark.Internal.Description.Parsed



{- INTERFACE -}


{-| -}
type Outcome failure almost success
    = Success success
    | Almost almost
    | Failure failure


{-| -}
toString : Parsed -> String
toString =
    Mark.Internal.Description.toString


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
        , line = 1
        , column = 1
        }
    , end =
        { offset = 0
        , line = 1
        , column = 1
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
        , initialSeed : Id.Seed
        , currentSeed : Id.Seed
        , expect : Expectation
        , parser : Parser Context Problem Parsed
        }


{-| -}
type
    Block data
    {-
        A `Block data` is just a parser that results in `data`.

       You'll be building up your `Document` in terms of the `Blocks`.

       A block starts with `|` and has a name(already built into the parser)

       A value is just a raw parser.
    -}
    = Block
        String
        { converter : Description -> Result AstError (Found data)
        , expect : Expectation
        , parser : Id.Seed -> ( Id.Seed, Parser Context Problem Description )
        }
    | Value
        { converter : Description -> Result AstError (Found data)
        , expect : Expectation
        , parser : Id.Seed -> ( Id.Seed, Parser Context Problem Description )
        }


getParser : Id.Seed -> Block data -> ( Id.Seed, Parser Context Problem Description )
getParser seed fromBlock =
    case fromBlock of
        Block name { parser } ->
            let
                ( newSeed, blockParser ) =
                    parser seed
            in
            ( newSeed
            , Parser.succeed identity
                |. Parser.token (Parser.Token "|" (ExpectingBlockName name))
                |. Parser.chompIf (\c -> c == ' ') Space
                |= blockParser
            )

        Value { parser } ->
            parser seed


blockName : Block data -> Maybe String
blockName fromBlock =
    case fromBlock of
        Block name _ ->
            Just name

        Value _ ->
            Nothing


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

        DescribeString rng _ str ->
            []

        DescribeMultiline rng _ str ->
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
    , region : Range
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
    { error :
        { range : Range
        , problem : Error.Error
        }
        -> result
    , view : Range -> child -> result
    }
    -> Block child
    -> Document result
document doc child =
    let
        expectation =
            getBlockExpectation child

        seed =
            Id.initialSeed

        ( currentSeed, blockParser ) =
            getParser seed child
    in
    Document
        { expect = expectation
        , initialSeed = seed
        , currentSeed = currentSeed
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
                        , initialSeed = seed
                        , currentSeed = currentSeed
                        , focus = Nothing
                        }
                )
                |= Parser.getSource
                |= Parse.withRange
                    (Parser.withIndent 0 blockParser)
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
            skipSeed <|
                Parser.map
                    (\( range, _ ) ->
                        DescribeStub name (Found range name)
                    )
                    (Parse.withRange
                        (Parser.succeed ()
                            |. Parser.keyword (Parser.Token name (ExpectingBlockName name))
                            |. Parser.chompWhile (\c -> c == ' ')
                        )
                    )
        }


skipSeed parser seed =
    ( seed, parser )



-- TODO: use (backtrackCharacters 2 pos)


{-| -}
block :
    { name : String
    , view : Range -> child -> result
    , error :
        { range : Range
        , problem : Error.Error
        }
        -> result
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
            \seed ->
                let
                    ( newSeed, childParser ) =
                        getParser seed child
                in
                ( newSeed
                , Parser.map
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
                    Parse.withRange
                        (Parser.getIndent
                            |> Parser.andThen
                                (\indentation ->
                                    Parser.succeed identity
                                        |. Parser.keyword
                                            (Parser.Token blockDetails.name
                                                (ExpectingBlockName blockDetails.name)
                                            )
                                        |. Parser.chompWhile (\c -> c == ' ')
                                        |. skipBlankLineWith ()
                                        |= Parser.oneOf
                                            [ (Parser.succeed identity
                                                |= Parse.getPosition
                                                |. Parser.token
                                                    (Parser.Token
                                                        (String.repeat (indentation + 4) " ")
                                                        (ExpectingIndentation (indentation + 4))
                                                    )
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
                                                                |= Parse.getPosition
                                                                |. Parser.loop "" (raggedIndentedStringAbove indentation)
                                                            , Parser.map Ok <|
                                                                Parser.withIndent
                                                                    (indentation + 4)
                                                                    (Parser.inContext (InBlock blockDetails.name) childParser)
                                                            ]
                                                    )

                                            -- If we're here, it's because the indentation failed.
                                            -- If the child parser failed in some way, it would
                                            -- take care of that itself by returning Unexpected
                                            , Parser.succeed
                                                (\( pos, foundIndent ) ->
                                                    Err ( pos, Error.ExpectingIndent (indentation + 4) )
                                                )
                                                |= Parse.withRange (Parser.chompWhile (\c -> c == ' '))
                                                |. Parser.loop "" (raggedIndentedStringAbove indentation)
                                            ]
                                )
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
    , error :
        { range : Range
        , problem : Error.Error
        }
        -> result
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
            \seed ->
                let
                    ( startSeed, startParser ) =
                        getParser seed startBlock

                    ( remainSeed, endParser ) =
                        getParser startSeed endBlock
                in
                ( remainSeed
                , Parser.succeed
                    (\( range, ( begin, end ) ) ->
                        StartsWith range
                            { found = begin
                            , expected = getBlockExpectation startBlock
                            }
                            { found = end
                            , expected = getBlockExpectation endBlock
                            }
                    )
                    |= Parse.withRange
                        (Parser.succeed Tuple.pair
                            |= startParser
                            |. Parser.loop 0 manyBlankLines
                            |= endParser
                        )
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
        , range : Range
        , problem : Error.Error
        }
        -> b
    }
    -> List (Block a)
    -> Block b
oneOf oneOfDetails blocks =
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
                                                            , range = unexpected.range
                                                            , problem = unexpected.problem
                                                            , options =
                                                                List.map (Choice details.id) expectations
                                                            }
                                                        )
                                                    )

                            Unexpected unexpected ->
                                Ok
                                    (Found unexpected.range
                                        (oneOfDetails.error
                                            { id = details.id
                                            , problem = unexpected.problem
                                            , range = unexpected.range
                                            , options =
                                                List.map (Choice details.id) expectations
                                            }
                                        )
                                    )

                    _ ->
                        Err NoMatch
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
                            |. Parser.token (Parser.Token "|" BlockStart)
                            |. Parser.oneOf
                                [ Parser.chompIf (\c -> c == ' ') Space
                                , Parser.succeed ()
                                ]
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
                                                        |. newline
                                                        |. Parser.loop "" (raggedIndentedStringAbove indentation)
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


humanReadableExpectations expect =
    case expect of
        ExpectBlock name exp ->
            "| " ++ name

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
        , range : Range
        , problem : Error.Error
        }
        -> b
    , merge :
        { parent : Id ManyOptions
        , options : List (Choice (Id ManyOptions) Expectation)
        }
        -> List b
        -> final
    }
    -> List (Block a)
    -> Block final
manyOf manyOfDetails blocks =
    let
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
                                                , range = unexpected.range
                                                , problem = unexpected.problem
                                                }
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
                                                        , problem = unexpected.problem
                                                        , range = unexpected.range
                                                        }
                                                        :: existing
                                                    )
                                                , index + 1
                                                )
                in
                case desc of
                    ManyOf many ->
                        List.foldl (getRendered many.id many.choices) ( Ok [], 0 ) many.children
                            |> Tuple.first
                            |> Result.map
                                (\items ->
                                    Found many.range
                                        (manyOfDetails.merge
                                            { parent = many.id
                                            , options = many.choices
                                            }
                                            (List.reverse items)
                                        )
                                )

                    _ ->
                        Err NoMatch
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
                        (Parser.getIndent
                            |> Parser.andThen
                                (\indentation ->
                                    Parser.loop
                                        { parsedSomething = False
                                        , found = []
                                        , seed = childStart
                                        }
                                        (blocksOrNewlines
                                            indentation
                                            blocks
                                        )
                                )
                        )
                )
        }


{-| -}
blocksOrNewlines indentation blocks cursor =
    Parser.oneOf
        [ Parser.end End
            |> Parser.map
                (\_ ->
                    Parser.Done (List.reverse cursor.found)
                )
        , Parser.succeed
            (Parser.Loop
                { parsedSomething = True
                , found = cursor.found
                , seed = cursor.seed
                }
            )
            |. newlineWith "empty newline"
        , if not cursor.parsedSomething then
            -- First thing already has indentation accounted for.
            makeBlocksParser blocks cursor.seed
                |> Parser.map
                    (\foundBlock ->
                        let
                            ( _, newSeed ) =
                                Id.step cursor.seed
                        in
                        Parser.Loop
                            { parsedSomething = True
                            , found = foundBlock :: cursor.found
                            , seed = newSeed
                            }
                    )

          else
            Parser.oneOf
                [ Parser.succeed
                    (\foundBlock ->
                        let
                            ( _, newSeed ) =
                                Id.step cursor.seed
                        in
                        Parser.Loop
                            { parsedSomething = True
                            , found = foundBlock :: cursor.found
                            , seed = newSeed
                            }
                    )
                    |. Parser.token (Parser.Token (String.repeat indentation " ") (ExpectingIndentation indentation))
                    |= makeBlocksParser blocks cursor.seed
                , Parser.succeed
                    (Parser.Loop
                        { parsedSomething = True
                        , found = cursor.found
                        , seed = cursor.seed
                        }
                    )
                    |. Parser.backtrackable (Parser.chompWhile (\c -> c == ' '))
                    |. Parser.backtrackable newline

                -- We reach here because the indentation parsing was not successful,
                -- meaning the indentation has been lowered and the block is done
                , Parser.succeed (Parser.Done (List.reverse cursor.found))
                ]

        -- Whitespace Line
        , Parser.succeed
            (Parser.Loop
                { parsedSomething = True
                , found = cursor.found
                , seed = cursor.seed
                }
            )
            |. Parser.chompWhile (\c -> c == ' ')
            |. newlineWith "ws-line"
        ]


makeBlocksParser blocks seed =
    let
        gatherParsers myBlock details =
            let
                -- We don't care about the new seed because that's handled by the loop.
                ( _, parser ) =
                    getParser seed myBlock
            in
            case blockName myBlock of
                Just name ->
                    { blockNames = name :: details.blockNames
                    , childBlocks = Parser.map Ok parser :: details.childBlocks
                    , childValues = details.childValues
                    }

                Nothing ->
                    { blockNames = details.blockNames
                    , childBlocks = details.childBlocks
                    , childValues = Parser.map Ok (Parse.withRange parser) :: details.childValues
                    }

        children =
            List.foldl gatherParsers
                { blockNames = []
                , childBlocks = []
                , childValues = []
                }
                blocks

        blockParser =
            Parser.map
                (\( pos, result ) ->
                    Result.map (\desc -> ( pos, desc )) result
                )
                (Parse.withRange
                    (Parser.succeed identity
                        |. Parser.token (Parser.Token "|" BlockStart)
                        |. Parser.oneOf
                            [ Parser.chompIf (\c -> c == ' ') Space
                            , Parser.succeed ()
                            ]
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
                                                    |. newline
                                                    |. Parser.loop "" (raggedIndentedStringAbove indentation)
                                            )
                                   ]
                            )
                    )
                )
    in
    Parser.oneOf
        (blockParser
            :: List.reverse children.childValues
        )


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
            \seed ->
                -- TODO: AHHHH, A NEW SEED NEEDS TO GET CREATED
                ( seed
                , Parser.getIndent
                    |> Parser.andThen
                        (\baseIndent ->
                            Parser.map
                                (\( pos, result ) ->
                                    DescribeTree
                                        { found = ( pos, buildTree baseIndent result )
                                        , expected = expectation
                                        }
                                )
                                (Parse.withRange
                                    (Parser.loop
                                        ( { base = baseIndent
                                          , prev = baseIndent
                                          }
                                        , []
                                        )
                                        (indentedBlocksOrNewlines seed config.start config.item)
                                    )
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


type alias New =
    Expectation


{-| -}
record2 :
    { name : String
    , view :
        { range : Range
        }
        -> one
        -> two
        -> data
    , error :
        { range : Range
        , problem : Error.Error
        }
        -> data
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
                                    Ok (Ok (record.view { range = pos }))
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
            \seed ->
                let
                    ( newSeed, fields ) =
                        Id.thread seed
                            [ fieldParser field1
                            , fieldParser field2
                            ]
                in
                ( newSeed
                , parseRecord record.name
                    expectations
                    fields
                )
        }


{-| -}
record3 :
    { name : String
    , view : { range : Range } -> one -> two -> three -> data
    , error :
        { range : Range
        , problem : Error.Error
        }
        -> data
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
                                    Ok (Ok (record.view { range = pos }))
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
            \seed ->
                let
                    ( newSeed, fields ) =
                        Id.thread seed
                            [ fieldParser field1
                            , fieldParser field2
                            , fieldParser field3
                            ]
                in
                ( newSeed
                , parseRecord record.name
                    expectations
                    fields
                )
        }


{-| -}
record4 :
    { name : String
    , view : { range : Range } -> one -> two -> three -> four -> data
    , error :
        { range : Range
        , problem : Error.Error
        }
        -> data
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
                                    Ok (Ok (record.view { range = pos }))
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
            \seed ->
                let
                    ( newSeed, fields ) =
                        Id.thread seed
                            [ fieldParser field1
                            , fieldParser field2
                            , fieldParser field3
                            , fieldParser field4
                            ]
                in
                ( newSeed
                , parseRecord record.name
                    expectations
                    fields
                )
        }


{-| -}
record5 :
    { name : String
    , view : { range : Range } -> one -> two -> three -> four -> five -> data
    , error :
        { range : Range
        , problem : Error.Error
        }
        -> data
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
                , fieldExpectation field5
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
                                    Ok (Ok (record.view { range = pos }))
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
            \seed ->
                let
                    ( newSeed, fields ) =
                        Id.thread seed
                            [ fieldParser field1
                            , fieldParser field2
                            , fieldParser field3
                            , fieldParser field4
                            , fieldParser field5
                            ]
                in
                ( newSeed
                , parseRecord record.name
                    expectations
                    fields
                )
        }


{-| -}
record6 :
    { name : String
    , view : { range : Range } -> one -> two -> three -> four -> five -> six -> data
    , error :
        { range : Range
        , problem : Error.Error
        }
        -> data
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
                , fieldExpectation field5
                , fieldExpectation field6
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
                                    Ok (Ok (record.view { range = pos }))
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
            \seed ->
                let
                    ( newSeed, fields ) =
                        Id.thread seed
                            [ fieldParser field1
                            , fieldParser field2
                            , fieldParser field3
                            , fieldParser field4
                            , fieldParser field5
                            , fieldParser field6
                            ]
                in
                ( newSeed
                , parseRecord record.name
                    expectations
                    fields
                )
        }


{-| -}
record7 :
    { name : String
    , view : { range : Range } -> one -> two -> three -> four -> five -> six -> seven -> data
    , error :
        { range : Range
        , problem : Error.Error
        }
        -> data
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
                , fieldExpectation field5
                , fieldExpectation field6
                , fieldExpectation field7
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
                                    Ok (Ok (record.view { range = pos }))
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
            \seed ->
                let
                    ( newSeed, fields ) =
                        Id.thread seed
                            [ fieldParser field1
                            , fieldParser field2
                            , fieldParser field3
                            , fieldParser field4
                            , fieldParser field5
                            , fieldParser field6
                            , fieldParser field7
                            ]
                in
                ( newSeed
                , parseRecord record.name
                    expectations
                    fields
                )
        }


{-| -}
record8 :
    { name : String
    , view : { range : Range } -> one -> two -> three -> four -> five -> six -> seven -> eight -> data
    , error :
        { range : Range
        , problem : Error.Error
        }
        -> data
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
                , fieldExpectation field5
                , fieldExpectation field6
                , fieldExpectation field7
                , fieldExpectation field8
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
                                    Ok (Ok (record.view { range = pos }))
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
            \seed ->
                let
                    ( newSeed, fields ) =
                        Id.thread seed
                            [ fieldParser field1
                            , fieldParser field2
                            , fieldParser field3
                            , fieldParser field4
                            , fieldParser field5
                            , fieldParser field6
                            , fieldParser field7
                            , fieldParser field8
                            ]
                in
                ( newSeed
                , parseRecord record.name
                    expectations
                    fields
                )
        }


{-| -}
record9 :
    { name : String
    , view : { range : Range } -> one -> two -> three -> four -> five -> six -> seven -> eight -> nine -> data
    , error :
        { range : Range
        , problem : Error.Error
        }
        -> data
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
                , fieldExpectation field5
                , fieldExpectation field6
                , fieldExpectation field7
                , fieldExpectation field8
                , fieldExpectation field9
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
                                    Ok (Ok (record.view { range = pos }))
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
            \seed ->
                let
                    ( newSeed, fields ) =
                        Id.thread seed
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
                in
                ( newSeed
                , parseRecord record.name
                    expectations
                    fields
                )
        }


{-| -}
record10 :
    { name : String
    , view : { range : Range } -> one -> two -> three -> four -> five -> six -> seven -> eight -> nine -> ten -> data
    , error :
        { range : Range
        , problem : Error.Error
        }
        -> data
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
                , fieldExpectation field5
                , fieldExpectation field6
                , fieldExpectation field7
                , fieldExpectation field8
                , fieldExpectation field9
                , fieldExpectation field10
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
                                    Ok (Ok (record.view { range = pos }))
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
            \seed ->
                let
                    ( newSeed, fields ) =
                        Id.thread seed
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
                in
                ( newSeed
                , parseRecord record.name
                    expectations
                    fields
                )
        }



{- TEXT BLOCKS -}


{-| -}
type alias Styles =
    { bold : Bool
    , italic : Bool
    , strike : Bool
    }


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
    { view : { range : Range } -> Styles -> String -> rendered
    , error :
        { range : Range
        , problem : Error.Error
        }
        -> rendered
    , inlines : List (Inline rendered)
    , replacements : List Replacement
    }
    -> Block (List rendered)
text options =
    Value
        { expect = ExpectText (List.map getInlineExpectation options.inlines)
        , converter = renderText options
        , parser =
            \seed ->
                -- TODO:  probably need a seed for text editing.
                ( seed
                , Parse.getPosition
                    |> Parser.andThen
                        (\pos ->
                            Parse.styledText
                                { inlines = List.map getInlineExpectation options.inlines
                                , replacements = options.replacements
                                }
                                seed
                                pos
                                emptyStyles
                                []
                        )
                )
        }


renderText :
    { view : { range : Range } -> Styles -> String -> rendered
    , error : UnexpectedDetails -> rendered
    , inlines : List (Inline rendered)
    , replacements : List Replacement
    }
    -> Description
    -> Result AstError (Found (List rendered))
renderText options description =
    case description of
        DescribeText details ->
            List.foldl (convertTextDescription options) [] details.text
                |> (Found details.range << List.reverse)
                |> Ok

        _ ->
            Err NoMatch


convertTextDescription :
    { view : { range : Range } -> Styles -> String -> rendered
    , error : UnexpectedDetails -> rendered
    , inlines : List (Inline rendered)
    , replacements : List Replacement
    }
    -> TextDescription
    -> List rendered
    -> List rendered
convertTextDescription options comp found =
    -- case comp of
    --     Styled range textEl ->
    --         options.view { range = range } textEl :: found
    --     InlineToken details ->
    --         let
    --             convertedInline =
    --                 List.foldl
    --                     (convertMatchedInline details.attributes)
    --                     (Err NoMatch)
    --                     options.inlines
    --         in
    --         case convertedInline of
    --             Err err ->
    --                 options.error
    --                     { range = details.range
    --                     , problem = Error.UnknownInline (List.map (getInlineName << getInlineExpectation) options.inlines)
    --                     }
    --                     :: found
    --             Ok list ->
    --                 list ++ found
    --     InlineAnnotation details ->
    --         case List.foldl (renderInline details.name (List.reverse details.attributes)) (Err NoMatch) options.inlines of
    --             Err err ->
    --                 options.error
    --                     { range = details.range
    --                     , problem = Error.UnknownInline (List.map (getInlineName << getInlineExpectation) options.inlines)
    --                     }
    --                     :: found
    --             Ok list ->
    --                 list ++ found
    --     -- DescribeInline name range foundInline ->
    --     -- case List.foldl (renderInline name range (List.reverse foundInline)) (Err NoMatch) options.inlines of
    --     --     Err err ->
    --     --         options.error
    --     --             { range = range
    --     --             , problem = Error.UnknownInline (List.map (getInlineName << getInlineExpectation) options.inlines)
    --     --             }
    --     --             :: found
    --     --     Ok list ->
    --     --         list ++ found
    --     UnexpectedInline details ->
    --         options.error details :: found
    []


convertMatchedInline : List InlineAttribute -> Inline rendered -> Result AstError rendered -> Result AstError rendered
convertMatchedInline foundAttributes inline found =
    found



-- renderInline name pieces (Inline inlineName details) found =
--     case found of
--         Ok _ ->
--             found
--         Err error ->
--             if name == inlineName then
--                 -- inlineRenderer
--                 details.converter pieces
--             else
--                 found


type alias Replacement =
    Parse.Replacement



-- {-| -}
-- type Inline data
--     = Inline
--         String
--         { converter : List InlineDescription -> Result AstError (List data)
--         , expect : InlineExpectation
--         }
{- Inline Rewrite

   Valid States:

       [Some text]{link| attribute=anything, !yup }

       - You can attach attributes to a stretch of text.
       - Attributes can have a value.

       You can also define a token, which shows up like this.

       {token}

       It can also have attributes

       {token| attribute = yoyoyo!}

       For inline stuff, all names must be lowercase.

-}


type Inline data
    = Inline
        { converter : List InlineAttribute -> Result AstError (List data)
        , expect : InlineExpectation
        }


{-| -}
token : String -> result -> Inline result
token name result =
    Inline
        { converter =
            \descriptor ->
                Ok [ result ]
        , expect =
            ExpectToken name []
        }


{-| -}
annotation : String -> result -> Inline result
annotation name result =
    Inline
        { converter =
            \descriptor ->
                Ok [ result ]
        , expect =
            ExpectAnnotation name []
        }


{-| -}
attrString : String -> Inline (String -> result) -> Inline result
attrString name newInline =
    case newInline of
        Inline details ->
            Inline
                { converter =
                    \descriptors ->
                        case descriptors of
                            [] ->
                                Err NoMatch

                            (AttrString attr) :: remaining ->
                                case details.converter remaining of
                                    Err err ->
                                        Err err

                                    Ok renderers ->
                                        Ok (List.map (\x -> x attr.value) renderers)
                , expect =
                    case details.expect of
                        ExpectToken tokenName attrs ->
                            ExpectToken tokenName (ExpectAttrString name :: attrs)

                        ExpectAnnotation noteName attrs ->
                            ExpectAnnotation noteName (ExpectAttrString name :: attrs)
                }


getInlineExpectation (Inline details) =
    details.expect



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
                    DescribeMultiline id range str ->
                        Ok (Found range (details.view id str))

                    _ ->
                        Err NoMatch
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
                                    Parser.loop "" (indentedString indentation)
                                )
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
                    DescribeString id range str ->
                        Ok (Found range (details.view id str))

                    _ ->
                        Err NoMatch
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
            \seed ->
                let
                    ( id, newSeed ) =
                        Id.step seed
                in
                ( newSeed
                , Parser.map
                    (\( pos, parsedPosix ) ->
                        case parsedPosix of
                            Err str ->
                                DescribeDate
                                    { id = id
                                    , found =
                                        Unexpected
                                            { range = pos
                                            , problem = Error.BadDate str
                                            }
                                    }

                            Ok ( str, posix ) ->
                                DescribeDate
                                    { id = id
                                    , found = Found pos ( str, posix )
                                    }
                    )
                    (Parse.withRange
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
            skipSeed
                (Parser.succeed
                    (\start _ end ->
                        DescribeStringExactly { start = start, end = end } key
                    )
                    |= Parse.getPosition
                    |= Parser.token (Parser.Token key (Expecting key))
                    |= Parse.getPosition
                )
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
                                            , problem = Error.BadBool err
                                            }

                                    Ok b ->
                                        Found range
                                            b
                            }
                    )
                    (Parse.withRange
                        (Parser.oneOf
                            [ Parser.token (Parser.Token "True" (Expecting "True"))
                                |> Parser.map (always (Ok True))
                            , Parser.token (Parser.Token "False" (Expecting "False"))
                                |> Parser.map (always (Ok False))
                            , Parser.map Err Parse.word
                            ]
                        )
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
                    integer
                )
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
                        Ok
                            (mapFound
                                (\( str_, fl ) ->
                                    view details.id fl
                                )
                                details.found
                            )

                    _ ->
                        Err NoMatch
        , expect = ExpectFloat default
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
                            { id = id, found = fl }
                    )
                    floating
                )
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
            \seed ->
                let
                    ( id, newSeed ) =
                        Id.step seed
                in
                ( newSeed
                , Parser.map
                    (\found ->
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
                )
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
            \seed ->
                let
                    ( id, newSeed ) =
                        Id.step seed
                in
                ( newSeed
                , Parser.map
                    (\found ->
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
                )
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


type alias BlockOrNewlineCursor thing =
    { parsedSomething : Bool
    , found : List thing
    , seed : Id.Seed
    }


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
                        , problem = Error.BadInt str
                        }
        )
        (Parse.withRange
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
                    |= Parse.word
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
                        , problem = Error.BadFloat str
                        }
        )
        (Parse.withRange
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
                    |= Parse.word
                ]
            )
        )



{- PARSER HELPERS -}
{- RECORD HELPERS -}


{-| -}
type Field value
    = Field String (Block value)


{-| -}
field : String -> Block value -> Field value
field name child =
    Field name child


fieldParser : Field value -> Id.Seed -> ( Id.Seed, ( String, Parser Context Problem ( String, Found Description ) ) )
fieldParser (Field name myBlock) seed =
    let
        ( newSeed, blockParser ) =
            getParser seed myBlock
    in
    ( newSeed
    , ( name
      , withFieldName
            name
            blockParser
      )
    )


fieldName : Field v -> String
fieldName (Field name _) =
    name


fieldExpectation (Field name fieldBlock) =
    ( name, getBlockExpectation fieldBlock )



{- RECORD PARSER HELPERS -}


backtrackCharacters chars range =
    { start =
        { offset = range.start.offset - chars
        , line = range.start.line
        , column = range.start.column - chars
        }
    , end = range.end
    }


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
                            Found (backtrackCharacters 2 pos) ok
                        }

                Err ( maybePosition, problem ) ->
                    Record
                        { expected = expectations
                        , name = recordName
                        , found =
                            Unexpected
                                { range = Maybe.withDefault (backtrackCharacters 2 pos) maybePosition
                                , problem = problem
                                }
                        }
        )
        |= Parse.withRange
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
                    Parse.withRange
                        (Parser.succeed identity
                            |. Parser.keyword (Parser.Token name (ExpectingFieldName name))
                            |. Parser.chompWhile (\c -> c == ' ')
                            |. Parser.chompIf (\c -> c == '=') (Expecting "=")
                            |. Parser.chompWhile (\c -> c == ' ')
                            |= Parser.oneOf
                                [ Parser.withIndent (indentation + 4) (Parser.inContext (InRecordField name) parser)
                                , Parser.succeed identity
                                    |. Parser.chompWhile (\c -> c == '\n')
                                    |. Parser.token (Parser.Token (String.repeat (indentation + 4) " ") (ExpectingIndentation indentation))
                                    |= Parser.withIndent (indentation + 4) (Parser.inContext (InRecordField name) parser)
                                ]
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
                        |= Parse.withRange (Parser.getChompedString (Parser.chompWhile Char.isAlphaNum))
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
    { remaining :
        List ( String, Parser Context Problem ( String, Found Description ) )
    , found :
        Result ( Maybe Range, Error.Error ) (List ( String, Found Description ))
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
indentOrSkip :
    Int
    -> Parser Context Problem (Parser.Step RecordFields a)
    -> Parser Context Problem (Indented (Parser.Step RecordFields a))
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
                    |. newlineWith "indentOrSkip one"

                -- parse field
                , Parser.succeed Indented
                    |= successParser

                -- |. newlineWith "indentOrSkip two"
                ]

        -- We're here because there is less than the desired indent.
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


newlineWith x =
    Parser.token (Parser.Token "\n" (Expecting x))


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


captureField :
    List ( String, Found Description )
    -> String
    -> RecordFields
    -> List String
    -> Parser Context Problem (Parser.Step RecordFields a)
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


{-| We want to create a maker function,

a -> b -> c -> Expectation

where every call to \`createMakerField : adds a new argument to

How does a New get constructed?

    view :
        Create New thing
        , Add to Msg
        , update Self potentially

    error :
        Create a New Thing
        Add to Msg
        update Self

-}
createMakerField foundField possiblyMakerFn =
    case possiblyMakerFn of
        Err err ->
            Err err

        Ok fn ->
            case foundField of
                Found pos desc ->
                    Ok (fn desc)

                Unexpected unexpected ->
                    Err unexpected


{-| -}
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
    Id.Seed
    -> Block icon
    -> Block thing
    -> ( NestedIndex, List ( Int, Maybe Description, Description ) )
    -> Parser Context Problem (Parser.Step ( NestedIndex, List ( Int, Maybe Description, Description ) ) (List ( Int, Maybe Description, Description )))
indentedBlocksOrNewlines seed icon item ( indentation, existing ) =
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
                let
                    ( iconSeed, iconParser ) =
                        getParser seed icon

                    ( itemSeed, itemParser ) =
                        getParser iconSeed item
                in
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
                    |= iconParser
                    |= itemParser

            _ ->
                Parser.oneOf
                    [ -- block with required indent
                      expectIndentation indentation.base indentation.prev
                        |> Parser.andThen
                            (\newIndent ->
                                let
                                    ( iconSeed, iconParser ) =
                                        getParser seed icon

                                    ( itemSeed, itemParser ) =
                                        getParser iconSeed item
                                in
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
                                            |= iconParser
                                            |= itemParser
                                         )
                                            :: (if newIndent - 4 == indentation.prev then
                                                    [ itemParser
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
    Parse.Replacement


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
    Parse.Balanced



{- TEXT HELPERS -}


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
