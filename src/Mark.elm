module Mark exposing
    ( Document
    , Outcome(..), Partial
    , compile, parse, Parsed, toString, render
    , document
    , Block, map, verify, onError
    , string, int, float, bool, multiline
    , block, oneOf, manyOf, startWith
    , tree, Tree(..)
    , field, Field, record2, record3, record4, record5, record6, record7, record8, record9, record10
    , Text(..), Styles, text, textWith, replacement, balanced, Replacement
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

@docs compile, parse, Parsed, toString, render


## Building Documents

@docs document

@docs Block, map, verify, onError


## Primitives

@docs string, int, float, bool, multiline


## Higher Level

@docs block, oneOf, manyOf, startWith


## Trees

@docs tree, Tree


## Records

@docs field, Field, record2, record3, record4, record5, record6, record7, record8, record9, record10


## Handling Text and Inline

@docs Text, Styles, text, textWith, replacement, balanced, Replacement

@docs Inline, token, annotation, attrString

@docs Range, Position


## Displaying Errors

@docs Error, errorToString, errorToHtml, Theme

-}

import Html
import Html.Attributes
import Mark.Format as Format
import Mark.Internal.Description as Desc exposing (..)
import Mark.Internal.Error as Error exposing (AstError(..), Context(..), Problem(..))
import Mark.Internal.Id as Id exposing (..)
import Mark.Internal.Outcome as Outcome
import Mark.Internal.Parser as Parse
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)
import Time


{-| -}
type alias Parsed =
    Desc.Parsed



{- INTERFACE -}


{-| -}
toString : Parsed -> String
toString =
    Desc.toString


{-| -}
parse : Document data -> String -> Outcome Error (Partial Parsed) Parsed
parse doc source =
    case Desc.parse doc source of
        Ok ((Parsed parsedDetails) as parsed) ->
            case parsedDetails.errors of
                [] ->
                    Success parsed

                _ ->
                    Almost
                        { errors = List.map (Error.render source) parsedDetails.errors
                        , result = parsed
                        }

        Err deadEnds ->
            Failure <|
                Error.render source
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


errorsToList ( fst, remain ) =
    fst :: remain


{-| -}
render : Document data -> Parsed -> Outcome (List Error) (Partial data) data
render doc ((Parsed parsedDetails) as parsed) =
    Desc.render doc parsed
        |> errorsToMessages


{-| -}
compile : Document data -> String -> Outcome (List Error) (Partial data) data
compile doc source =
    Desc.compile doc source
        |> errorsToMessages


errorsToMessages outcome =
    case outcome of
        Outcome.Success s ->
            Success s

        Outcome.Almost { errors, result } ->
            Almost { errors = List.map (Error.render "") errors, result = result }

        Outcome.Failure errors ->
            Failure (List.map (Error.render "") errors)


{-| -}
type alias Document data =
    Desc.Document data


{-| -}
type Outcome failure almost success
    = Success success
    | Almost almost
    | Failure failure


{-| This doesn't cause something to completely fail, but logs the error.
-}
logErrors :
    ( ErrorWithRange, List ErrorWithRange )
    -> Outcome.Outcome f (Uncertain success) success
    -> Outcome.Outcome f (Uncertain success) success
logErrors (( fst, remainingErrs ) as err) outcome =
    case outcome of
        Outcome.Success s ->
            Outcome.Almost (Recovered err s)

        Outcome.Almost (Uncertain u) ->
            Outcome.Almost (Uncertain (mergeErrors u err))

        Outcome.Almost (Recovered e a) ->
            Outcome.Almost (Recovered (mergeErrors e err) a)

        Outcome.Failure failures ->
            Outcome.Failure failures


{-| -}
uncertain : ErrorWithRange -> Outcome.Outcome AstError (Uncertain data) data
uncertain err =
    Outcome.Almost (Uncertain ( err, [] ))


{-| -}
type alias Block data =
    Desc.Block data



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
            -- TODO: Get unexpecteds!!
            []

        -- List.concatMap getNestedUnexpecteds (Tuple.second details.found)
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

        DescribeNothing ->
            []



-- getNestedUnexpecteds (Nested nest) =
--     case nest.content of
--         ( desc, items ) ->
--             getUnexpecteds desc
--                 ++ List.concatMap
--                     getUnexpecteds
--                     nest.content


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
    (child -> result)
    -> Block child
    -> Document result
document view child =
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
                            Outcome.Success renderedChild ->
                                Outcome.Success (view renderedChild)

                            Outcome.Failure err ->
                                Outcome.Failure err

                            Outcome.Almost (Uncertain unexpected) ->
                                Outcome.Almost (Uncertain unexpected)

                            Outcome.Almost (Recovered errors renderedChild) ->
                                Outcome.Almost (Recovered errors (view renderedChild))

                    Unexpected unexpected ->
                        Outcome.Almost (Uncertain ( unexpected, [] ))
        , parser =
            Parser.succeed
                (\source ( range, val ) ->
                    Parsed
                        { errors = getUnexpecteds val
                        , found = Found range val
                        , expected = getBlockExpectation child
                        , initialSeed = seed
                        , currentSeed = currentSeed
                        }
                )
                |= Parser.getSource
                |= Parse.withRange
                    (Parser.withIndent 0 blockParser)
                |. Parser.chompWhile (\c -> c == ' ' || c == '\n')
                |. Parser.end End
        }


{-| Change the result of a block by applying a function to it.
-}
map : (a -> b) -> Block a -> Block b
map fn child =
    case child of
        Block name details ->
            Block name
                { converter = mapSuccessAndRecovered fn << details.converter
                , parser = details.parser
                , expect = details.expect
                }

        Value details ->
            Value
                { converter = mapSuccessAndRecovered fn << details.converter
                , parser = details.parser
                , expect = details.expect
                }


{-| -}
type alias CustomError =
    { title : String
    , message : List String
    }


{-| -}
verify : (a -> Result CustomError b) -> Block a -> Block b
verify fn myBlock =
    case myBlock of
        Block name details ->
            Block name
                { expect = details.expect
                , parser = details.parser
                , converter =
                    \desc ->
                        case details.converter desc of
                            Outcome.Success a ->
                                case fn a of
                                    Ok new ->
                                        Outcome.Success new

                                    Err newErr ->
                                        uncertain
                                            { problem = Error.Custom newErr
                                            , range = startDocRange
                                            }

                            Outcome.Almost (Recovered err a) ->
                                case fn a of
                                    Ok new ->
                                        Outcome.Almost (Recovered err new)

                                    Err newErr ->
                                        uncertain
                                            { problem = Error.Custom newErr
                                            , range = startDocRange
                                            }

                            Outcome.Almost (Uncertain x) ->
                                Outcome.Almost (Uncertain x)

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
                                case fn a of
                                    Ok new ->
                                        Outcome.Success new

                                    Err newErr ->
                                        uncertain
                                            { problem = Error.Custom newErr
                                            , range = startDocRange
                                            }

                            Outcome.Almost (Recovered err a) ->
                                case fn a of
                                    Ok new ->
                                        Outcome.Almost (Recovered err new)

                                    Err newErr ->
                                        uncertain
                                            { problem = Error.Custom newErr
                                            , range = startDocRange
                                            }

                            Outcome.Almost (Uncertain x) ->
                                Outcome.Almost (Uncertain x)

                            Outcome.Failure f ->
                                Outcome.Failure f
                }


{-| -}
onError : (List UnexpectedDetails -> a) -> Block a -> Block a
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

                            Outcome.Almost (Uncertain x) ->
                                Outcome.Almost (Recovered x (recover (errorsToList x)))

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

                            Outcome.Almost (Uncertain x) ->
                                Outcome.Almost (Recovered x (recover (errorsToList x)))

                            Outcome.Failure f ->
                                Outcome.Failure f
                }


{-| -}
mapFound : (a -> b) -> Found a -> Found b
mapFound fn found =
    case found of
        Found range item ->
            Found range (fn item)

        Unexpected unexp ->
            Unexpected unexp



-- {-| -}
-- andThenFound : (a -> Result CustomError b) -> Found a -> Found b
-- andThenFound fn found =
--     case found of
--         Found range item ->
--             Found range (fn item)
--         Unexpected unexp ->
--             Unexpected unexp
-- {-| -}
-- stub : String -> (Range -> result) -> Block result
-- stub name renderer =
--     Block name
--         { expect = ExpectStub name
--         , converter =
--             \desc ->
--                 case desc of
--                     DescribeStub actualBlockName found ->
--                         if actualBlockName == name then
--                             case found of
--                                 Found range _ ->
--                                     Ok (Found range (renderer range))
--                                 Unexpected unexpected ->
--                                     Outcome.Almost unexpected
--                         else
--                             Outcome.Failure NoMatch
--                     _ ->
--                         Outcome.Failure NoMatch
--         , parser =
--             skipSeed <|
--                 Parser.map
--                     (\( range, _ ) ->
--                         DescribeStub name (Found range name)
--                     )
--                     (Parse.withRange
--                         (Parser.succeed ()
--                             |. Parser.keyword (Parser.Token name (ExpectingBlockName name))
--                             |. Parser.chompWhile (\c -> c == ' ')
--                         )
--                     )
--         }


skipSeed parser seed =
    ( seed, parser )



-- TODO: use (backtrackCharacters 2 pos)


{-| -}
block :
    String
    -> (child -> result)
    -> Block child
    -> Block result
block name view child =
    Block name
        { expect = ExpectBlock name (getBlockExpectation child)
        , converter =
            \desc ->
                case desc of
                    DescribeBlock details ->
                        if details.name == name then
                            case details.found of
                                Found range found ->
                                    renderBlock child found
                                        |> mapSuccessAndRecovered view

                                Unexpected unexpected ->
                                    uncertain unexpected

                        else
                            -- This is not the block that was expected.
                            Outcome.Failure NoMatch

                    _ ->
                        Outcome.Failure NoMatch
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
                    Parse.withRange
                        (Parser.getIndent
                            |> Parser.andThen
                                (\indentation ->
                                    Parser.succeed identity
                                        |. Parser.keyword
                                            (Parser.Token name
                                                (ExpectingBlockName name)
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
                                                                |. Parser.loop "" (Parse.raggedIndentedStringAbove indentation)
                                                            , Parser.map Ok <|
                                                                Parser.withIndent
                                                                    (indentation + 4)
                                                                    (Parser.inContext (InBlock name) childParser)
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
                                                |. Parser.loop "" (Parse.raggedIndentedStringAbove indentation)
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


mergeWith fn one two =
    case ( one, two ) of
        ( Outcome.Success renderedOne, Outcome.Success renderedTwo ) ->
            Outcome.Success (fn renderedOne renderedTwo)

        ( Outcome.Almost (Recovered firstErrs fst), Outcome.Almost (Recovered secondErrs snd) ) ->
            Outcome.Almost
                (Recovered
                    (mergeErrors firstErrs secondErrs)
                    (fn fst snd)
                )

        ( Outcome.Almost (Uncertain unexpected), _ ) ->
            Outcome.Almost (Uncertain unexpected)

        ( _, Outcome.Almost (Uncertain unexpected) ) ->
            Outcome.Almost (Uncertain unexpected)

        _ ->
            Outcome.Failure NoMatch


{-| -}
startWith :
    (start -> rest -> result)
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
                        mergeWith fn
                            (renderBlock startBlock start.found)
                            (renderBlock endBlock end.found)

                    _ ->
                        Outcome.Failure NoMatch
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
oneOf : List (Block a) -> Block a
oneOf blocks =
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
                                List.foldl (matchBlock found) (Outcome.Failure NoMatch) blocks

                            Unexpected unexpected ->
                                uncertain unexpected

                    _ ->
                        Outcome.Failure NoMatch
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


{-| Many blocks that are all at the same indentation level.
-}
manyOf :
    List (Block a)
    -> Block (List a)
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
            |. Parse.newlineWith "empty Parse.newline"
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
                    |. Parser.backtrackable Parse.newline

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
            |. Parse.newlineWith "ws-line"
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
                                                    |. Parse.newline
                                                    |. Parser.loop "" (Parse.raggedIndentedStringAbove indentation)
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


{-| -}
type Tree item
    = Tree
        { index : List Int
        , icon : Icon
        , content : List item
        , children :
            List (Tree item)
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

    Mark.nested "List"
        ((Nested nested) ->
        -- Do something with nested.content and nested.children
        )
        text

**Note** the indentation is always a multiple of 4.

-}
tree :
    String
    -> (Tree item -> result)
    -> Block item
    -> Block (List result)
tree name view contentBlock =
    let
        expectation =
            ExpectTree (getBlockExpectation contentBlock)
    in
    Block name
        { expect = expectation
        , converter =
            \description ->
                case description of
                    DescribeTree details ->
                        case details.found of
                            ( pos, nestedDescriptors ) ->
                                nestedDescriptors
                                    |> reduceRender (renderTreeNodeSmall contentBlock)
                                    |> mapSuccessAndRecovered (List.map view)

                    _ ->
                        Outcome.Failure NoMatch
        , parser =
            \seed ->
                -- TODO: AHHHH, A NEW SEED NEEDS TO GET CREATED
                ( seed
                , Parser.getIndent
                    |> Parser.andThen
                        (\baseIndent ->
                            Parser.succeed identity
                                |. Parser.keyword
                                    (Parser.Token name
                                        (ExpectingBlockName name)
                                    )
                                |. Parser.chompWhile (\c -> c == ' ')
                                |. skipBlankLineWith ()
                                |= Parser.map
                                    (\( pos, result ) ->
                                        DescribeTree
                                            { found = ( pos, buildTree (baseIndent + 4) result )
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
                                            (indentedBlocksOrNewlines
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
        [ Parser.succeed Bullet
            |. Parser.chompIf (\c -> c == '-') (Expecting "-")
            |. Parser.chompWhile (\c -> c == '-')
        , Parser.succeed AutoNumber
            |. Parser.chompIf (\c -> c == '#') (Expecting "#")
            |. Parser.chompWhile (\c -> c == '.')
        ]


{-| -}
type alias Index =
    List Int



-- {-| -}
-- foldNestedList : (Index -> a -> b -> b) -> b -> List (Nested a) -> b
-- foldNestedList fn accum nodes =
--     List.foldl
--         (\child ( i, gathered ) ->
--             ( i + 1, foldNestedHelper [ i ] fn gathered child )
--         )
--         ( 1, accum )
--         nodes
--         |> Tuple.second
-- {-| -}
-- foldNested : (Index -> a -> b -> b) -> b -> Nested a -> b
-- foldNested fn accum node =
--     foldNestedHelper [ 1 ] fn accum node
-- foldNestedHelper : Index -> (Index -> a -> b -> b) -> b -> Nested a -> b
-- foldNestedHelper index fn accum (Nested node) =
--     let
--         newIndex =
--             1 :: index
--         advanced =
--             fn newIndex node.content accum
--     in
--     List.foldl
--         (\child ( i, gathered ) ->
--             ( i + 1, foldNestedHelper (i :: newIndex) fn gathered child )
--         )
--         ( 1, advanced )
--         node.children
--         |> Tuple.second


{-| -}
record2 :
    String
    ->
        (one
         -> two
         -> data
        )
    -> Field one
    -> Field two
    -> Block data
record2 name view field1 field2 =
    let
        expectations =
            ExpectRecord name
                [ fieldExpectation field1
                , fieldExpectation field2
                ]
    in
    Block name
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record details ->
                        if details.name == name then
                            case details.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok view)
                                        |> Result.map2 applyField (getField field1 fieldDescriptions)
                                        |> Result.map2 applyField (getField field2 fieldDescriptions)
                                        |> renderRecordResult pos

                                Unexpected unexpected ->
                                    uncertain unexpected

                        else
                            Outcome.Failure NoMatch

                    _ ->
                        Outcome.Failure NoMatch
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
                , parseRecord name
                    expectations
                    fields
                )
        }


{-| -}
record3 :
    String
    -> (one -> two -> three -> data)
    -> Field one
    -> Field two
    -> Field three
    -> Block data
record3 name view field1 field2 field3 =
    let
        expectations =
            ExpectRecord name
                [ fieldExpectation field1
                , fieldExpectation field2
                , fieldExpectation field3
                ]
    in
    Block name
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record details ->
                        if details.name == name then
                            case details.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok view)
                                        |> Result.map2 applyField (getField field1 fieldDescriptions)
                                        |> Result.map2 applyField (getField field2 fieldDescriptions)
                                        |> Result.map2 applyField (getField field3 fieldDescriptions)
                                        |> renderRecordResult pos

                                Unexpected unexpected ->
                                    uncertain unexpected

                        else
                            Outcome.Failure NoMatch

                    _ ->
                        Outcome.Failure NoMatch
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
                , parseRecord name
                    expectations
                    fields
                )
        }


{-| -}
record4 :
    String
    -> (one -> two -> three -> four -> data)
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Block data
record4 name view field1 field2 field3 field4 =
    let
        expectations =
            ExpectRecord name
                [ fieldExpectation field1
                , fieldExpectation field2
                , fieldExpectation field3
                , fieldExpectation field4
                ]
    in
    Block name
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record details ->
                        if details.name == name then
                            case details.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok view)
                                        |> Result.map2 applyField (getField field1 fieldDescriptions)
                                        |> Result.map2 applyField (getField field2 fieldDescriptions)
                                        |> Result.map2 applyField (getField field3 fieldDescriptions)
                                        |> Result.map2 applyField (getField field4 fieldDescriptions)
                                        |> renderRecordResult pos

                                Unexpected unexpected ->
                                    uncertain unexpected

                        else
                            Outcome.Failure NoMatch

                    _ ->
                        Outcome.Failure NoMatch
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
                , parseRecord name
                    expectations
                    fields
                )
        }


{-| -}
record5 :
    String
    -> (one -> two -> three -> four -> five -> data)
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Field five
    -> Block data
record5 name view field1 field2 field3 field4 field5 =
    let
        expectations =
            ExpectRecord name
                [ fieldExpectation field1
                , fieldExpectation field2
                , fieldExpectation field3
                , fieldExpectation field4
                , fieldExpectation field5
                ]
    in
    Block name
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record details ->
                        if details.name == name then
                            case details.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok view)
                                        |> Result.map2 applyField (getField field1 fieldDescriptions)
                                        |> Result.map2 applyField (getField field2 fieldDescriptions)
                                        |> Result.map2 applyField (getField field3 fieldDescriptions)
                                        |> Result.map2 applyField (getField field4 fieldDescriptions)
                                        |> Result.map2 applyField (getField field5 fieldDescriptions)
                                        |> renderRecordResult pos

                                Unexpected unexpected ->
                                    uncertain unexpected

                        else
                            Outcome.Failure NoMatch

                    _ ->
                        Outcome.Failure NoMatch
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
                , parseRecord name
                    expectations
                    fields
                )
        }


{-| -}
record6 :
    String
    -> (one -> two -> three -> four -> five -> six -> data)
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Field five
    -> Field six
    -> Block data
record6 name view field1 field2 field3 field4 field5 field6 =
    let
        expectations =
            ExpectRecord name
                [ fieldExpectation field1
                , fieldExpectation field2
                , fieldExpectation field3
                , fieldExpectation field4
                , fieldExpectation field5
                , fieldExpectation field6
                ]
    in
    Block name
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record details ->
                        if details.name == name then
                            case details.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok view)
                                        |> Result.map2 applyField (getField field1 fieldDescriptions)
                                        |> Result.map2 applyField (getField field2 fieldDescriptions)
                                        |> Result.map2 applyField (getField field3 fieldDescriptions)
                                        |> Result.map2 applyField (getField field4 fieldDescriptions)
                                        |> Result.map2 applyField (getField field5 fieldDescriptions)
                                        |> Result.map2 applyField (getField field6 fieldDescriptions)
                                        |> renderRecordResult pos

                                Unexpected unexpected ->
                                    uncertain unexpected

                        else
                            Outcome.Failure NoMatch

                    _ ->
                        Outcome.Failure NoMatch
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
                , parseRecord name
                    expectations
                    fields
                )
        }


{-| -}
record7 :
    String
    -> (one -> two -> three -> four -> five -> six -> seven -> data)
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Field five
    -> Field six
    -> Field seven
    -> Block data
record7 name view field1 field2 field3 field4 field5 field6 field7 =
    let
        expectations =
            ExpectRecord name
                [ fieldExpectation field1
                , fieldExpectation field2
                , fieldExpectation field3
                , fieldExpectation field4
                , fieldExpectation field5
                , fieldExpectation field6
                , fieldExpectation field7
                ]
    in
    Block name
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record details ->
                        if details.name == name then
                            case details.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok view)
                                        |> Result.map2 applyField (getField field1 fieldDescriptions)
                                        |> Result.map2 applyField (getField field2 fieldDescriptions)
                                        |> Result.map2 applyField (getField field3 fieldDescriptions)
                                        |> Result.map2 applyField (getField field4 fieldDescriptions)
                                        |> Result.map2 applyField (getField field5 fieldDescriptions)
                                        |> Result.map2 applyField (getField field6 fieldDescriptions)
                                        |> Result.map2 applyField (getField field7 fieldDescriptions)
                                        |> renderRecordResult pos

                                Unexpected unexpected ->
                                    uncertain unexpected

                        else
                            Outcome.Failure NoMatch

                    _ ->
                        Outcome.Failure NoMatch
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
                , parseRecord name
                    expectations
                    fields
                )
        }


{-| -}
record8 :
    String
    -> (one -> two -> three -> four -> five -> six -> seven -> eight -> data)
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Field five
    -> Field six
    -> Field seven
    -> Field eight
    -> Block data
record8 name view field1 field2 field3 field4 field5 field6 field7 field8 =
    let
        expectations =
            ExpectRecord name
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
    Block name
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record details ->
                        if details.name == name then
                            case details.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok view)
                                        |> Result.map2 applyField (getField field1 fieldDescriptions)
                                        |> Result.map2 applyField (getField field2 fieldDescriptions)
                                        |> Result.map2 applyField (getField field3 fieldDescriptions)
                                        |> Result.map2 applyField (getField field4 fieldDescriptions)
                                        |> Result.map2 applyField (getField field5 fieldDescriptions)
                                        |> Result.map2 applyField (getField field6 fieldDescriptions)
                                        |> Result.map2 applyField (getField field7 fieldDescriptions)
                                        |> Result.map2 applyField (getField field8 fieldDescriptions)
                                        |> renderRecordResult pos

                                Unexpected unexpected ->
                                    uncertain unexpected

                        else
                            Outcome.Failure NoMatch

                    _ ->
                        Outcome.Failure NoMatch
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
                , parseRecord name
                    expectations
                    fields
                )
        }


{-| -}
record9 :
    String
    -> (one -> two -> three -> four -> five -> six -> seven -> eight -> nine -> data)
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
record9 name view field1 field2 field3 field4 field5 field6 field7 field8 field9 =
    let
        expectations =
            ExpectRecord name
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
    Block name
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record details ->
                        if details.name == name then
                            case details.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok view)
                                        |> Result.map2 applyField (getField field1 fieldDescriptions)
                                        |> Result.map2 applyField (getField field2 fieldDescriptions)
                                        |> Result.map2 applyField (getField field3 fieldDescriptions)
                                        |> Result.map2 applyField (getField field4 fieldDescriptions)
                                        |> Result.map2 applyField (getField field5 fieldDescriptions)
                                        |> Result.map2 applyField (getField field6 fieldDescriptions)
                                        |> Result.map2 applyField (getField field7 fieldDescriptions)
                                        |> Result.map2 applyField (getField field8 fieldDescriptions)
                                        |> Result.map2 applyField (getField field9 fieldDescriptions)
                                        |> renderRecordResult pos

                                Unexpected unexpected ->
                                    uncertain unexpected

                        else
                            Outcome.Failure NoMatch

                    _ ->
                        Outcome.Failure NoMatch
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
                , parseRecord name
                    expectations
                    fields
                )
        }


{-| -}
record10 :
    String
    -> (one -> two -> three -> four -> five -> six -> seven -> eight -> nine -> ten -> data)
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
record10 name view field1 field2 field3 field4 field5 field6 field7 field8 field9 field10 =
    let
        expectations =
            ExpectRecord name
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
    Block name
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record details ->
                        if details.name == name then
                            case details.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok view)
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
                                        |> renderRecordResult pos

                                Unexpected unexpected ->
                                    uncertain unexpected

                        else
                            Outcome.Failure NoMatch

                    _ ->
                        Outcome.Failure NoMatch
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
                , parseRecord name
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


{-| -}
type Text
    = Text Styles String


{-|

    Mark.text (\styles string -> Html.span [] [ Html.text string ])

-}
text :
    (Text -> rendered)
    -> Block (List rendered)
text view =
    Value
        { expect = ExpectText []
        , converter =
            renderText
                { view = view
                , inlines = []
                , replacements = []
                }
        , parser =
            \seed ->
                -- TODO:  probably need a seed for text editing.
                ( seed
                , Parse.getPosition
                    |> Parser.andThen
                        (\pos ->
                            Parse.styledText
                                { inlines = []
                                , replacements = []
                                }
                                seed
                                pos
                                emptyStyles
                                []
                        )
                )
        }


{-| Handling formatted text is a little more involved than may be initially apparent.
Text styling can be overlapped such as
/My italicized sentence can have _bold_/
In order to render this, the above sentence is chopped up into `Text` fragments that can have multiple styles active.

  - `view` is the function to render an individual fragment.
  - `inlines` are custom inline blocks. These are how links are implemented in `Mark.Default`!
  - `replacements` will replace characters before rendering. For example, we can replace `...` with the real ellipses unicode character, `…`.
    **Note** check out `Mark.Default.text` to see an example.

-}
textWith :
    { view : Text -> rendered
    , inlines : List (Inline rendered)
    , replacements : List Replacement
    }
    -> Block (List rendered)
textWith options =
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


type alias Cursor data =
    Outcome.Outcome AstError (Uncertain data) data


renderText :
    { view : Text -> rendered
    , inlines : List (Inline rendered)
    , replacements : List Replacement
    }
    -> Description
    -> Cursor (List rendered)
renderText options description =
    case description of
        DescribeText details ->
            List.foldl (convertTextDescription options) (Outcome.Success []) details.text
                |> mapSuccessAndRecovered List.reverse

        _ ->
            Outcome.Failure NoMatch


textToText (Desc.Text styling txt) =
    Text styling txt


convertTextDescription :
    { view : Text -> rendered
    , inlines : List (Inline rendered)
    , replacements : List Replacement
    }
    -> TextDescription
    -> Cursor (List rendered)
    -> Cursor (List rendered)
convertTextDescription options comp cursor =
    case comp of
        Styled range (Desc.Text styling str) ->
            mergeWith (::) (Outcome.Success (options.view (Text styling str))) cursor

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
                        (matchedInline.converter (List.map textToText details.text) details.attributes)
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


type alias Replacement =
    Parse.Replacement



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
        { converter : List Text -> List InlineAttribute -> Outcome.Outcome AstError (Uncertain (List data)) (List data)
        , expect : InlineExpectation
        , name : String
        }


{-| -}
token : String -> result -> Inline result
token name result =
    Inline
        { converter =
            \_ attrs ->
                Outcome.Success [ result ]
        , expect =
            ExpectToken name []
        , name = name
        }


isToken : Inline data -> Bool
isToken (Inline inline) =
    case inline.expect of
        ExpectToken _ _ ->
            True

        _ ->
            False


{-| -}
annotation : String -> (List Text -> result) -> Inline result
annotation name result =
    Inline
        { converter =
            \textPieces attrs ->
                Outcome.Success [ result textPieces ]
        , expect =
            ExpectAnnotation name []
        , name = name
        }


{-| -}
attrString : String -> Inline (String -> result) -> Inline result
attrString name newInline =
    case newInline of
        Inline details ->
            Inline
                { converter =
                    \textPieces attrs ->
                        case attrs of
                            [] ->
                                Outcome.Failure NoMatch

                            (AttrString attr) :: remaining ->
                                details.converter textPieces remaining
                                    |> mapSuccessAndRecovered (List.map (\x -> x attr.value))
                , expect =
                    case details.expect of
                        ExpectToken tokenName attrs ->
                            ExpectToken tokenName (ExpectAttrString name :: attrs)

                        ExpectAnnotation noteName attrs ->
                            ExpectAnnotation noteName (ExpectAttrString name :: attrs)
                , name = details.name
                }


getInlineExpectation (Inline details) =
    details.expect



{- PRIMITIVE BLOCKS -}


{-| -}
multiline : Block String
multiline =
    Value
        { expect = ExpectMultiline "REPLACE"
        , converter =
            \desc ->
                case desc of
                    DescribeMultiline id range str ->
                        Outcome.Success str

                    _ ->
                        Outcome.Failure NoMatch
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


{-| -}
string : Block String
string =
    Value
        { expect = ExpectString "-- Replace Me --"
        , converter =
            \desc ->
                case desc of
                    DescribeString id range str ->
                        Outcome.Success str

                    _ ->
                        Outcome.Failure NoMatch
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


foundToResult found err =
    case found of
        Found _ b ->
            Ok b

        _ ->
            Err err


{-| Parse either `True` or `False`.
-}
bool : Block Bool
bool =
    Value
        { expect = ExpectBoolean False
        , converter =
            \desc ->
                case desc of
                    DescribeBoolean details ->
                        foundToOutcome details.found

                    _ ->
                        Outcome.Failure NoMatch
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


foundToOutcome found =
    case found of
        Found rng i ->
            Outcome.Success i

        Unexpected unexpected ->
            Outcome.Almost (Uncertain ( unexpected, [] ))


{-| Parse an `Int` block.
-}
int : Block Int
int =
    Value
        { converter =
            \desc ->
                case desc of
                    DescribeInteger details ->
                        foundToOutcome details.found

                    _ ->
                        Outcome.Failure NoMatch
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
float : Block Float
float =
    Value
        { converter =
            \desc ->
                case desc of
                    DescribeFloat details ->
                        foundToOutcome details.found
                            |> Outcome.mapSuccess Tuple.second

                    _ ->
                        Outcome.Failure NoMatch
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
                            { id = id, found = fl }
                    )
                    Parse.float
                )
        }



{- Parser Heleprs -}


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

                Err ( maybePosition, prob ) ->
                    Record
                        { expected = expectations
                        , name = recordName
                        , found =
                            Unexpected
                                { range = Maybe.withDefault (backtrackCharacters 2 pos) maybePosition
                                , problem = prob
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
                     -- |. Parse.newline
                     -- |. Parser.map (Debug.log "unexpected capture") (Parser.loop "" (raggedIndentedStringAbove (indent - 4)))
                    )
            )


resultToFound result =
    case result of
        Ok ( range, desc ) ->
            Found range desc

        Err ( range, prob ) ->
            Unexpected
                { range = range
                , problem = prob
                }


renderRecordResult pos result =
    case result of
        Ok parsedCorrectly ->
            case parsedCorrectly of
                Ok rendered ->
                    Outcome.Success rendered

                Err unexpected ->
                    uncertain unexpected

        Err prob ->
            uncertain
                { problem = prob
                , range = pos
                }


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

    1. Parses indent ++ parser ++ Parse.newline
        -> Outcome.Success!
    2. Parses many spaces ++ Parse.newline
        -> Ignore completely
    3. Parses some number of spaces ++ some not Parse.newlines ++ Parse.newline
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
                [ Parser.map (always EmptyLine) Parse.newline
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
                    |. Parse.newlineWith "indentOrSkip one"

                -- parse field
                , Parser.succeed Indented
                    |= successParser

                -- |. Parse.newlineWith "indentOrSkip two"
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
                [ Parser.map (always False) Parse.newline
                , Parser.succeed True
                    |. Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n'))
                    |. Parse.newline
                ]
        ]


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
                                    |. Parser.loop "" (Parse.raggedIndentedStringAbove (indentation - 4))
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


getField :
    Field value
    -> List ( String, Found Description )
    -> Result Error.Error (Found value)
getField (Field name fieldBlock) fields =
    List.foldl (matchField name fieldBlock) (Err (Error.MissingFields [ name ])) fields


matchField :
    String
    -> Block value
    -> ( String, Found Description )
    -> Result Error.Error (Found value)
    -> Result Error.Error (Found value)
matchField targetName targetBlock ( name, foundDescription ) existing =
    case existing of
        Ok _ ->
            existing

        Err err ->
            if name == targetName then
                case foundDescription of
                    Found rng description ->
                        case renderBlock targetBlock description of
                            Outcome.Success rendered ->
                                Ok (Found rng rendered)

                            Outcome.Almost invalidAst ->
                                Err err

                            Outcome.Failure _ ->
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
type TreeBuilder
    = TreeBuilder
        { previousIndent : Int
        , levels :
            -- (mostRecent :: remaining)
            List (Nested Description)
        }


emptyTreeBuilder : TreeBuilder
emptyTreeBuilder =
    TreeBuilder
        { previousIndent = 0
        , levels = []
        }


reduceRender :
    (thing -> Outcome.Outcome AstError (Uncertain other) other)
    -> List thing
    -> Outcome.Outcome AstError (Uncertain (List other)) (List other)
reduceRender fn list =
    list
        |> List.foldl
            (\x gathered ->
                case gathered of
                    Outcome.Success remain ->
                        case fn x of
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
            (Outcome.Success [])
        |> Outcome.mapSuccess List.reverse


mergeErrors ( h1, r1 ) ( h2, r2 ) =
    ( h1, r1 ++ h2 :: r2 )


{-| -}
renderTreeNodeSmall :
    Block item
    -> Nested Description
    -> Outcome.Outcome AstError (Uncertain (Tree item)) (Tree item)
renderTreeNodeSmall contentBlock (Nested cursor) =
    let
        renderedChildren =
            reduceRender (renderTreeNodeSmall contentBlock) cursor.children

        renderedContent =
            reduceRender (renderBlock contentBlock) cursor.content
    in
    mergeWith
        (\content children ->
            Tree
                { icon = cursor.icon
                , index = cursor.index
                , content = content
                , children = children
                }
        )
        renderedContent
        renderedChildren


buildTree : Int -> List FlatCursor -> List (Nested Description)
buildTree baseIndent items =
    let
        -- gather ( indentation, icon, item ) (TreeBuilder builder) =
        gather item builder =
            addItem (item.indent - baseIndent) item.icon item.content builder

        groupByIcon item maybeCursor =
            case maybeCursor of
                Nothing ->
                    case item.icon of
                        Just icon ->
                            Just
                                { indent = item.indent
                                , icon = icon
                                , items = [ item.content ]
                                , accumulated = []
                                }

                        Nothing ->
                            -- Because of how the code runs, we have a tenuous guarantee that this branch won't execute.
                            -- Not entirely sure how to make the types work to eliminate this.
                            Nothing

                Just cursor ->
                    Just <|
                        case item.icon of
                            Nothing ->
                                { indent = cursor.indent
                                , icon = cursor.icon
                                , items = item.content :: cursor.items
                                , accumulated = cursor.accumulated
                                }

                            Just icon ->
                                { indent = item.indent
                                , icon = icon
                                , items = [ item.content ]
                                , accumulated =
                                    { indent = cursor.indent
                                    , icon = cursor.icon
                                    , content = cursor.items
                                    }
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
                            { indent = cursor.indent
                            , icon = cursor.icon
                            , content = cursor.items
                            }
                                :: cursor.accumulated

        newTree =
            items
                |> List.foldl groupByIcon Nothing
                |> finalizeGrouping
                |> List.reverse
                |> List.foldl gather emptyTreeBuilder
    in
    case newTree of
        TreeBuilder builder ->
            List.reverse (renderLevels builder.levels)


type alias NestedIndex =
    { base : Int
    , prev : Int
    }


type alias FlatCursor =
    { icon : Maybe Icon
    , indent : Int
    , content : Description
    }


{-| Results in a flattened version of the parsed list.

    ( 0, Maybe Icon, [ "item one" ] )

    ( 0, Maybe Icon, [ "item two" ] )

    ( 4, Maybe Icon, [ "nested item two", "additional text for nested item two" ] )

    ( 0, Maybe Icon, [ "item three" ] )

    ( 4, Maybe Icon, [ "nested item three" ] )

-}
indentedBlocksOrNewlines :
    Id.Seed
    -> Block thing
    -> ( NestedIndex, List FlatCursor )
    -> Parser Context Problem (Parser.Step ( NestedIndex, List FlatCursor ) (List FlatCursor))
indentedBlocksOrNewlines seed item ( indentation, existing ) =
    Parser.oneOf
        [ Parser.end End
            |> Parser.map
                (\_ ->
                    Parser.Done (List.reverse existing)
                )

        -- Whitespace Line
        , skipBlankLineWith (Parser.Loop ( indentation, existing ))
        , Parser.oneOf
            [ -- block with required indent
              expectIndentation indentation.base indentation.prev
                |> Parser.andThen
                    (\newIndent ->
                        let
                            ( itemSeed, itemParser ) =
                                getParser seed item
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
                                            , { indent = newIndent
                                              , icon = Just iconResult
                                              , content = itemResult
                                              }
                                                :: existing
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
                                                            , { indent = indentation.prev
                                                              , icon = Nothing
                                                              , content = foundBlock
                                                              }
                                                                :: existing
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
addItem : Int -> Icon -> List Description -> TreeBuilder -> TreeBuilder
addItem indentation icon content (TreeBuilder builder) =
    let
        newItem : Int -> Nested Description
        newItem index =
            Nested
                { index = [ index ]
                , icon = icon
                , children = []
                , content = content
                }
    in
    case builder.levels of
        [] ->
            TreeBuilder
                { previousIndent = indentation
                , levels =
                    [ newItem 1 ]
                }

        lvl :: remaining ->
            if indentation == 0 then
                -- add to current level
                TreeBuilder
                    { previousIndent = indentation
                    , levels =
                        newItem (List.length remaining + 2) :: lvl :: remaining
                    }

            else
                -- We've dedented, so we need to first collapse the current level
                -- into the one below, then add an item to that level
                TreeBuilder
                    { previousIndent = indentation
                    , levels =
                        addToLevel
                            ((abs indentation // 4) - 1)
                            (newItem (List.length remaining + 1))
                            lvl
                            :: remaining
                    }


indentIndex (Nested nested) =
    Nested
        { nested
            | index = 1 :: nested.index
        }


indexTo i (Nested nested) =
    case nested.index of
        [] ->
            Nested { nested | index = [ i ] }

        top :: tail ->
            Nested { nested | index = i :: tail }


addToLevel index brandNewItem (Nested parent) =
    if index <= 0 then
        Nested
            { parent
                | children =
                    indexTo
                        (List.length parent.children + 1)
                        (indentIndex brandNewItem)
                        :: parent.children
            }

    else
        case parent.children of
            [] ->
                Nested parent

            top :: remain ->
                let
                    withNewIndex =
                        brandNewItem
                            |> indentIndex
                            |> indexTo (List.length parent.children)
                in
                Nested
                    { parent
                        | children =
                            addToLevel (index - 1) withNewIndex top
                                :: remain
                    }



--
-- case levels of
--     [] ->
--         [ brandNewItem ]
--     lvl :: remaining ->
--         addToChildren newItem lvl
--             :: remaining


addToChildren : Nested Description -> Nested Description -> Nested Description
addToChildren child (Nested parent) =
    Nested { parent | children = child :: parent.children }


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
collapseLevel : Int -> List (Nested Description) -> List (Nested Description)
collapseLevel num levels =
    if num == 0 then
        levels

    else
        case levels of
            [] ->
                levels

            topLevel :: lowerItem :: remaining ->
                collapseLevel (num - 1) <|
                    addToChildren lowerItem topLevel
                        :: remaining

            _ ->
                levels


type alias TreeIndex =
    { index : Int
    , level : List Int
    }


dive cursor =
    { cursor
        | index = 0
        , level = cursor.index :: cursor.level
    }


next cursor =
    { cursor
        | index = cursor.index + 1
    }


renderLevels : List (Nested Description) -> List (Nested Description)
renderLevels levels =
    case levels of
        [] ->
            []

        _ ->
            List.indexedMap
                (\index level ->
                    reverseTree { index = index, level = [] } level
                )
                levels


reverseTree cursor (Nested nest) =
    Nested
        { index = nest.index
        , icon = nest.icon
        , content = List.reverse nest.content
        , children =
            List.foldl rev ( dive cursor, [] ) nest.children
                |> Tuple.second
        }


rev nest ( cursor, found ) =
    ( next cursor, reverseTree cursor nest :: found )


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
