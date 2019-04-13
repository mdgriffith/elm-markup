module Mark exposing
    ( Document
    , Outcome(..), Partial
    , compile, parse, Parsed, toString, render
    , document
    , Block, map, verify, onError
    , string, int, float, bool, multiline
    , block, oneOf, manyOf, startWith
    , tree
    , field, Field, record2, record3, record4, record5, record6, record7, record8, record9, record10
    , Text(..), Styles, text, textWith, replacement, balanced, Replacement
    , Inline, token, annotation, verbatim, attrString, attrFloat, attrInt
    , Error, errorToString, errorToHtml, Theme(..)
    , ErrorDetails, errorDetails, Range, Position
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

@docs tree


## Records

@docs field, Field, record2, record3, record4, record5, record6, record7, record8, record9, record10


## Handling Text and Inline

@docs Text, Styles, text, textWith, replacement, balanced, Replacement

@docs Inline, token, annotation, verbatim, attrString, attrFloat, attrInt


## Displaying Errors

@docs Error, errorToString, errorToHtml, Theme

@docs ErrorDetails, errorDetails, Range, Position

-}

import Html
import Html.Attributes
import Mark.Edit
import Mark.Internal.Description as Desc exposing (..)
import Mark.Internal.Error as Error exposing (AstError(..), Context(..), Problem(..))
import Mark.Internal.Id as Id exposing (..)
import Mark.Internal.Outcome as Outcome
import Mark.Internal.Parser as Parse
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


{-| -}
type alias Parsed =
    Desc.Parsed



{- INTERFACE -}


{-| -}
toString : Parsed -> String
toString =
    Desc.toString


{-| -}
parse : Document data -> String -> Outcome (List Error) (Partial Parsed) Parsed
parse doc source =
    Desc.compile doc source
        |> moveParsedToResult


moveParsedToResult :
    Result
        (Outcome.Outcome (List Error.Rendered)
            { errors : List Error.Rendered
            , result : data
            }
            data
        )
        ( Parsed
        , Outcome.Outcome (List Error.Rendered)
            { errors : List Error.Rendered
            , result : data
            }
            data
        )
    -> Outcome (List Error) (Partial Parsed) Parsed
moveParsedToResult result =
    case result of
        Ok ( parsed, Outcome.Success success ) ->
            Success parsed

        Ok ( parsed, Outcome.Almost almost ) ->
            Almost
                { errors = almost.errors
                , result = parsed
                }

        Ok ( parsed, Outcome.Failure errors ) ->
            Failure errors

        Err (Outcome.Success success) ->
            Failure []

        Err (Outcome.Almost almost) ->
            Failure almost.errors

        Err (Outcome.Failure fail) ->
            Failure fail


{-| -}
render : Document data -> Parsed -> Outcome (List Error) (Partial data) data
render doc ((Parsed parsedDetails) as parsed) =
    Desc.render doc parsed
        |> rewrapOutcome


{-| -}
compile : Document data -> String -> Outcome (List Error) (Partial data) data
compile doc source =
    Desc.compile doc source
        |> flattenErrors
        |> rewrapOutcome


flattenErrors result =
    case result of
        Ok ( parsed, outcome ) ->
            outcome

        Err outcome ->
            outcome


rewrapOutcome : Outcome.Outcome x y z -> Outcome x y z
rewrapOutcome outcome =
    case outcome of
        Outcome.Success s ->
            Success s

        Outcome.Almost x ->
            Almost x

        Outcome.Failure f ->
            Failure f


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
type alias Document data =
    Desc.Document data


{-| -}
type Outcome failure almost success
    = Success success
    | Almost almost
    | Failure failure


{-| -}
uncertain : Error.UnexpectedDetails -> Outcome.Outcome AstError (Uncertain data) data
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


getUnexpecteds : Description -> List Error.UnexpectedDetails
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
        DescribeBoolean details ->
            unexpectedFromFound details.found

        DescribeInteger details ->
            unexpectedFromFound details.found

        DescribeFloat details ->
            unexpectedFromFound details.found

        DescribeText details ->
            []

        DescribeString rng _ str ->
            []

        DescribeMultiline rng _ str ->
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
    Error.Rendered


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
            Parse.getFailableBlock seed child
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
                (\source result ->
                    case result of
                        Ok details ->
                            Parsed
                                { errors =
                                    List.map (Error.render source) (getUnexpecteds details.value)
                                , found = Found details.range details.value
                                , expected = getBlockExpectation child
                                , initialSeed = seed
                                , currentSeed = currentSeed
                                }

                        Err details ->
                            Parsed
                                { errors =
                                    [ Error.render source
                                        { range = details.range
                                        , problem = details.error
                                        }
                                    ]
                                , found =
                                    Unexpected
                                        { range = details.range
                                        , problem = details.error
                                        }
                                , expected = getBlockExpectation child
                                , initialSeed = seed
                                , currentSeed = currentSeed
                                }
                )
                |. Parser.chompWhile (\c -> c == '\n')
                |= Parser.getSource
                |= Parse.withRangeResult (Parser.withIndent 0 blockParser)
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

                                            -- TODO: Does this mean we need to thread source snippets everywhere to get them here?
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
onError : a -> Block a -> Block a
onError newValue myBlock =
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
                                Outcome.Almost
                                    (Recovered x newValue)

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
                                Outcome.Almost
                                    (Recovered x newValue)

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


skipSeed parser seed =
    ( seed, parser )


{-| -}
block : String -> (child -> result) -> Block child -> Block result
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
                    (\result ->
                        case result of
                            Ok details ->
                                DescribeBlock
                                    { found = Found details.range details.value
                                    , name = name
                                    , expected = ExpectBlock name (getBlockExpectation child)
                                    }

                            Err details ->
                                DescribeBlock
                                    { name = name
                                    , found =
                                        Unexpected
                                            { range = details.range
                                            , problem = details.error
                                            }
                                    , expected = ExpectBlock name (getBlockExpectation child)
                                    }
                    )
                  <|
                    Parse.withRangeResult
                        (Parse.withIndent
                            (\indentation ->
                                Parser.succeed identity
                                    |. Parser.keyword
                                        (Parser.Token name
                                            (ExpectingBlockName name)
                                        )
                                    |. Parser.chompWhile (\c -> c == ' ')
                                    |. Parse.skipBlankLineWith ()
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
                                                        -- ALERT:  This first parser will fail because `Parse.raggedIndentedStringAbove`
                                                        -- expects indentation for the first line.
                                                        [ Parser.succeed
                                                            (\end ->
                                                                Err (Error.ExpectingIndent (indentation + 4))
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
                                            (Err (Error.ExpectingIndent (indentation + 4)))
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
        [ Parse.skipBlankLineWith (Parser.Loop (lineCount + 1))
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
            Parse.oneOf blocks expectations
        }


{-| Many blocks that are all at the same indentation level.
-}
manyOf : List (Block a) -> Block (List a)
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


{-| It can be useful to parse a tree structure. For example, here's a nested list.

    | List
        - item one
        - item two
            - nested item two

            additional text for nested item two
        - item three
            - nested item three

In order to parse the above, you could define a block as

    Mark.tree "List"
        ((Mark.Tree node) ->
        -- Do something with node.content and node.children
        )
        text

**Note** the indentation is always a multiple of 4.

-}
tree :
    String
    -> (Mark.Edit.Tree item -> result)
    -> Block item
    -> Block (List result)
tree name view contentBlock =
    Mark.Edit.tree name
        (\meta items ->
            List.map view items
        )
        contentBlock


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

**NOTE** includes `Mark.commonReplacements` by default.

-}
text :
    (Text -> rendered)
    -> Block (List rendered)
text view =
    Value
        { expect = ExpectTextBlock []
        , converter =
            renderText
                { view = view
                , inlines = []
                , replacements = commonReplacements
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
                                , replacements = commonReplacements
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

    /My italicized sentence can have *bold* words./

In order to render this, the above sentence is chopped up into `Text` fragments that can have multiple styles active.

  - `view` is the function to render an individual fragment.
  - `inlines` are custom inline blocks.
  - `replacements` will replace characters before rendering. For example, we can replace `...` with the real ellipses unicode character, `…`.

-}
textWith :
    { view : Text -> rendered
    , inlines : List (Inline rendered)
    , replacements : List Replacement
    }
    -> Block (List rendered)
textWith options =
    let
        inlineExpectations =
            List.map getInlineExpectation options.inlines
    in
    Value
        { expect = ExpectTextBlock inlineExpectations
        , converter = renderText options
        , parser =
            \seed ->
                -- TODO:  probably need a seed for text editing.
                ( seed
                , Parse.getPosition
                    |> Parser.andThen
                        (\pos ->
                            Parse.styledText
                                { inlines = inlineExpectations
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


isToken : Inline data -> Bool
isToken (Inline inline) =
    case inline.expect of
        ExpectToken _ _ ->
            True

        _ ->
            False


isVerbatim : Inline data -> Bool
isVerbatim (Inline inline) =
    case inline.expect of
        ExpectVerbatim _ _ _ ->
            True

        _ ->
            False


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


{-| -}
annotation : String -> (List Text -> result) -> Inline result
annotation name result =
    Inline
        { converter =
            \textPieces attrs ->
                Outcome.Success [ result textPieces ]
        , expect =
            ExpectAnnotation name [] []
        , name = name
        }


{-| -}
verbatim : String -> (Text -> result) -> Inline result
verbatim name result =
    Inline
        { converter =
            \textPieces attrs ->
                case textPieces of
                    [] ->
                        -- This should never happen
                        Outcome.Failure NoMatch

                    fst :: _ ->
                        Outcome.Success [ result fst ]
        , expect =
            ExpectVerbatim name [] "placeholder"
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
                                    |> mapSuccessAndRecovered (List.map (\x -> x (String.trim attr.value)))

                            _ ->
                                Outcome.Failure NoMatch
                , expect =
                    case details.expect of
                        ExpectToken tokenName attrs ->
                            ExpectToken tokenName (ExpectAttrString name "" :: attrs)

                        ExpectAnnotation noteName attrs placeholder ->
                            ExpectAnnotation noteName (ExpectAttrString name "" :: attrs) placeholder

                        ExpectVerbatim verbatimName attrs placeholder ->
                            ExpectVerbatim verbatimName (ExpectAttrString name "" :: attrs) placeholder

                        -- This shouldn't happen
                        ExpectText x ->
                            ExpectText x
                , name = details.name
                }


{-| -}
attrInt : String -> Inline (Int -> result) -> Inline result
attrInt name newInline =
    case newInline of
        Inline details ->
            Inline
                { converter =
                    \textPieces attrs ->
                        case attrs of
                            [] ->
                                Outcome.Failure NoMatch

                            (AttrInt attr) :: remaining ->
                                details.converter textPieces remaining
                                    |> mapSuccessAndRecovered (List.map (\x -> x attr.value))

                            _ ->
                                Outcome.Failure NoMatch
                , expect =
                    case details.expect of
                        ExpectToken tokenName attrs ->
                            ExpectToken tokenName (ExpectAttrInt name 0 :: attrs)

                        ExpectAnnotation noteName attrs placeholder ->
                            ExpectAnnotation noteName (ExpectAttrInt name 0 :: attrs) placeholder

                        ExpectVerbatim verbatimName attrs placeholder ->
                            ExpectVerbatim verbatimName (ExpectAttrInt name 0 :: attrs) placeholder

                        -- This shouldn't happen
                        ExpectText x ->
                            ExpectText x
                , name = details.name
                }


defaultFloatAttr =
    ( "0", 0 )


{-| -}
attrFloat : String -> Inline (Float -> result) -> Inline result
attrFloat name newInline =
    case newInline of
        Inline details ->
            Inline
                { converter =
                    \textPieces attrs ->
                        case attrs of
                            [] ->
                                Outcome.Failure NoMatch

                            (AttrFloat attr) :: remaining ->
                                details.converter textPieces remaining
                                    |> mapSuccessAndRecovered (List.map (\x -> x (Tuple.second attr.value)))

                            _ ->
                                Outcome.Failure NoMatch
                , expect =
                    case details.expect of
                        ExpectToken tokenName attrs ->
                            ExpectToken tokenName (ExpectAttrFloat name defaultFloatAttr :: attrs)

                        ExpectAnnotation noteName attrs placeholder ->
                            ExpectAnnotation noteName (ExpectAttrFloat name defaultFloatAttr :: attrs) placeholder

                        ExpectVerbatim verbatimName attrs placeholder ->
                            ExpectVerbatim verbatimName (ExpectAttrFloat name defaultFloatAttr :: attrs) placeholder

                        -- This shouldn't happen
                        ExpectText x ->
                            ExpectText x
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
                        (Parse.withIndent
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
                    (\boolResult ->
                        DescribeBoolean
                            { id = id
                            , found =
                                case boolResult of
                                    Err err ->
                                        Unexpected
                                            { range = err.range
                                            , problem = Error.BadBool
                                            }

                                    Ok details ->
                                        Found
                                            details.range
                                            details.value
                            }
                    )
                    (Parse.withRangeResult
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
        (\result ->
            case result of
                Ok details ->
                    Record
                        { expected = expectations
                        , name = recordName
                        , found =
                            Found (backtrackCharacters 2 details.range) details.value
                        }

                -- Err ( maybePosition, prob ) ->
                Err err ->
                    Record
                        { expected = expectations
                        , name = recordName
                        , found =
                            Unexpected
                                { range = Maybe.withDefault (backtrackCharacters 2 err.range) (Tuple.first err.error)
                                , problem = Tuple.second err.error
                                }
                        }
        )
        |= Parse.withRangeResult
            (Parse.withIndent
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
    Parse.withIndent
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
    Parse.withIndent
        (\indentation ->
            Parser.map
                (\{ range, value } ->
                    ( value
                    , Unexpected
                        { range = range
                        , problem =
                            Error.UnexpectedField
                                { found = value
                                , options = options
                                , recordName = recordName
                                }
                        }
                    )
                )
                (Parse.getRangeAndSource
                    (Parser.succeed identity
                        |= Parser.getChompedString (Parser.chompWhile Char.isAlphaNum)
                        |. Parser.chompWhile (\c -> c == ' ')
                        |. Parser.chompIf (\c -> c == '=') (Expecting "=")
                        |. Parser.chompWhile (\c -> c == ' ')
                        -- TODO: parse multiline string
                        |. Parser.withIndent (indentation + 4) (Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n')))
                     -- |. Parse.newline
                     -- |. Parser.map (Debug.log "unexpected capture") (Parser.loop "" (raggedIndentedStringAbove (indent - 4)))
                    )
                )
        )


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
            Parse.withIndent
                (\indentation ->
                    Parser.succeed
                        (\remaining ->
                            if String.trim remaining == "" then
                                Parser.Done fields.found

                            else
                                Parser.Done
                                    (Err
                                        ( Nothing
                                        , Error.UnexpectedField
                                            { options = fieldNames
                                            , found = String.trim remaining
                                            , recordName = recordName
                                            }
                                        )
                                    )
                        )
                        |= Parser.oneOf
                            [ Parser.succeed identity
                                |. Parser.token
                                    (Parser.Token ("\n" ++ String.repeat indentation " ")
                                        (ExpectingIndentation indentation)
                                    )
                                |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n'))
                            , Parser.succeed ""
                            ]
                )

        _ ->
            case fields.found of
                Ok found ->
                    Parse.withIndent
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
                    Parse.withIndent
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
applyField : Found a -> Result Error.UnexpectedDetails (a -> b) -> Result Error.UnexpectedDetails b
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



-- {-|
--     1.
--         1.1
--     2.
--     Steps =
--     []
--     [ Level [ Item 1. [] ]
--     ]
--     [ Level [ Item 1.1 ]
--     , Level [ Item 1. [] ]
--     ]
--     -- collapse into lower level
--     [ Level [ Item 1. [ Item 1.1 ] ]
--     ]
--     -- add new item
--     [ Level [ Item 2, Item 1. [ Item 1.1 ] ]
--     ]
-- -}
-- collapseLevel : Int -> List (Nested Description) -> List (Nested Description)
-- collapseLevel num levels =
--     if num == 0 then
--         levels
--     else
--         case levels of
--             [] ->
--                 levels
--             topLevel :: lowerItem :: remaining ->
--                 collapseLevel (num - 1) <|
--                     addToChildren lowerItem topLevel
--                         :: remaining
--             _ ->
--                 levels


{-| This is a set of common character replacements with some typographical niceties.

  - `...` is converted to the ellipses unicode character(`…`).
  - `"` Straight double quotes are [replaced with curly quotes](https://practicaltypography.com/straight-and-curly-quotes.html) (`“`, `”`)
  - `'` Single Quotes are replaced with apostrophes(`’`).
  - `--` is replaced with an en-dash(`–`).
  - `---` is replaced with an em-dash(`—`).
  - `<>` also known as "glue", will create a non-breaking space (`&nbsp;`). This is not for manually increasing space (sequential `<>` tokens will only render as one `&nbsp;`), but to signify that the space between two words shouldn't break when wrapping. Like glueing two words together!
  - `//` will change to `/`. Normally `/` starts italic formatting. To escape this, we'd normally do `\/`, though that looks pretty funky. `//` just feels better!

**NOTE** this is included by default in `Mark.text`

-}
commonReplacements : List Replacement
commonReplacements =
    [ Parse.Replacement "..." "…"
    , Parse.Replacement "<>" "\u{00A0}"
    , Parse.Replacement "---" "—"
    , Parse.Replacement "--" "–"
    , Parse.Replacement "//" "/"
    , Parse.Replacement "'" "’"
    , Parse.Balanced
        { start = ( "\"", "“" )
        , end = ( "\"", "”" )
        }
    ]


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
errorToString error =
    case error of
        Error.Rendered details ->
            formatErrorString
                { title = details.title
                , message = details.message
                }

        Error.Global global ->
            formatErrorString
                { title = global.title
                , message = global.message
                }


{-| -}
type alias ErrorDetails =
    { title : String
    , message : String
    , region : Maybe Range
    }


{-| -}
errorDetails : Error -> ErrorDetails
errorDetails error =
    case error of
        Error.Rendered details ->
            { title = details.title
            , message = String.join "" (List.map .text details.message)
            , region = Just details.region
            }

        Error.Global global ->
            { title = global.title
            , message = String.join "" (List.map .text global.message)
            , region = Nothing
            }


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
    case error of
        Error.Rendered details ->
            formatErrorHtml theme
                { title = details.title
                , message = details.message
                }

        Error.Global global ->
            formatErrorHtml theme
                { title = global.title
                , message = global.message
                }


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
