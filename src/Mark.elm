module Mark exposing
    ( Document, document, documentWith
    , Block, block
    , string, int, float, bool
    , Styles, text, textWith
    , Replacement, commonReplacements, replacement, balanced
    , annotation, verbatim
    , Record, record, field, toBlock
    , oneOf, manyOf
    , tree, Enumerated(..), Item(..), Icon(..)
    , Outcome(..), Partial
    , compile, parse, Parsed, toString, render
    , map, verify, onError
    , withId, idToString, stringToId
    )

{-|


# Building Documents

@docs Document, document, documentWith

@docs Block, block


# Primitives

@docs string, int, float, bool


# Text

@docs Styles, text, textWith


# Text Replacements

@docs Replacement, commonReplacements, replacement, balanced


# Text Annotations

Along with basic [`styling`](#text) and [`replacements`](#replacement), we also have a few ways to annotate text.

@docs annotation, verbatim


# Records

@docs Record, record, field, toBlock


# Higher Level

@docs oneOf, manyOf


# Trees

@docs tree, Enumerated, Item, Icon


# Rendering

@docs Outcome, Partial

@docs compile, parse, Parsed, toString, render


# Constraining and Recovering Blocks

@docs map, verify, onError

@docs withId, idToString, stringToId

-}

import Html
import Html.Attributes
import Mark.Edit
import Mark.Error
import Mark.Internal.Description as Desc exposing (..)
import Mark.Internal.Error as Error exposing (AstError(..), Context(..), Problem(..))
import Mark.Internal.Id as Id exposing (..)
import Mark.Internal.Index as Index
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
parse : Document data -> String -> Outcome (List Mark.Error.Error) (Partial Parsed) Parsed
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
    -> Outcome (List Mark.Error.Error) (Partial Parsed) Parsed
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
render : Document data -> Parsed -> Outcome (List Mark.Error.Error) (Partial data) data
render doc ((Parsed parsedDetails) as parsed) =
    Desc.render doc parsed
        |> rewrapOutcome


{-| -}
compile : Document data -> String -> Outcome (List Mark.Error.Error) (Partial data) data
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
    { errors : List Mark.Error.Error
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
type alias Document data =
    Desc.Document data


{-| -}
type Outcome failure almost success
    = Success success
    | Almost almost
    | Failure failure


{-| -}
type alias Block data =
    Desc.Block data


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

        StartsWith details ->
            getUnexpecteds details.first.found
                ++ getUnexpecteds details.second.found

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

        DescribeNothing _ ->
            []


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


{-| Create a markup `Document`. You're first goal is to describe a document in terms of the blocks you're expecting.

Here's an overly simple document that captures one block, a Title, and wraps it in some `Html`

    document : Mark.Document (Html msg)
    document =
        Mark.document
            (\title -> Html.main [] [ title ])
            (Mark.block "Title"
                (Html.h1 [])
                Mark.string
            )

will parse the following markup:

```markup
|> Title
    Here is my title!
```

and ultimately render it as

```html
<main>
    <h1>Here is my title!</h1>
</main>
```

-}
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
            Parse.getFailableBlock Desc.ParseBlock seed child
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


{-| Capture some metadata at the start of your document, followed by the body.

    import Mark.Record as Record

    Mark.documentWith
        (\metadata body ->
            { metadata = metadata
            , body = body
            }
        )
        { metadata =
            Record.record
                (\author publishedAt ->
                    { author = author
                    , publishedAt = publishedAt
                    }
                )
                |> Record.field "author" Mark.string
                |> Record.field "publishedAt" Mark.string
                |> Record.toBlock
        , body =
            --...
        }

-}
documentWith :
    (metadata -> body -> document)
    ->
        { metadata : Block metadata
        , body : Block body
        }
    -> Document document
documentWith renderer { metadata, body } =
    document
        identity
        (startWith
            renderer
            metadata
            body
        )


{-| Change the result of a block by applying a function to it.
-}
map : (a -> b) -> Block a -> Block b
map fn (Block details) =
    Block
        { kind = details.kind
        , converter = mapSuccessAndRecovered fn << details.converter
        , parser = details.parser
        , expect = details.expect
        }


{-| -}
type alias CustomError =
    { title : String
    , message : List String
    }


{-| `Mark.verify` lets you put constraints on a block.

Let's say you don't just want a `Mark.string`, you actually want a date.

So, you install the [`ISO8601`](https://package.elm-lang.org/packages/rtfeldman/elm-iso8601-date-strings/latest/) and you write something that looks like:

    import Iso8601
    import Mark
    import Mark.Error
    import Time

    date : Mark.Block Time.Posix
    date =
        Mark.verify
            (\str ->
                str
                    |> Iso8601.toTime
                    |> Result.mapError
                        (\_ -> illformatedDateMessage)
            )
            Mark.string

    illformatedDateMessage =
        Mark.Error.custom
            { title = "Bad Date"
            , message =
                [ "I was trying to parse a date, but this format looks off.\n\n"
                , "Dates should be in ISO 8601 format:\n\n"
                , "YYYY-MM-DDTHH:mm:ss.SSSZ"
                ]
            }

Now you can use `date` whever you actually want dates and the error message will be shown if something goes wrong.

More importantly, you now know if a document parses successfully, that all your dates are correctly formatted.

`Mark.verify` is a very nice way to extend your markup however you'd like.

You could use it to

  - add units to numbers
  - parse a custom format, like [Latex mathematical equations](https://en.wikibooks.org/wiki/LaTeX/Mathematics#Operators)
  - ensure that numbers are between a range or are always positive.

How exciting! Seriously, I think this is pretty cool.

-}
verify : (a -> Result Mark.Error.Custom b) -> Block a -> Block b
verify fn (Block details) =
    Block
        { kind = details.kind
        , expect = details.expect
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


{-| Get an `Id` associated with a `Block`, which can be used to make updates through `Mark.Edit`.

    Mark.withId
        (\id str ->
            Html.span
                [ onClick (Mark.Edit.delete id) ]
                [ Html.text str ]
        )
        Mark.string

-}
withId : (Mark.Edit.Id -> a -> b) -> Block a -> Block b
withId fn (Block details) =
    Block
        { kind = details.kind
        , converter =
            \desc ->
                let
                    id =
                        Desc.getId desc
                in
                mapSuccessAndRecovered
                    (fn id)
                    (details.converter desc)
        , parser = details.parser
        , expect = details.expect
        }


{-| It may be necessary to convert an `Id` to a `String` and back in order attach it as an `Html.Attributes.id` and read it back.

See the editor example for more details.

**Note** be aware that the actual string format of an `Id` is an implementation detail and may change even on patch releases of a library.

-}
idToString : Mark.Edit.Id -> String
idToString =
    Id.toString


{-| -}
stringToId : String -> Maybe Mark.Edit.Id
stringToId =
    Id.fromString


{-| Parsing any given `Block` can fail.

However sometimes we don't want the _whole document_ to be unable to render just because there was a small error somewhere.

So, we need some way to say "Hey, if you run into an issue, here's a placeholder value to use."

And that's what `Mark.onError` does.

    Mark.int
        |> Mark.onError 5

This means if we fail to parse an integer (let's say we added a decimal), that this block would still be renderable with a default value of `5`.

**Note** If there _is_ an error that is fixed using `onError`, we'll get a [`Partial`](#Partial) when we render the document. This will let us see the _full rendered document_, but also see the _error_ that actually occurred.

-}
onError : a -> Block a -> Block a
onError newValue (Block details) =
    Block
        { kind = details.kind
        , expect = details.expect
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


{-| A named block.

    Mark.block "MyBlock"
        Html.text
        Mark.string

Will parse the following and render it using `Html.text`

```markup
|> MyBlock
    Here is an unformatted string!
```

**Note** block names should be capitalized. In the future this may be enforced.

-}
block : String -> (child -> result) -> Block child -> Block result
block name view child =
    Block
        { kind = Named name
        , expect = ExpectBlock name (getBlockExpectation child)
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
            \context seed ->
                let
                    ( newSeed, childParser ) =
                        getParser context seed child

                    ( parentId, finalSeed ) =
                        Id.step newSeed
                in
                ( finalSeed
                , Parser.map
                    (\result ->
                        case result of
                            Ok details ->
                                DescribeBlock
                                    { found = Found details.range details.value
                                    , name = name
                                    , id = parentId
                                    , expected = ExpectBlock name (getBlockExpectation child)
                                    }

                            Err details ->
                                DescribeBlock
                                    { name = name
                                    , id = parentId
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


{-| -}
startWith :
    (start -> rest -> result)
    -> Block start
    -> Block rest
    -> Block result
startWith fn startBlock endBlock =
    Block
        { kind = Value
        , expect = ExpectStartsWith (getBlockExpectation startBlock) (getBlockExpectation endBlock)
        , converter =
            \desc ->
                case desc of
                    StartsWith details ->
                        mergeWith fn
                            (renderBlock startBlock details.first.found)
                            (renderBlock endBlock details.second.found)

                    _ ->
                        Outcome.Failure NoMatch
        , parser =
            \context seed ->
                let
                    ( parentId, newSeed ) =
                        Id.step seed

                    ( startSeed, startParser ) =
                        getParser ParseBlock newSeed startBlock

                    ( remainSeed, endParser ) =
                        getParser ParseBlock startSeed endBlock
                in
                ( remainSeed
                , Parser.succeed
                    (\( range, ( begin, end ) ) ->
                        StartsWith
                            { range = range
                            , id = parentId
                            , first =
                                { found = begin
                                , expected = getBlockExpectation startBlock
                                }
                            , second =
                                { found = end
                                , expected = getBlockExpectation endBlock
                                }
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
    Block
        { kind = Value
        , expect = ExpectOneOf expectations
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
    Block
        { kind = Value
        , expect = ExpectManyOf expectations
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
            \context seed ->
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
type Icon
    = Bullet
    | Number


{-| -}
type Enumerated item
    = Enumerated
        { icon : Icon
        , items : List (Item item)
        }


{-| **Note** `index` is our position within the nested list.

The first `Int` in the tuple is our current position in the current sub list.

The `List Int` that follows are the indices for the parent list.

For example, given this list

```markup
|> List
    1. First element
    -- Second Element
        1. Element #2.1
            -- Element #2.1.1
        -- Element #2.2
    -- Third Element
```

here are the indices:

```markup
1. (1, [])
-- (2, [])
    1. (1, [2])
        -- (1, [1,2])
    -- (2, [2])
-- (3, [])
```

-}
type Item item
    = Item
        { index : ( Int, List Int )
        , content : List item
        , children : Enumerated item
        }


{-| Would you believe that a markdown list is actually a tree?

Here's an example of a nested list in `elm-markup`:

```markup
|> List
    1.  This is definitely the first thing.

        With some additional content.

    --- Another thing.

        And some more content

        1.  A sublist

            With it's content

            And some other content

        --- Second item
```

**Note** As before, the indentation is always a multiple of 4.

In `elm-markup` you can make a nested section either `Bulleted` or `Numbered` by having the first element of the section start with `-` or `1.`.

The rest of the icons at that level are ignored. So this:

```markup
|> List
    1. First
    -- Second
    -- Third
```

Is a numbered list. And this:

```markup
|> List
    -- First
        1. sublist one
        -- sublist two
        -- sublist three
    -- Second
    -- Third
```

is a bulleted list with a numbered list inside of it.

**Note** You can use as many dashes(`-`) as you want to start an item. This can be useful to make the indentation match up. Similarly, you can also use spaces after the dash or number.

Here's how to render the above list:

    import Mark

    myTree =
        Mark.tree "List" renderList text

    -- Note: we have to define this as a separate function because
    -- `Items` and `Node` are a pair of mutually recursive data structures.
    -- It's easiest to render them using two separate functions:
    -- renderList and renderItem
    renderList (Mark.Enumerated list) =
        let
            group =
                case list.icon of
                    Mark.Bullet ->
                        Html.ul

                    Mark.Number ->
                        Html.ol
        in
        group []
            (List.map renderItem list.items)

    renderItem (Mark.Item item) =
        Html.li []
            [ Html.div [] item.content
            , renderList item.children
            ]

-}
tree :
    String
    -> (Enumerated item -> result)
    -> Block item
    -> Block result
tree name view contentBlock =
    let
        blockExpectation =
            getBlockExpectation contentBlock

        expectation =
            ExpectTree
                (getBlockExpectation contentBlock)
                [ TreeExpectation
                    { icon = Desc.Bullet
                    , content = [ blockExpectation ]
                    , children = []
                    }
                ]
    in
    Block
        { kind = Named name
        , expect = expectation
        , converter =
            \description ->
                case description of
                    DescribeTree details ->
                        details.children
                            |> reduceRender Index.zero
                                getNestedIcon
                                (renderTreeNodeSmall contentBlock)
                            |> (\( _, icon, outcome ) ->
                                    mapSuccessAndRecovered
                                        (\nodes ->
                                            view
                                                --details.id
                                                (Enumerated
                                                    { icon =
                                                        case icon of
                                                            Desc.Bullet ->
                                                                Bullet

                                                            Desc.AutoNumber _ ->
                                                                Number
                                                    , items = nodes
                                                    }
                                                )
                                        )
                                        outcome
                               )

                    _ ->
                        Outcome.Failure Error.NoMatch
        , parser =
            \context seed ->
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
                                            ParseInTree
                                            seed
                                            contentBlock
                                        )
                                    )
                                )
                    )
                )
        }


getNestedIcon (Nested cursor) =
    cursor.icon


{-| -}
renderTreeNodeSmall :
    Block item
    -> Desc.Icon
    -> Index.Index
    -> Nested Description
    -> Outcome.Outcome Error.AstError (Uncertain (Item item)) (Item item)
renderTreeNodeSmall contentBlock icon index (Nested cursor) =
    let
        ( newIndex, childrenIcon, renderedChildren ) =
            reduceRender (Index.indent index)
                getNestedIcon
                (renderTreeNodeSmall contentBlock)
                cursor.children

        ( _, _, renderedContent ) =
            reduceRender (Index.dedent newIndex)
                (always Desc.Bullet)
                (\icon_ i content ->
                    renderBlock contentBlock content
                )
                cursor.content
    in
    mergeWith
        (\content children ->
            Item
                { index = Index.toList index
                , content = content
                , children =
                    Enumerated
                        { icon =
                            case childrenIcon of
                                Desc.Bullet ->
                                    Bullet

                                Desc.AutoNumber _ ->
                                    Number
                        , items =
                            children
                        }
                }
        )
        renderedContent
        renderedChildren


reduceRender :
    Index.Index
    -> (thing -> Desc.Icon)
    -> (Desc.Icon -> Index.Index -> thing -> Outcome.Outcome Error.AstError (Uncertain other) other)
    -> List thing
    -> ( Index.Index, Desc.Icon, Outcome.Outcome Error.AstError (Uncertain (List other)) (List other) )
reduceRender index getIcon fn list =
    list
        |> List.foldl
            (\item ( i, existingIcon, gathered ) ->
                let
                    icon =
                        if Index.top i == 0 then
                            getIcon item

                        else
                            existingIcon

                    newItem =
                        case gathered of
                            Outcome.Success remain ->
                                case fn icon i item of
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
                in
                ( Index.increment i
                , icon
                , newItem
                )
            )
            ( index, Desc.Bullet, Outcome.Success [] )
        |> (\( i, ic, outcome ) ->
                ( i, ic, Outcome.mapSuccess List.reverse outcome )
           )


{-| -}
type alias Index =
    List Int



{- TEXT BLOCKS -}


{-| -}
type alias Styles =
    { bold : Bool
    , italic : Bool
    , strike : Bool
    }


{-| One of the first things that's interesting about a markup language is how to handle _styled text_.

In `elm-markup` there are only a limited number of special characters for formatting text.

  - `/italic/` results in _italics_
  - `*bold*` results in **bold**
  - and `~strike~` results in ~~strike~~

Here's an example of how to convert markup text into `Html` using `Mark.text`:

    Mark.text
        (\styles string ->
            Html.span
                [ Html.Attributes.classList
                    [ ( "bold", styles.bold )
                    , ( "italic", styles.italic )
                    , ( "strike", styles.strike )
                    ]
                ]
                [ Html.text string ]
        )

Though you might be thinking that `bold`, `italic`, and `strike` are not nearly enough!

And you're right, this is just to get you started. Your next stop is [`Mark.textWith`](#textWith), which is more involved to use but can represent everything you're used to having in a markup language.

**Note:** Text blocks stop when two consecutive newline characters are encountered.

-}
text :
    (Styles -> String -> text)
    -> Block (List text)
text view =
    textWith
        { view = view
        , inlines = []
        , replacements = commonReplacements
        }


{-| -}
type alias Selection =
    { anchor : Offset
    , focus : Offset
    }


{-| -}
type alias Offset =
    Int


{-| Handling formatted text is a little more involved than may be initially apparent, but have no fear!

`textWith` is where a lot of things come together. Let's check out what these fields actually mean.

  - `view` is the function to render an individual fragment of text.
      - This is mostly what [`Mark.text`](#text) does, so it should seem familiar.
  - `replacements` will replace characters before rendering.
      - For example, we can replace `...` with the real ellipses unicode character, `…`.
  - `inlines` are custom inline blocks. You can use these to render things like links or emojis :D.

-}
textWith :
    { view :
        Styles
        -> String
        -> rendered
    , replacements : List Replacement
    , inlines : List (Record rendered)
    }
    -> Block (List rendered)
textWith options =
    let
        inlineRecords =
            List.map recordToInlineBlock options.inlines

        inlineExpectations =
            List.map
                (\(ProtoRecord rec) ->
                    ExpectInlineBlock
                        { name = rec.name
                        , kind =
                            blockKindToSelection rec.blockKind
                        , fields = rec.expectations
                        }
                )
                options.inlines
    in
    Block
        { kind = Value
        , expect = ExpectTextBlock inlineExpectations
        , converter =
            renderText
                { view = always options.view
                , inlines = inlineRecords
                }
        , parser =
            \context seed ->
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
                                { inlines = List.map (\x -> x Desc.EmptyAnnotation) inlineRecords
                                , replacements = options.replacements
                                }
                                context
                                newSeed
                                pos
                                emptyStyles
                                []
                        )
                )
        }


recordToInlineBlock (Desc.ProtoRecord details) annotationType =
    let
        expectations =
            Desc.ExpectRecord details.name
                details.expectations
    in
    Desc.Block
        { kind = details.blockKind
        , expect = expectations
        , converter =
            \desc ->
                case details.fieldConverter desc annotationType of
                    Outcome.Success ( pos, fieldDescriptions, rendered ) ->
                        Outcome.Success rendered

                    Outcome.Failure fail ->
                        Outcome.Failure fail

                    Outcome.Almost (Desc.Uncertain e) ->
                        Outcome.Almost (Desc.Uncertain e)

                    Outcome.Almost (Desc.Recovered e ( pos, fieldDescriptions, rendered )) ->
                        Outcome.Almost (Desc.Recovered e rendered)
        , parser =
            \context seed ->
                let
                    ( parentId, parentSeed ) =
                        Id.step seed

                    ( newSeed, fields ) =
                        Id.thread parentSeed (List.foldl (\f ls -> f ParseInline :: ls) [] details.fields)
                in
                ( newSeed
                , Parse.record Parse.InlineRecord
                    parentId
                    details.name
                    expectations
                    fields
                )
        }


type alias Cursor data =
    { outcome : Outcome.Outcome Error.AstError (Uncertain data) data
    , lastOffset : Int
    }


type alias TextOutcome data =
    Outcome.Outcome Error.AstError (Uncertain data) data


renderText :
    { view :
        { id : Id
        , selection : Selection
        }
        -> Styles
        -> String
        -> rendered
    , inlines : List (Desc.AnnotationType -> Block rendered)
    }
    -> Description
    -> TextOutcome (List rendered)
renderText options description =
    case description of
        DescribeText details ->
            details.text
                |> List.foldl (convertTextDescription details.id options)
                    { outcome = Outcome.Success []
                    , lastOffset = 0
                    }
                |> .outcome
                |> mapSuccessAndRecovered List.reverse

        _ ->
            Outcome.Failure Error.NoMatch


convertTextDescription :
    Id
    ->
        { view :
            { id : Id
            , selection : Selection
            }
            -> Styles
            -> String
            -> rendered
        , inlines : List (Desc.AnnotationType -> Block rendered)
        }
    -> TextDescription
    -> Cursor (List rendered)
    -> Cursor (List rendered)
convertTextDescription id options comp cursor =
    let
        blockLength =
            length comp
    in
    case comp of
        Styled range (Desc.Text styling str) ->
            { outcome =
                mergeWith (::)
                    (Outcome.Success
                        (options.view
                            { id = id
                            , selection =
                                { anchor = cursor.lastOffset
                                , focus = cursor.lastOffset + blockLength
                                }
                            }
                            styling
                            str
                        )
                    )
                    cursor.outcome
            , lastOffset =
                cursor.lastOffset + blockLength
            }

        InlineBlock details ->
            let
                recordName =
                    Desc.recordName details.record
                        |> Maybe.withDefault ""

                matchInlineName name almostInlineBlock maybeFound =
                    case maybeFound of
                        Nothing ->
                            let
                                (Block inlineDetails) =
                                    almostInlineBlock details.kind
                            in
                            if matchKinds details inlineDetails.kind then
                                Just inlineDetails

                            else
                                Nothing

                        _ ->
                            maybeFound

                maybeMatched =
                    List.foldl
                        (matchInlineName recordName)
                        Nothing
                        options.inlines
            in
            case maybeMatched of
                Nothing ->
                    { outcome =
                        uncertain
                            { range = details.range
                            , problem =
                                Error.UnknownInline
                                    (List.map
                                        (\inline ->
                                            Desc.inlineExample details.kind (inline Desc.EmptyAnnotation)
                                        )
                                        options.inlines
                                    )
                            }
                    , lastOffset = cursor.lastOffset + blockLength
                    }

                Just matched ->
                    { outcome =
                        mergeWith (::)
                            (matched.converter details.record)
                            cursor.outcome
                    , lastOffset = cursor.lastOffset + blockLength
                    }


matchKinds inline blockKind =
    let
        recordName =
            case inline.record of
                Record rec ->
                    Just rec.name

                _ ->
                    Nothing
    in
    case ( recordName, inline.kind, blockKind ) of
        ( Just inlineName, SelectString str, VerbatimNamed vertName ) ->
            inlineName == vertName

        ( Just inlineName, SelectText _, AnnotationNamed annName ) ->
            inlineName == annName

        ( Just inlineName, EmptyAnnotation, Named name ) ->
            inlineName == name

        _ ->
            False


{-| -}
type alias Replacement =
    Parse.Replacement


{-| An annotation is some **text**, a **name**, and zero or more **attributes**.

So, we can make a `link` that looks like this in markup:

```markup
Here is my [*cool* sentence]{link| url = website.com }.
```

and rendered in elm-land via:

    link =
        Mark.annotation "link"
            (\styles url ->
                Html.a
                    [ Html.Attributes.href url ]
                    (List.map renderStyles styles)
            )
            |> Record.field "url" Mark.string

-}
annotation : String -> (List ( Styles, String ) -> result) -> Record result
annotation name view =
    Desc.ProtoRecord
        { name = name
        , blockKind = Desc.AnnotationNamed name
        , expectations = []
        , fieldConverter =
            \desc selected ->
                case desc of
                    Desc.Record details ->
                        if details.name == name then
                            case details.found of
                                Desc.Found pos fieldDescriptions ->
                                    Outcome.Success ( pos, fieldDescriptions, view (selectedText selected) )

                                Desc.Unexpected unexpected ->
                                    Desc.uncertain unexpected

                        else
                            Outcome.Failure NoMatch

                    _ ->
                        Outcome.Failure NoMatch
        , fields = []
        }


selectedText sel =
    case sel of
        EmptyAnnotation ->
            []

        SelectText txts ->
            List.map textToTuple txts

        SelectString _ ->
            []


textToTuple (Desc.Text style str) =
    ( style, str )


selectedString sel =
    case sel of
        EmptyAnnotation ->
            ""

        SelectText txts ->
            ""

        SelectString str ->
            str


{-| A `verbatim` annotation is denoted by backticks(\`) and allows you to capture a literal string.

Just like `token` and `annotation`, a `verbatim` can have a name and attributes attached to it.

Let's say we wanted to embed an inline piece of elm code. We could write

    inlineElm =
        Mark.verbatim "elm"
            (\str ->
                Html.span
                    [ Html.Attributes.class "elm-code" ]
                    [ Html.text str ]
            )

Which would capture the following

```markup
Here's an inline function: `\you -> Awesome`{elm}.
```

**Note** A verbatim can be written without a name or attributes and will capture the contents as a literal string, ignoring any special characters.

```markup
Let's take a look at `http://elm-lang.com`.
```

-}
verbatim : String -> (String -> result) -> Record result
verbatim name view =
    Desc.ProtoRecord
        { name = name
        , blockKind = Desc.VerbatimNamed name
        , expectations = []
        , fieldConverter =
            \desc selected ->
                case desc of
                    Desc.Record details ->
                        if details.name == name then
                            case details.found of
                                Desc.Found pos fieldDescriptions ->
                                    Outcome.Success ( pos, fieldDescriptions, view (selectedString selected) )

                                Desc.Unexpected unexpected ->
                                    Desc.uncertain unexpected

                        else
                            Outcome.Failure NoMatch

                    _ ->
                        Outcome.Failure NoMatch
        , fields = []
        }



{- PRIMITIVE BLOCKS -}


{-| This will capture a multiline string.

For example:

    Mark.block "Poem"
        (\str -> str)
        Mark.string

will capture

```markup
|> Poem
    Whose woods these are I think I know.
    His house is in the village though;
    He will not see me stopping here
    To watch his woods fill up with snow.
```

Where `str` in the above function will be

    """Whose woods these are I think I know.
    His house is in the village though;
    He will not see me stopping here
    To watch his woods fill up with snow."""

**Note** If you're looking for styled text, you probably want [`Mark.text`](#text) or [`Mark.textWith`](#textWith).

-}
string : Block String
string =
    Block
        { kind = Value
        , expect = ExpectString "REPLACE"
        , converter =
            \desc ->
                case desc of
                    DescribeString id range str ->
                        Outcome.Success (String.trim str)

                    _ ->
                        Outcome.Failure NoMatch
        , parser =
            \context seed ->
                let
                    ( id, newSeed ) =
                        Id.step seed
                in
                ( newSeed
                , case context of
                    ParseInline ->
                        Parser.succeed
                            (\start str end ->
                                DescribeString id
                                    { start = start
                                    , end = end
                                    }
                                    (String.trim str)
                            )
                            |= Parse.getPosition
                            |= Parser.getChompedString
                                (Parser.chompWhile
                                    (\c -> c /= '\n' && c /= ',' && c /= '}')
                                )
                            |= Parse.getPosition

                    ParseBlock ->
                        Parser.map
                            (\( pos, str ) ->
                                DescribeString id pos str
                            )
                            (Parse.withRange
                                (Parse.withIndent
                                    (\indentation ->
                                        Parser.loop "" (Parse.indentedString indentation)
                                    )
                                )
                            )

                    ParseInTree ->
                        Parser.map
                            (\( pos, str ) ->
                                DescribeString id pos str
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


{-| Capture either `True` or `False`.
-}
bool : Block Bool
bool =
    Block
        { kind = Value
        , expect = ExpectBoolean False
        , converter =
            \desc ->
                case desc of
                    DescribeBoolean details ->
                        foundToOutcome details.found

                    _ ->
                        Outcome.Failure NoMatch
        , parser =
            \context seed ->
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


{-| -}
int : Block Int
int =
    Block
        { kind = Value
        , converter =
            \desc ->
                case desc of
                    DescribeInteger details ->
                        foundToOutcome details.found

                    _ ->
                        Outcome.Failure NoMatch
        , expect = ExpectInteger 0
        , parser =
            \context seed ->
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
    Block
        { kind = Value
        , converter =
            \desc ->
                case desc of
                    DescribeFloat details ->
                        foundToOutcome details.found
                            |> Outcome.mapSuccess Tuple.second

                    _ ->
                        Outcome.Failure NoMatch
        , expect = ExpectFloat 0
        , parser =
            \context seed ->
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


{-| This is a set of common character replacements with some typographical niceties.

  - `...` is converted to the ellipses unicode character(`…`).
  - `"` Straight double quotes are [replaced with curly quotes](https://practicaltypography.com/straight-and-curly-quotes.html) (`“`, `”`)
  - `'` Single Quotes are replaced with apostrophes(`’`).
  - `--` is replaced with an [en-dash(`–`)](https://practicaltypography.com/hyphens-and-dashes.html).
  - `---` is replaced with an [em-dash(`—`)](https://practicaltypography.com/hyphens-and-dashes.html).
  - `<>` also known as "glue", will create a non-breaking space (`&nbsp;`). This is not for manually increasing space (sequential `<>` tokens will only render as one `&nbsp;`), but to signify that the space between two words shouldn't break when wrapping. Like glueing two words together!
  - `//` will change to `/`. Normally `/` starts italic formatting. To escape this, we'd normally do `\/`, though that looks pretty funky. `//` just feels better!

**Note** this is included by default in `Mark.text`

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

For example, we could use this to replace `...` with the unicode ellipses character: `…`.

-}
replacement : String -> String -> Replacement
replacement =
    Parse.Replacement


{-| A balanced replacement. This is used for replacing parentheses or to do auto-curly quotes.

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



{- RECORDS -}


{-| -}
type alias Record a =
    Desc.Record a


{-| Parse a record with any number of fields.

    Mark.record "Image"
        (\src description ->
            Html.img
                [ Html.Attributes.src src
                , Html.Attributes.alt description
                ]
                []
        )
        |> Mark.field "src" Mark.string
        |> Mark.field "description" Mark.string
        |> Mark.toBlock

would parse the following markup:

```markup
|> Image
    src = http://placekitten/200/500
    description = What a cutie.
```

Fields can be in any order in the markup. Also, by convention field names should be `camelCase`. This might be enforced in the future.

-}
record : String -> data -> Record data
record name view =
    Desc.ProtoRecord
        { name = name
        , blockKind = Desc.Named name
        , expectations = []
        , fieldConverter =
            \desc ann ->
                case desc of
                    Desc.Record details ->
                        if details.name == name && ann == Desc.EmptyAnnotation then
                            case details.found of
                                Desc.Found pos fieldDescriptions ->
                                    Outcome.Success ( pos, fieldDescriptions, view )

                                Desc.Unexpected unexpected ->
                                    Desc.uncertain unexpected

                        else
                            Outcome.Failure NoMatch

                    _ ->
                        Outcome.Failure NoMatch
        , fields = []
        }


{-| -}
field : String -> Block value -> Record (value -> result) -> Record result
field name value (Desc.ProtoRecord details) =
    let
        newField =
            Field name value
    in
    Desc.ProtoRecord
        { name = details.name
        , blockKind = details.blockKind
        , expectations = fieldExpectation newField :: details.expectations
        , fieldConverter =
            \desc ann ->
                case details.fieldConverter desc ann of
                    Outcome.Success ( pos, fieldDescriptions, rendered ) ->
                        case getField newField fieldDescriptions of
                            Just outcome ->
                                mapSuccessAndRecovered
                                    (\myField ->
                                        ( pos
                                        , fieldDescriptions
                                        , rendered myField
                                        )
                                    )
                                    outcome

                            Nothing ->
                                Desc.uncertain
                                    { problem = Error.MissingFields [ fieldName newField ]
                                    , range = pos
                                    }

                    Outcome.Almost (Desc.Recovered e ( pos, fieldDescriptions, rendered )) ->
                        case getField newField fieldDescriptions of
                            Just outcome ->
                                mapSuccessAndRecovered
                                    (\myField ->
                                        ( pos
                                        , fieldDescriptions
                                        , rendered myField
                                        )
                                    )
                                    outcome

                            Nothing ->
                                Desc.uncertain
                                    { problem = Error.MissingFields [ fieldName newField ]
                                    , range = pos
                                    }

                    Outcome.Failure fail ->
                        Outcome.Failure fail

                    Outcome.Almost (Desc.Uncertain e) ->
                        Outcome.Almost (Desc.Uncertain e)
        , fields =
            fieldParser newField :: details.fields
        }


fieldName (Field name _) =
    name


getField :
    Field value
    -> List ( String, Desc.Found Desc.Description )
    -> Maybe (Outcome.Outcome Error.AstError (Uncertain value) value)
getField (Field name fieldBlock) fields =
    List.foldl (matchField name fieldBlock) Nothing fields


matchField :
    String
    -> Block value
    -> ( String, Desc.Found Desc.Description )
    -> Maybe (Outcome.Outcome Error.AstError (Uncertain value) value)
    -> Maybe (Outcome.Outcome Error.AstError (Uncertain value) value)
matchField targetName targetBlock ( name, foundDescription ) existing =
    case existing of
        Just _ ->
            existing

        Nothing ->
            if name == targetName then
                case foundDescription of
                    Desc.Found rng description ->
                        Just (Desc.renderBlock targetBlock description)

                    Desc.Unexpected unexpected ->
                        Just (Desc.uncertain unexpected)

            else
                existing


{-| Convert a `Record` to a `Block`.
-}
toBlock : Record a -> Block a
toBlock (Desc.ProtoRecord details) =
    let
        expectations =
            Desc.ExpectRecord details.name
                details.expectations
    in
    Desc.Block
        { kind = details.blockKind
        , expect = expectations
        , converter =
            \desc ->
                case details.fieldConverter desc Desc.EmptyAnnotation of
                    Outcome.Success ( pos, fieldDescriptions, rendered ) ->
                        Outcome.Success rendered

                    Outcome.Failure fail ->
                        Outcome.Failure fail

                    Outcome.Almost (Desc.Uncertain e) ->
                        Outcome.Almost (Desc.Uncertain e)

                    Outcome.Almost (Desc.Recovered e ( pos, fieldDescriptions, rendered )) ->
                        Outcome.Almost (Desc.Recovered e rendered)
        , parser =
            \context seed ->
                let
                    ( parentId, parentSeed ) =
                        Id.step seed

                    ( newSeed, fields ) =
                        Id.thread parentSeed (List.foldl (\f ls -> f ParseBlock :: ls) [] details.fields)
                in
                ( newSeed
                , Parse.record Parse.BlockRecord
                    parentId
                    details.name
                    expectations
                    fields
                )
        }


{-| -}
type Field value
    = Field String (Block value)


fieldParser : Field value -> Desc.ParseContext -> Id.Seed -> ( Id.Seed, ( String, Parser Context Problem ( String, Desc.Found Desc.Description ) ) )
fieldParser (Field name myBlock) context seed =
    let
        ( newSeed, blockParser ) =
            Desc.getParser context seed myBlock
    in
    ( newSeed
    , ( name
      , fieldContentParser
            name
            blockParser
      )
    )


fieldExpectation (Field name fieldBlock) =
    ( name, Desc.getBlockExpectation fieldBlock )


fieldContentParser : String -> Parser Error.Context Error.Problem Desc.Description -> Parser Error.Context Error.Problem ( String, Desc.Found Desc.Description )
fieldContentParser name parser =
    Parse.withIndent
        (\indentation ->
            Parser.map
                (\( pos, description ) ->
                    ( name, Desc.Found pos description )
                )
                (Parse.withRange
                    (Parser.succeed identity
                        |= Parser.oneOf
                            [ Parser.withIndent (indentation + 4) (Parser.inContext (InRecordField name) parser)
                            , Parser.succeed identity
                                |. Parser.chompWhile (\c -> c == '\n')
                                |. Parser.token (Parser.Token (String.repeat (indentation + 4) " ") (ExpectingIndentation indentation))
                                |= Parser.withIndent (indentation + 4) (Parser.inContext (InRecordField name) parser)
                            ]
                    )
                )
        )
