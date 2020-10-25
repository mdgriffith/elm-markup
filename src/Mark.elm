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
    , metadata, compile, parse, Parsed, toString, render
    , map, verify, onError
    , withId, withAttr, documentId, idToString, stringToId
    , lookup
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

@docs metadata, compile, parse, Parsed, toString, render


# Constraining and Recovering Blocks

@docs map, verify, onError

@docs withId, withAttr, documentId, idToString, stringToId

@docs lookup

-}

import Mark.Edit
import Mark.Error
import Mark.Internal.Description as Desc exposing (..)
import Mark.Internal.Error as Error exposing (AstError(..), Context(..), Problem(..))
import Mark.Internal.Id as Id exposing (..)
import Mark.Internal.Index as Index
import Mark.Internal.Outcome as Outcome
import Mark.Internal.Parser as Parse
import Mark.New exposing (block)
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
parse : Document metadata data -> String -> Outcome (List Mark.Error.Error) (Partial Parsed) Parsed
parse doc source =
    Desc.compile doc source
        |> moveParsedToResult


{-| -}
metadata : Document metadata document -> String -> Result Mark.Error.Error metadata
metadata (Desc.Document doc) source =
    case Parser.run doc.metadata source of
        Ok parsed ->
            parsed
                |> Result.mapError
                    (Error.render source)

        Err irrecoverableParsingErrors ->
            Err (Error.renderParsingErrors source irrecoverableParsingErrors)


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
render : Document meta data -> Parsed -> Outcome (List Mark.Error.Error) (Partial ( meta, List data )) ( meta, List data )
render doc ((Parsed parsedDetails) as parsed) =
    Desc.render doc parsed
        |> rewrapOutcome


{-| -}
compile : Document meta data -> String -> Outcome (List Mark.Error.Error) (Partial ( meta, List data )) ( meta, List data )
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
type alias Document meta data =
    Desc.Document meta data


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
            getUnexpecteds details.found

        Record details ->
            List.concatMap
                (Tuple.second >> getUnexpecteds)
                details.found

        Group many ->
            List.concatMap getUnexpecteds many.children

        StartsWith details ->
            getUnexpecteds details.first
                ++ getUnexpecteds details.second

        DescribeItem details ->
            List.concatMap getUnexpecteds details.content
                ++ List.concatMap getUnexpecteds details.children

        -- Primitives
        DescribeBoolean details ->
            []

        DescribeInteger details ->
            []

        DescribeFloat details ->
            []

        DescribeText details ->
            []

        DescribeString _ str ->
            []

        DescribeUnexpected _ details ->
            [ details ]


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
    List (Block block)
    -> Document () block
document blocks =
    createDocument (\_ -> "none")
        (Parser.succeed (Ok ()))
        (map (Tuple.pair ()) (manyOf blocks))


createDocument :
    (meta -> String)
    -> Parser Error.Context Error.Problem (Result Error.UnexpectedDetails meta)
    -> Block ( meta, List data )
    -> Document meta data
createDocument toDocumentId meta child =
    Document
        { expect =
            getBlockExpectation child
        , metadata =
            meta
        , converter =
            \(Parsed parsed) ->
                Desc.renderBlock child parsed.found
        , parser =
            Parser.getSource
                |> Parser.andThen
                    (\src ->
                        let
                            docId : String
                            docId =
                                case Parser.run meta src of
                                    Ok (Ok m) ->
                                        toDocumentId m

                                    _ ->
                                        -- This should return an error!
                                        -- it means the metadata is invalid
                                        ""

                            seed : Seed
                            seed =
                                Id.initialSeed docId

                            ( currentSeed, blockParser ) =
                                Parse.getFailableBlock Desc.ParseBlock seed child
                        in
                        Parser.succeed
                            (\source ( range, value ) ->
                                Parsed
                                    { errors =
                                        List.map (Error.render source) (getUnexpecteds value)
                                    , found = value
                                    , expected = getBlockExpectation child
                                    , initialSeed = seed
                                    , currentSeed = currentSeed
                                    , attributes = []
                                    }
                            )
                            |. Parser.chompWhile (\c -> c == '\n')
                            |= Parser.getSource
                            |= Parse.withRange (Parser.withIndent 0 blockParser)
                            |. Parser.chompWhile (\c -> c == ' ' || c == '\n')
                            |. Parser.end End
                    )
        }


{-| Capture some metadata at the start of your document, followed by the body.

    import Mark

    Mark.documentWith
        { id = \metadata -> metadata.id
        , metadata =
            Mark.record
                (\id author publishedAt ->
                    { author = author
                    , publishedAt = publishedAt
                    }
                )
                |> Mark.field "id" Mark.string
                |> Mark.field "author" Mark.string
                |> Mark.field "publishedAt" Mark.string
        , blocks =
            [

            ]
        }

**Note** - You can also specify an `id`, which is a document identifier and is included in `Mark.Edit.Id`. This is really only necessary if you're building an editor that can edit multiple documents at once.

Otherwise, feel free to simple put `id = \_ -> "doc"`

-}
documentWith :
    { id : metadata -> String
    , metadata : Record metadata
    , blocks : List (Block block)
    }
    -> Document metadata block
documentWith config =
    let
        metadataBlock =
            toBlock config.metadata
    in
    createDocument config.id
        (getMetadataParser metadataBlock)
        (startWith
            Tuple.pair
            metadataBlock
            (manyOf config.blocks)
        )


getMetadataParser metadataBlock =
    let
        ( _, metadataParser ) =
            Parse.getFailableBlock Desc.ParseBlock (Id.initialSeed "") metadataBlock
    in
    Parser.andThen
        (\description ->
            case renderBlock metadataBlock description of
                Outcome.Success meta ->
                    Parser.succeed (Ok meta.data)

                Outcome.Failure astError ->
                    Parser.succeed
                        (Err
                            { problem = Error.DocumentMismatch
                            , range = Desc.emptyRange
                            }
                        )

                Outcome.Almost (Uncertain ( unexpected, otherUnexpecteds )) ->
                    Parser.succeed (Err unexpected)

                Outcome.Almost (Recovered ( unexpected, otherUnexpecteds ) renderedChild) ->
                    Parser.succeed (Err unexpected)
        )
        (Parser.withIndent 0 metadataParser)


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
        , parser =
            \ctxt seed ->
                details.parser ctxt seed
                    |> Tuple.mapSecond
                        (\parser ->
                            parser
                                |> Parse.withRange
                                |> Parser.andThen
                                    (\( range, desc ) ->
                                        -- we only care about reporting if applying `fn` was a problem
                                        -- not other errors, which will shake out normally
                                        case details.converter desc of
                                            Outcome.Success a ->
                                                case fn a.data of
                                                    Ok new ->
                                                        Parser.succeed desc

                                                    Err newErr ->
                                                        Parser.succeed
                                                            (DescribeUnexpected (getId desc)
                                                                { problem = Error.Custom newErr
                                                                , range = range
                                                                }
                                                            )

                                            Outcome.Almost (Recovered err a) ->
                                                case fn a.data of
                                                    Ok new ->
                                                        Parser.succeed desc

                                                    Err newErr ->
                                                        Parser.succeed
                                                            (DescribeUnexpected (getId desc)
                                                                { problem = Error.Custom newErr
                                                                , range = range
                                                                }
                                                            )

                                            Outcome.Almost (Uncertain x) ->
                                                Parser.succeed desc

                                            Outcome.Failure f ->
                                                Parser.succeed desc
                                    )
                        )
        , converter =
            \desc ->
                case details.converter desc of
                    Outcome.Success a ->
                        case fn a.data of
                            Ok new ->
                                Outcome.Success { data = new, attrs = a.attrs }

                            Err newErr ->
                                uncertain
                                    { problem = Error.Custom newErr

                                    -- TODO: Does this mean we need to thread source snippets everywhere to get them here?
                                    , range = startDocRange
                                    }

                    Outcome.Almost (Recovered err a) ->
                        case fn a.data of
                            Ok new ->
                                Outcome.Almost (Recovered err { data = new, attrs = a.attrs })

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
documentId : Mark.Edit.Id -> String
documentId (Id.Id str _) =
    str


{-| Look up a specific block in your document by id.
-}
lookup : Mark.Edit.Id -> Document meta block -> Parsed -> Outcome (List Mark.Error.Error) (Partial block) block
lookup id doc parsed =
    Desc.lookup id doc parsed
        |> rewrapOutcome


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
                    id : Id
                    id =
                        Desc.getId desc
                in
                Desc.mapSuccessAndRecovered
                    (fn id)
                    (details.converter desc)
        , parser = details.parser
        , expect = details.expect
        }


{-| Get an `Id` associated with a `Block`, which can be used to make updates through `Mark.Edit`.

        Mark.string
            |> Mark.withAttr
                (\str -> ("link", str))

-}
withAttr : (a -> ( String, String )) -> Block a -> Block a
withAttr fn (Block details) =
    Block
        { kind = details.kind
        , converter =
            \desc ->
                let
                    id : Id
                    id =
                        Desc.getId desc
                in
                case details.converter desc of
                    Outcome.Success deets ->
                        let
                            new =
                                case fn deets.data of
                                    ( key, value ) ->
                                        { name = key
                                        , value = value
                                        , block = id
                                        }
                        in
                        Outcome.Success
                            { data = deets.data
                            , attrs = new :: deets.attrs
                            }

                    Outcome.Almost (Uncertain x) ->
                        Outcome.Almost (Uncertain x)

                    Outcome.Almost (Recovered errs deets) ->
                        let
                            new =
                                case fn deets.data of
                                    ( key, value ) ->
                                        { name = key
                                        , value = value
                                        , block = id
                                        }
                        in
                        Outcome.Almost
                            (Recovered errs
                                { data = deets.data
                                , attrs = new :: deets.attrs
                                }
                            )

                    Outcome.Failure errs ->
                        Outcome.Failure errs
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
                            (Recovered x { data = newValue, attrs = [] })

                    Outcome.Failure f ->
                        Outcome.Failure f
        }


{-| A named block.

    Mark.block "MyBlock"
        Html.text
        Mark.string

Will parse the following and render it using `Html.text`

    |> MyBlock
        Here is an unformatted string!

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
                            renderBlock child details.found
                                |> mapSuccessAndRecovered view

                        else
                            -- This is not the block that was expected.
                            Outcome.Failure NoMatch

                    _ ->
                        Outcome.Failure NoMatch
        , parser =
            \context seed ->
                let
                    ( newSeed, childParser ) =
                        getParser context (Id.indent seed) child

                    ( parentId, parentSeed ) =
                        Id.step seed

                    ( errorId, finalSeed ) =
                        Id.step parentSeed
                in
                ( finalSeed
                , Parser.map
                    (\result ->
                        case result of
                            Ok details ->
                                DescribeBlock
                                    { found = details.value
                                    , name = name
                                    , id = parentId
                                    }

                            Err details ->
                                DescribeBlock
                                    { name = name
                                    , id = parentId
                                    , found =
                                        DescribeUnexpected errorId
                                            { range = details.range
                                            , problem = details.error
                                            }
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
                        Desc.mergeWithAttrs fn
                            (Desc.renderBlock startBlock details.first)
                            (Desc.renderBlock endBlock details.second)

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
                            { id = parentId
                            , first = begin
                            , second = end
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
        expectations =
            List.map getBlockExpectation blocks
    in
    Block
        { kind = Value
        , expect = ExpectOneOf expectations
        , converter =
            \desc ->
                Desc.findMatch desc blocks
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
                    getRendered id existingResult children =
                        case children of
                            [] ->
                                mapSuccessAndRecovered List.reverse existingResult

                            top :: remain ->
                                getRendered id
                                    (Desc.mergeWithAttrs (::)
                                        (Desc.findMatch top blocks)
                                        existingResult
                                    )
                                    remain
                in
                case desc of
                    Group many ->
                        getRendered many.id
                            (Outcome.Success { data = [], attrs = [] })
                            many.children

                    _ ->
                        Outcome.Failure NoMatch
        , parser =
            \context seed ->
                let
                    ( parentId, newSeed ) =
                        Id.step seed

                    indentedSeed : Seed
                    indentedSeed =
                        Id.indent seed
                in
                ( newSeed
                , Parser.succeed
                    (\children ->
                        Group
                            { id = parentId
                            , children = children
                            }
                    )
                    |= Parse.withIndent
                        (\indentation ->
                            Parser.loop
                                { parsedSomething = False
                                , found = []
                                , seed =
                                    indentedSeed
                                }
                                (Parse.manyOf indentation blocks)
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

    |> List
        1. First
        -- Second
        -- Third

Is a numbered list. And this:

    |> List
        -- First
            1. sublist one
            -- sublist two
            -- sublist three
        -- Second
        -- Third

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
    (Enumerated item -> result)
    -> Block item
    -> Block result
tree view contentBlock =
    let
        blockExpectation =
            getBlockExpectation contentBlock

        expectation : Expectation
        expectation =
            ExpectTree blockExpectation
    in
    Block
        { kind = Value
        , expect = expectation
        , converter =
            \description ->
                case description of
                    Group details ->
                        details.children
                            |> reduceRender Index.zero
                                getItemIcon
                                (renderTreeNodeSmall contentBlock)
                            |> (\( _, icon, outcome ) ->
                                    case outcome of
                                        Outcome.Success nodes ->
                                            Outcome.Success
                                                { data =
                                                    view
                                                        (Enumerated
                                                            { icon =
                                                                case icon of
                                                                    Desc.Bullet ->
                                                                        Bullet

                                                                    Desc.AutoNumber _ ->
                                                                        Number
                                                            , items = List.map .data nodes
                                                            }
                                                        )
                                                , attrs = List.concatMap .attrs nodes
                                                }

                                        Outcome.Almost (Uncertain u) ->
                                            Outcome.Almost (Uncertain u)

                                        Outcome.Almost (Recovered errs nodes) ->
                                            Outcome.Almost
                                                (Recovered errs
                                                    { data =
                                                        view
                                                            (Enumerated
                                                                { icon =
                                                                    case icon of
                                                                        Desc.Bullet ->
                                                                            Bullet

                                                                        Desc.AutoNumber _ ->
                                                                            Number
                                                                , items = List.map .data nodes
                                                                }
                                                            )
                                                    , attrs = List.concatMap .attrs nodes
                                                    }
                                                )

                                        Outcome.Failure f ->
                                            Outcome.Failure f
                               )

                    _ ->
                        Outcome.Failure Error.NoMatch
        , parser =
            \context seed ->
                let
                    ( newId, newSeed ) =
                        Id.step seed

                    indentedSeed : Seed
                    indentedSeed =
                        Id.indent seed
                in
                ( newSeed
                , Parse.withIndent
                    (\baseIndent ->
                        let
                            ( secondSeed, itemParser ) =
                                getParser context indentedSeed contentBlock
                        in
                        Parser.map
                            (\( pos, builtTree ) ->
                                Group
                                    { id = newId
                                    , children = builtTree
                                    }
                            )
                            (Parse.withRange
                                (parseTree baseIndent secondSeed contentBlock itemParser)
                            )
                    )
                )
        }


parseTree baseIndent seed contentBlock itemParser =
    (Parser.succeed
        (\start icon item end ->
            { start = start
            , end = end
            , item = item
            , icon = icon
            }
        )
        |= Parse.getPosition
        |= Parse.iconParser
        |= itemParser
        |= Parse.getPosition
    )
        |> Parser.andThen
            (\details ->
                let
                    ( startId, startSeed ) =
                        Id.step seed
                in
                Parser.loop
                    ( { base = baseIndent
                      , prev = baseIndent
                      , seed = startSeed
                      }
                    , { previouslyAdded = Parse.AddedItem
                      , captured = []
                      , stack =
                            [ { start = details.start
                              , description =
                                    DescribeItem
                                        { id = startId
                                        , icon = details.icon
                                        , content =
                                            [ details.item
                                            ]
                                        , children = []
                                        }
                              }
                            ]
                      }
                    )
                    (Parse.fullTree
                        ParseInTree
                        contentBlock
                    )
            )


getItemIcon desc =
    case desc of
        DescribeItem item ->
            item.icon

        _ ->
            Desc.Bullet


{-| -}
renderTreeNodeSmall :
    Block item
    -> Desc.Icon
    -> Index.Index
    -> Description
    -> Desc.BlockOutcome (Item item)
renderTreeNodeSmall contentBlock icon index found =
    case found of
        DescribeItem item ->
            let
                ( newIndex, childrenIcon, renderedChildren ) =
                    reduceRender (Index.indent index)
                        getItemIcon
                        (renderTreeNodeSmall contentBlock)
                        item.children

                ( contentIndex, _, renderedContent ) =
                    reduceRender (Index.dedent newIndex)
                        (always Desc.Bullet)
                        (\icon_ i foundItem ->
                            renderBlock contentBlock foundItem
                        )
                        item.content
            in
            Desc.mergeListWithAttrs
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

        _ ->
            Outcome.Failure Error.NoMatch


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
                    -- Note #1 - seed for styled text is advanced here
                    --  instead of within the styledText parser
                    ( _, newSeed ) =
                        Id.step seed
                in
                ( newSeed
                , Parse.getPosition
                    |> Parser.andThen
                        (\pos ->
                            Parse.styledText
                                { inlines = List.map (\x -> x Desc.EmptyAnnotation) inlineRecords
                                , replacements = options.replacements
                                }
                                context
                                seed
                                pos
                                emptyStyles
                        )
                )
        }


recordToInlineBlock (Desc.ProtoRecord details) annotationType =
    let
        expectations : Expectation
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
                    Outcome.Success fields ->
                        case fields.data of
                            ( fieldDescriptions, rendered ) ->
                                Outcome.Success
                                    { data = rendered
                                    , attrs = fields.attrs
                                    }

                    Outcome.Failure fail ->
                        Outcome.Failure fail

                    Outcome.Almost (Desc.Uncertain e) ->
                        Outcome.Almost (Desc.Uncertain e)

                    Outcome.Almost (Desc.Recovered e fields) ->
                        case fields.data of
                            ( fieldDescriptions, rendered ) ->
                                Outcome.Almost
                                    (Desc.Recovered e
                                        { data = rendered
                                        , attrs = fields.attrs
                                        }
                                    )
        , parser =
            \context seed ->
                let
                    ( parentId, parentSeed ) =
                        Id.step seed

                    ( newSeed, fields ) =
                        Id.thread (Id.indent seed) (List.foldl (\f ls -> f ParseInline :: ls) [] details.fields)

                    ( failureId, finalSeed ) =
                        Id.step newSeed
                in
                ( finalSeed
                , Parse.record Parse.InlineRecord
                    parentId
                    failureId
                    details.name
                    expectations
                    fields
                )
        }


type alias Cursor data =
    { outcome : BlockOutcome data
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
    , inlines : List (InlineSelection -> Block rendered)
    }
    -> Description
    -> BlockOutcome (List rendered)
renderText options description =
    case description of
        DescribeText details ->
            let
                outcome =
                    details.text
                        |> List.foldl (convertTextDescription details.id options)
                            { outcome = Outcome.Success { data = [], attrs = [] }
                            , lastOffset = 0
                            }
                        |> .outcome
            in
            outcome
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
        , inlines : List (InlineSelection -> Block rendered)
        }
    -> TextDescription
    -> Cursor (List rendered)
    -> Cursor (List rendered)
convertTextDescription id options comp cursor =
    let
        blockLength : Int
        blockLength =
            length comp
    in
    case comp of
        Styled (Desc.Text styling str) ->
            { outcome =
                Desc.mergeWithAttrs (::)
                    (Outcome.Success
                        { data =
                            options.view
                                { id = id
                                , selection =
                                    { anchor = cursor.lastOffset
                                    , focus = cursor.lastOffset + blockLength
                                    }
                                }
                                styling
                                str
                        , attrs = []
                        }
                    )
                    cursor.outcome
            , lastOffset =
                cursor.lastOffset + blockLength
            }

        InlineBlock details ->
            case details.record of
                DescribeUnexpected unexpId unexpDetails ->
                    { outcome =
                        Desc.uncertain unexpDetails
                    , lastOffset = cursor.lastOffset + blockLength
                    }

                _ ->
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
                                Outcome.Failure NoMatch
                            , lastOffset = cursor.lastOffset + blockLength
                            }

                        Just matched ->
                            { outcome =
                                Desc.mergeWithAttrs (::)
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


{-| An annotation is some _styled text_, a _name_, and zero or more _attributes_.

So, we can make a `link` that looks like this in markup:

```markup
Here is my [*cool* sentence]{link| url = website.com }.
```

and rendered in elm-land via:

    link =
        Mark.annotation "link"
            (\id styles url ->
                Html.a
                    [ Html.Attributes.href url ]
                    (List.map renderStyles styles)
            )
            |> Mark.field "url" Mark.string

-}
annotation : String -> (Id -> List ( Styles, String ) -> result) -> Record result
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
                            Outcome.Success
                                { data =
                                    ( details.found
                                    , view (getId desc) (selectedText selected)
                                    )
                                , attrs = []
                                }

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

Just like `annotation`, a `verbatim` can have a name and attributes attached to it.

Let's say we wanted to embed an inline piece of elm code. We could write

    inlineElm =
        Mark.verbatim "elm"
            (\id str ->
                Html.span
                    [ Html.Attributes.class "elm-code" ]
                    [ Html.text str ]
            )

Which would capture the following

    Here's an inline function: `\you -> Awesome`{elm}.

**Note** A verbatim can be written without a name or attributes and will capture the contents as a literal string, ignoring any special characters.

    Let's take a look at `http://elm-lang.com`.

-}
verbatim : String -> (Id -> String -> result) -> Record result
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
                            Outcome.Success
                                { attrs = []
                                , data = ( details.found, view (getId desc) (selectedString selected) )
                                }

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
                    DescribeString id str ->
                        Outcome.Success { data = String.trim str, attrs = [] }

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
                                DescribeString id str
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
                            (DescribeString id)
                            (Parse.withIndent
                                (\indentation ->
                                    Parser.loop "" (Parse.indentedString indentation)
                                )
                            )
                )
        }


{-| Capture either `True` or `False`.

`elm-markup` doesn't infer truthiness in other values, so it needs to be exactly `True` or `False`.

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
                        Outcome.Success
                            { data = details.found
                            , attrs = []
                            }

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
                        case boolResult of
                            Err err ->
                                DescribeUnexpected id
                                    { range = err.range
                                    , problem = Error.BadBool
                                    }

                            Ok details ->
                                DescribeBoolean
                                    { id = id
                                    , found = details.value
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


{-| -}
int : Block Int
int =
    Block
        { kind = Value
        , converter =
            \desc ->
                case desc of
                    DescribeInteger details ->
                        Outcome.Success
                            { data = details.found
                            , attrs = []
                            }

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
                , Parse.int id
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
                        Outcome.Success
                            { data = Tuple.second details.found
                            , attrs = []
                            }

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
                , Parse.float id
                )
        }



{- Parser Heleprs -}


{-| This is a set of common character replacements with some typographical niceties.

  - `...` is converted to the ellipses unicode character(`…`).
  - `"` Straight double quotes are [replaced with curly quotes](https://practicaltypography.com/straight-and-curly-quotes.html) (`“`, `”`)
  - `'` Single Quotes are replaced with apostrophes(`’`).
  - `--` is replaced with an [en-dash(`–`)](https://practicaltypography.com/hyphens-and-dashes.html).
  - `---` is replaced with an [em-dash(`—`)](https://practicaltypography.com/hyphens-and-dashes.html).
  - `<>` also known as "glue", will create a non-breaking space (`&nbsp;`). This is not for manually increasing space (sequential `<>` tokens will only render as one `&nbsp;`), but to signify that the space between two words shouldn't break when wrapping. Like glueing two words together!

**Note** this is included by default in `Mark.text`

-}
commonReplacements : List Replacement
commonReplacements =
    [ Parse.Replacement "..." "…"
    , Parse.Replacement "<>" "\u{00A0}"
    , Parse.Replacement "---" "—"
    , Parse.Replacement "--" "–"
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
                            Outcome.Success { data = ( details.found, view ), attrs = [] }

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
                    Outcome.Success fields ->
                        case fields.data of
                            ( fieldDescriptions, rendered ) ->
                                case getField newField fieldDescriptions of
                                    Just outcome ->
                                        mapSuccessAndRecovered
                                            (\myField ->
                                                ( fieldDescriptions
                                                , rendered myField
                                                )
                                            )
                                            outcome

                                    Nothing ->
                                        Desc.uncertain
                                            { problem = Error.MissingFields [ fieldName newField ]
                                            , range = Desc.emptyRange
                                            }

                    Outcome.Almost (Desc.Recovered e fields) ->
                        case fields.data of
                            ( fieldDescriptions, rendered ) ->
                                case getField newField fieldDescriptions of
                                    Just outcome ->
                                        mapSuccessAndRecovered
                                            (\myField ->
                                                ( fieldDescriptions
                                                , rendered myField
                                                )
                                            )
                                            outcome

                                    Nothing ->
                                        Desc.uncertain
                                            { problem = Error.MissingFields [ fieldName newField ]
                                            , range = Desc.emptyRange
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


{-| TODO: convert to recursion
-}
getField :
    Field value
    -> List ( String, Desc.Description )
    -> Maybe (Desc.BlockOutcome value)
getField (Field name fieldBlock) fields =
    List.foldl (matchField name fieldBlock) Nothing fields


matchField :
    String
    -> Block value
    -> ( String, Desc.Description )
    -> Maybe (Desc.BlockOutcome value)
    -> Maybe (Desc.BlockOutcome value)
matchField targetName targetBlock ( name, description ) existing =
    case existing of
        Just _ ->
            existing

        Nothing ->
            if name == targetName then
                Just (Desc.renderBlock targetBlock description)

            else
                existing


{-| Convert a `Record` to a `Block`.
-}
toBlock : Record a -> Block a
toBlock (Desc.ProtoRecord details) =
    let
        expectations : Expectation
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
                    Outcome.Success fields ->
                        case fields.data of
                            ( fieldDescriptions, rendered ) ->
                                Outcome.Success { data = rendered, attrs = fields.attrs }

                    Outcome.Failure fail ->
                        Outcome.Failure fail

                    Outcome.Almost (Desc.Uncertain e) ->
                        Outcome.Almost (Desc.Uncertain e)

                    Outcome.Almost (Desc.Recovered e fields) ->
                        case fields.data of
                            ( fieldDescriptions, rendered ) ->
                                Outcome.Almost
                                    (Desc.Recovered e
                                        { data = rendered
                                        , attrs = fields.attrs
                                        }
                                    )
        , parser =
            \context seed ->
                let
                    ( parentId, parentSeed ) =
                        Id.step seed

                    ( newSeed, fields ) =
                        Id.thread (Id.indent seed)
                            (List.foldl (\f ls -> f ParseBlock :: ls) [] details.fields)

                    ( failureId, finalSeed ) =
                        Id.step newSeed
                in
                ( parentSeed
                , Parse.record Parse.BlockRecord
                    parentId
                    failureId
                    details.name
                    expectations
                    fields
                )
        }


{-| -}
type Field value
    = Field String (Block value)


fieldParser :
    Field value
    -> Desc.ParseContext
    -> Id.Seed
    -> ( Id.Seed, ( String, Parser Context Problem ( String, Desc.Description ) ) )
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


fieldContentParser : String -> Parser Error.Context Error.Problem Desc.Description -> Parser Error.Context Error.Problem ( String, Desc.Description )
fieldContentParser name parser =
    Parse.withIndent
        (\indentation ->
            Parser.map
                (\( pos, description ) ->
                    ( name, description )
                )
                (Parse.withRange
                    (Parser.oneOf
                        [ Parser.withIndent (indentation + 4) (Parser.inContext (InRecordField name) parser)
                        , Parser.succeed identity
                            |. Parser.chompWhile (\c -> c == '\n')
                            |. Parser.token (Parser.Token (String.repeat (indentation + 4) " ") (ExpectingIndentation indentation))
                            |= Parser.withIndent (indentation + 4) (Parser.inContext (InRecordField name) parser)
                        ]
                    )
                )
        )
