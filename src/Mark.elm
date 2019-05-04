module Mark exposing
    ( Document
    , document, documentWith
    , Block, block
    , string, int, float, bool
    , Styles, text
    , textWith
    , Replacement, commonReplacements, replacement, balanced
    , Inline, annotation, verbatim, token
    , attrString, attrFloat, attrInt
    , oneOf, manyOf
    , tree
    , Outcome(..), Partial
    , compile, parse, Parsed, toString, render
    , map, verify, onError
    )

{-|

@docs Document


# Building Documents

@docs document, documentWith

@docs Block, block


# Primitives

@docs string, int, float, bool


# Text

@docs Styles, text

@docs textWith


# Text Replacements

@docs Replacement, commonReplacements, replacement, balanced


# Text Annotations

Along with basic [`styling`](#text) and [`replacements`](#replacement), we also have a few ways to annotate text.

@docs Inline, annotation, verbatim, token

@docs attrString, attrFloat, attrInt


# Higher Level

@docs oneOf, manyOf


# Trees

@docs tree


# Rendering

@docs Outcome, Partial

@docs compile, parse, Parsed, toString, render


# Constraining and Recovering Blocks

@docs map, verify, onError

-}

import Html
import Html.Attributes
import Mark.Edit
import Mark.Error
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

        DescribeMultiline rng _ str ->
            []

        DescribeNothing _ ->
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
    Mark.Edit.Error


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


{-| Capture some metadata at the start of your document, followed by the body.

    Mark.documentWith
        (\metadata body ->
            { metadata = metadata
            , body = body
            }
        )
        { metadata =
            Mark.record
                (\author publishedAt ->
                    { author = author
                    , publishedAt = publishedAt
                    }
                )
                |> Mark.field "author" Mark.string
                |> Mark.field "publishedAt" Mark.string
                |> Mark.close
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


{-| `Mark.verify` lets you put whatever constraints you want on a block.

Let's say you don't just want a `Mark.string`, you actually want a date.

So, you install the [`ISO8601`](https://package.elm-lang.org/packages/rtfeldman/elm-iso8601-date-strings/latest/) and you write something that looks like the follwing:

    import Iso8601
    import Mark.Error

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
  - ensure that numbers are between a range, or are always positive.

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


skipSeed parser seed =
    ( seed, parser )


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
            \seed ->
                let
                    ( newSeed, childParser ) =
                        getParser seed child

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
            \seed ->
                let
                    ( parentId, newSeed ) =
                        Id.step seed

                    ( startSeed, startParser ) =
                        getParser newSeed startBlock

                    ( remainSeed, endParser ) =
                        getParser startSeed endBlock
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


{-| It can be useful to parse a tree structure. For example, here's a nested list.

    |> List
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


{-| Handling formatted text is a little more involved than may be initially apparent.

But `textWith` is where a lot of things come together. Let's check out what these fields actually mean.

  - `view` is the function to render an individual fragment of text. This is mostly what [`Mark.text`](#text) does, so it should seem familiar.
  - `replacements` will replace characters before rendering. For example, we can replace `...` with the real ellipses unicode character, `…`.
  - `inlines` are custom inline blocks. You can use these to render things like links or render emojis :D.

-}
textWith :
    { view : Styles -> String -> rendered
    , replacements : List Replacement
    , inlines : List (Inline rendered)
    }
    -> Block (List rendered)
textWith options =
    Mark.Edit.text
        { view = always options.view
        , inlines = options.inlines
        , replacements = options.replacements
        }


{-| -}
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


{-| -}
type alias Inline data =
    Desc.Inline data


{-| Lastly, a `token` is like an annotation but has no text that it's attached to, it will just insert a certain value.

Maybe the easiest usecase to think of would be to insert an emoji or an icon:

```markup
My markup with a {smilie}.
```

Could be created via

    emoji =
        Mark.token "smilie" (Html.text 😄)

-}
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


{-| An annotation is some **text**, a **name**, and zero or more **attributes**.

Here's what it looks like in markup.

```markup
[ My / styled / text ] { name | attr1 = 5, attr5 = yes }
```

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
            |> Mark.withString "url"

-}
annotation : String -> (List ( Styles, String ) -> result) -> Inline result
annotation name result =
    Inline
        { converter =
            \textPieces attrs ->
                Outcome.Success [ result (List.map textToTuple textPieces) ]
        , expect =
            ExpectAnnotation name [] []
        , name = name
        }


textToTuple (Desc.Text style str) =
    ( style, str )


{-| A `verbatim` annotation is denoted by backticks(\`) and allows you to capture a literal string.

Just like `token` and `annotation`, a `verbatim` can have a name and attributes attached to it as well via:

```markup
Here's an inline function: `\you -> Awesome`{elm}.
```

**Note** A verbatim can be written without a name or attributes. So, the following is potentially valid:

```markup
Let's take a look at `http://elm-lang.com`.
```

It will match the first `verbatim` definition listed in `textWith` that has no attributes.

-}
verbatim : String -> (String -> result) -> Inline result
verbatim name result =
    Inline
        { converter =
            \textPieces attrs ->
                case textPieces of
                    [] ->
                        -- This should never happen
                        Outcome.Failure NoMatch

                    (Desc.Text _ fst) :: _ ->
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

**Note** If you're looking for styled text, you probably want [`Mark.text`](#text) or [`Mark.textWith`](#textWith) instead.

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
            \seed ->
                let
                    ( id, newSeed ) =
                        Id.step seed
                in
                ( newSeed
                , Parser.map
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



-- {-| -}
-- string : Block String
-- string =
--     Block
--         { kind = Value
--         , expect = ExpectString "-- Replace Me --"
--         , converter =
--             \desc ->
--                 case desc of
--                     DescribeString id range str ->
--                         Outcome.Success str
--                     _ ->
--                         Outcome.Failure NoMatch
--         , parser =
--             \seed ->
--                 let
--                     ( id, newSeed ) =
--                         Id.step seed
--                 in
--                 ( newSeed
--                 , Parser.succeed
--                     (\start val end ->
--                         DescribeString id
--                             { start = start
--                             , end = end
--                             }
--                             val
--                     )
--                     |= Parse.getPosition
--                     |= Parser.getChompedString
--                         (Parser.chompWhile
--                             (\c -> c /= '\n')
--                         )
--                     |= Parse.getPosition
--                 )
--         }


foundToResult found err =
    case found of
        Found _ b ->
            Ok b

        _ ->
            Err err


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
{- RECORD RENDERER HELPERS -}


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
