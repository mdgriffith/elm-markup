module Mark exposing
    ( parse
    , Document, document
    , Block, block, stub, bool, date
    , int, intBetween, float, floatBetween
    , string, multiline, exactly
    , map
    , text, Text, Style(..)
    , Inline, inline, inlineString, inlineText
    , Replacement, replacement, balanced
    , oneOf, manyOf, startWith
    , nested, Nested
    , Field, field, record2
    , Error, errorToString
    )

{-| `elm-markup` is about defining what you're expecting in a markup document.

The `elm-markup` language relies heavily on indentation, which is always some multiple of 4 spaces.

@docs parse


# Blocks

@docs Document, document

@docs Block, block, stub, bool, date


# Numbers

@docs int, intBetween, float, floatBetween


# Strings

@docs string, multiline, exactly


# Mapping

@docs map


# Text

@docs text, Text, Style

@docs Inline, inline, inlineString, inlineText

@docs Replacement, replacement, balanced


# Blocks

@docs oneOf, manyOf, startWith

@docs nested, Nested


# Records

Embed a data record.

Here's an implementation to render an `<img>`

    Mark.record2 "Image"
        (\src description ->
            Html.img [ Html.Attribute.src src, Html.Attribute.alt description ] []
        )
        (Mark.field "src" Mark.string)
        (Mark.field "description" Mark.string)

Which would parse

    | Image
        src = http://placekitten/200/500
        description = A super cute kitten.

**Note** The order of the fields in the document does not matter, so this will parse correctly as well:

    | Image
        description = A super cute kitten.
        src = http://placekitten/200/500

@docs Field, field, record2


# Errors

@docs Error, errorToString

-}

import Html
import Html.Attributes
import Iso8601
import Mark.Advanced as Advanced
import Mark.Description as Description
import Mark.Format as Format
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)
import Time


{-| -}
parse : Document result -> String -> Result (List Error) result
parse doc source =
    case Advanced.compile doc source of
        Advanced.Success parsed ->
            case parsed of
                Just result ->
                    Ok result

                Nothing ->
                    -- This only happens if there was an error parsing or compiling
                    -- Technically this branch should never execute (it would go to Almost or Failure)
                    Err []

        Advanced.Almost partial ->
            Err partial.errors

        Advanced.Failure failure ->
            Err [ failure ]


{-| -}
type alias Document result =
    Advanced.Document (Maybe result)


{-| -}
type alias Replacement =
    Advanced.Replacement


{-| -}
type alias Text =
    Description.Text


{-| Create a markup `Document`. You're first goal is to describe a document in terms of the blocks you're expecting.

Here's a brief example of a document that will parse many `MyBlock` blocks, and capture the string that's indented inside the block.

    document : Mark.Document (List String)
    document =
        Mark.document
            identity
            (Mark.manyOf
                [ Mark.block "MyBlock"
                    identity
                    Mark.string
                , Mark.string
                ]
            )

    parse : String -> Result (DeadEnd Context Problem) (List String)
    parse source =
        Mark.parse document source

Which will parse a document with many of either raw strings or`MyBlock` blocks. The `identity` function usage above means that this will result in a `List String`, but you could change that function to make this parser result in anything you want!

    | MyBlock
        Here's text captured in a MyBlock

    This text is captured at the top level.

    | MyBlock
        Here's some more text in a MyBlock.

-}
document : (child -> result) -> Block child -> Document result
document renderer child =
    Advanced.document
        { view =
            \_ doc -> Maybe.map renderer doc
        , error = always Nothing
        }
        child


{-| A `Block data` is just a parser that results in `data`.

You'll be building up your `Document` in terms of the `Blocks`.

-}
type alias Block data =
    Advanced.Block (Maybe data)


{-| -}
type Style
    = Bold
    | Italic
    | Strike


{-| A named block.

    Mark.block "MyBlock"
        Html.text
        Mark.string

Will parse the following and render it as `Html.text`

    | MyBlock
        Here is an unformatted string!

**Note** block names should be capitalized. In the future this may be enforced.

-}
block : String -> (child -> result) -> Block child -> Block result
block name renderer child =
    Advanced.block
        { name = name
        , view =
            \_ blockResult ->
                Maybe.map renderer blockResult
        , error = always Nothing
        }
        child


{-| An empty block. This could be useful for inserting something that doesn't need parameters.

    | Logo

-}
stub : String -> result -> Block result
stub name result =
    Advanced.stub name (\rng -> Just result)


{-| Parse two blocks in sequence.

This can be useful to do things like require that a specific block comes first.

So, the classic case of a blog post that has some meta data at the start could be done like this

    Mark.startWith
        (\metadata article ->
            { meta = metadata
            , article = article
            }
        )
        (Mark.record3 "Article"
            (\author title published ->
                { author = author
                , title = title
                , published = published
                }
            )
            (Mark.field "author" Mark.string)
            (Mark.field "title" Mark.string)
            (Mark.field "published" Mark.bool)
        )
        (Mark.manyOf [ Mark.text ])

Which would parse the following doc:

    | Article
        author = Mortimer, the Stylish Elephant
        published = False
        title = How to use CSS to rearrange your living room.

    First off, thank you for using my library.

    Secondly, who knew that flex-box could wrap a sofa?

-}
startWith : (start -> rest -> result) -> Block start -> Block rest -> Block result
startWith fn start rest =
    Advanced.startWith
        { view =
            \_ maybeOne maybeTwo ->
                case ( maybeOne, maybeTwo ) of
                    ( Just one, Just two ) ->
                        Just (fn one two)

                    _ ->
                        Nothing
        , error = always Nothing
        }
        start
        rest



-- {-| Define your own parser using [`elm/parser`](https://package.elm-lang.org/packages/elm/parser/latest/).
-- This would be the place to start if you wanted to create a parser for syntax highlighting for an Elm block.
-- **Warning** This is the only place where you can break the rules/conventions of `elm-markup`.
-- **Note** You can get the current expected indentation using [`Parser.Advanced.getIndent`](https://package.elm-lang.org/packages/elm/parser/latest/Parser-Advanced#getIndent).
-- -}
-- advanced : Parser Context Problem result -> Block result
-- advanced parser =
--     Value parser
-- {-| Expect a different block based on the result of a previous one.
-- -}
-- andThen : (a -> Block b) -> Block a -> Block b
-- andThen fn myBlock =
--     case myBlock of
--         Block name blockParser ->
--             Block name
--                 (Parser.andThen (getParser << fn) blockParser)
--         Value blockParser ->
--             Value
--                 (Parser.andThen (getParser << fn) blockParser)


{-| Change the result of a block by applying a function to it.
-}
map : (a -> b) -> Block a -> Block b
map fn child =
    Advanced.map (Maybe.map fn) child


{-| -}
type alias Nested item =
    Description.Nested item


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
        (\(Nested nested) ->
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
    Advanced.map
        (\items ->
            allPresent <|
                List.indexedMap
                    (\_ item ->
                        Advanced.replaceNested
                            (\index ( icon, content ) children ->
                                case ( icon, allPresent content, allPresent children ) of
                                    ( Just ic, Just ct, Just childs ) ->
                                        Just <|
                                            Description.Nested
                                                { content = ( ic, ct )
                                                , children = childs
                                                }

                                    _ ->
                                        Nothing
                            )
                            item
                    )
                    items
        )
        (Advanced.nested config)


allPresent : List (Maybe a) -> Maybe (List a)
allPresent listOfMaybes =
    Maybe.map List.reverse <|
        List.foldl
            (\maybeItem maybeLs ->
                case maybeLs of
                    Nothing ->
                        maybeLs

                    Just ls ->
                        case maybeItem of
                            Nothing ->
                                Nothing

                            Just item ->
                                Just (item :: ls)
            )
            (Just [])
            listOfMaybes


{-| Many blocks that are all at the same indentation level.
-}
manyOf : List (Block a) -> Block (List a)
manyOf options =
    Advanced.map
        allPresent
        (Advanced.manyOf
            { view = \_ a -> a
            , error = \_ _ -> Nothing
            }
            options
        )


{-| Parse an ISO-8601 date string.

Format: `YYYY-MM-DDTHH:mm:ss.SSSZ`

Though you don't need to specify all segments, so `YYYY-MM-DD` works as well.

Results in a `Posix` integer, which works well with [elm/time](https://package.elm-lang.org/packages/elm/time/latest/).

-}
date : Block Time.Posix
date =
    Advanced.date
        { default = Time.millisToPosix 0
        , view = \id_ str -> Just str
        }


{-| -}
oneOf : List (Block a) -> Block a
oneOf options =
    Advanced.oneOf
        { view = \_ a -> a
        , error = always Nothing
        }
        options


{-| Parse an exact string. This can be useful to parse custom types if you pair it with `Mark.oneOf`.

    type Plant = IsWatered | IsDying


    Mark.oneOf
        [ Mark.exactly "IsWatered" IsWatered
        , Mark.exactly "IsDying" IsDying
        ]

-}
exactly : String -> value -> Block value
exactly key val =
    Advanced.exactly key (Just val)


{-| -}
int : Block Int
int =
    Advanced.map Just
        (Advanced.int
            { default = 0
            , view = \id_ i -> i
            }
        )


{-| Parse an `Int` within an inclusive range.

    Mark.intBetween 0 100

Will parse any Int between 0 and 100.

-}
intBetween : Int -> Int -> Block Int
intBetween bottom top =
    Advanced.map Just
        (Advanced.intBetween
            { min = bottom
            , max = top
            , default = min bottom top
            , view = \id_ i -> i
            }
        )


{-| -}
float : Block Float
float =
    Advanced.map Just
        (Advanced.float
            { default = 0
            , view = \id_ i -> i
            }
        )


{-| Parse a `Float` within an inclusive range.

    Mark.floatBetween 0 1

Will parse any Float between 0 and 1.

-}
floatBetween : Float -> Float -> Block Float
floatBetween bottom top =
    Advanced.map Just
        (Advanced.floatBetween
            { min = bottom
            , max = top
            , default = min bottom top
            , view = \id_ f -> f
            }
        )


{-| Parse either `True` or `False`.
-}
bool : Block Bool
bool =
    Advanced.map Just
        (Advanced.bool
            { default = True
            , view = \id_ b -> b
            }
        )


{-| Parse a single line and return it as a string.
-}
string : Block String
string =
    Advanced.map Just
        (Advanced.string
            { default = "A String"
            , view = \id_ str -> str
            }
        )


{-| Parse multiple lines at the current indentation level.

For example:

    Mark.block "Poem"
        (\str -> str)
        Mark.multiline

Will parse the following:

    | Poem
        Whose woods these are I think I know.
        His house is in the village though;
        He will not see me stopping here
        To watch his woods fill up with snow.

Where `str` in the above function will be

    """Whose woods these are I think I know.
    His house is in the village though;
    He will not see me stopping here
    To watch his woods fill up with snow."""

-}
multiline : Block String
multiline =
    Advanced.map Just
        (Advanced.multiline
            { default = "A String"
            , view = \id_ str -> str
            }
        )


{-| -}
type alias Field value =
    Advanced.Field (Maybe value)


{-| A record with two fields.

**Note** there's no `record1`, because that's basically just a `block`.

-}
record2 :
    String
    -> (one -> two -> data)
    -> Field one
    -> Field two
    -> Block data
record2 recordName renderer field1 field2 =
    Advanced.record2
        { name = recordName
        , view =
            \range one two ->
                case ( one, two ) of
                    ( Just o, Just t ) ->
                        Just <| renderer o t

                    _ ->
                        Nothing
        , error = always Nothing
        }
        field1
        field2



-- {-| -}
-- record3 :
--     String
--     -> (one -> two -> three -> data)
--     -> Field one
--     -> Field two
--     -> Field three
--     -> Block data
-- record3 recordName renderer field1 field2 field3 =
--     let
--         recordParser =
--             Parser.succeed renderer
--                 |= fieldParser field1
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field2
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field3
--     in
--     masterRecordParser recordName [ fieldName field1, fieldName field2, fieldName field3 ] recordParser
-- {-| -}
-- record4 :
--     String
--     -> (one -> two -> three -> four -> data)
--     -> Field one
--     -> Field two
--     -> Field three
--     -> Field four
--     -> Block data
-- record4 recordName renderer field1 field2 field3 field4 =
--     let
--         recordParser =
--             Parser.succeed renderer
--                 |= fieldParser field1
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field2
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field3
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field4
--     in
--     masterRecordParser recordName [ fieldName field1, fieldName field2, fieldName field3, fieldName field4 ] recordParser
-- {-| -}
-- record5 :
--     String
--     -> (one -> two -> three -> four -> five -> data)
--     -> Field one
--     -> Field two
--     -> Field three
--     -> Field four
--     -> Field five
--     -> Block data
-- record5 recordName renderer field1 field2 field3 field4 field5 =
--     let
--         recordParser =
--             Parser.succeed renderer
--                 |= fieldParser field1
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field2
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field3
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field4
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field5
--     in
--     masterRecordParser recordName
--         [ fieldName field1
--         , fieldName field2
--         , fieldName field3
--         , fieldName field4
--         , fieldName field5
--         ]
--         recordParser
-- {-| -}
-- record6 :
--     String
--     ->
--         (one
--          -> two
--          -> three
--          -> four
--          -> five
--          -> six
--          -> data
--         )
--     -> Field one
--     -> Field two
--     -> Field three
--     -> Field four
--     -> Field five
--     -> Field six
--     -> Block data
-- record6 recordName renderer field1 field2 field3 field4 field5 field6 =
--     let
--         recordParser =
--             Parser.succeed renderer
--                 |= fieldParser field1
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field2
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field3
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field4
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field5
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field6
--     in
--     masterRecordParser recordName
--         [ fieldName field1
--         , fieldName field2
--         , fieldName field3
--         , fieldName field4
--         , fieldName field5
--         , fieldName field6
--         ]
--         recordParser
-- {-| -}
-- record7 :
--     String
--     ->
--         (one
--          -> two
--          -> three
--          -> four
--          -> five
--          -> six
--          -> seven
--          -> data
--         )
--     -> Field one
--     -> Field two
--     -> Field three
--     -> Field four
--     -> Field five
--     -> Field six
--     -> Field seven
--     -> Block data
-- record7 recordName renderer field1 field2 field3 field4 field5 field6 field7 =
--     let
--         recordParser =
--             Parser.succeed renderer
--                 |= fieldParser field1
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field2
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field3
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field4
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field5
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field6
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field7
--     in
--     masterRecordParser recordName
--         [ fieldName field1
--         , fieldName field2
--         , fieldName field3
--         , fieldName field4
--         , fieldName field5
--         , fieldName field6
--         , fieldName field7
--         ]
--         recordParser
-- {-| -}
-- record8 :
--     String
--     ->
--         (one
--          -> two
--          -> three
--          -> four
--          -> five
--          -> six
--          -> seven
--          -> eight
--          -> data
--         )
--     -> Field one
--     -> Field two
--     -> Field three
--     -> Field four
--     -> Field five
--     -> Field six
--     -> Field seven
--     -> Field eight
--     -> Block data
-- record8 recordName renderer field1 field2 field3 field4 field5 field6 field7 field8 =
--     let
--         recordParser =
--             Parser.succeed renderer
--                 |= fieldParser field1
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field2
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field3
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field4
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field5
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field6
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field7
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field8
--     in
--     masterRecordParser recordName
--         [ fieldName field1
--         , fieldName field2
--         , fieldName field3
--         , fieldName field4
--         , fieldName field5
--         , fieldName field6
--         , fieldName field7
--         , fieldName field8
--         ]
--         recordParser
-- {-| -}
-- record9 :
--     String
--     ->
--         (one
--          -> two
--          -> three
--          -> four
--          -> five
--          -> six
--          -> seven
--          -> eight
--          -> nine
--          -> data
--         )
--     -> Field one
--     -> Field two
--     -> Field three
--     -> Field four
--     -> Field five
--     -> Field six
--     -> Field seven
--     -> Field eight
--     -> Field nine
--     -> Block data
-- record9 recordName renderer field1 field2 field3 field4 field5 field6 field7 field8 field9 =
--     let
--         recordParser =
--             Parser.succeed renderer
--                 |= fieldParser field1
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field2
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field3
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field4
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field5
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field6
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field7
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field8
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field9
--     in
--     masterRecordParser recordName
--         [ fieldName field1
--         , fieldName field2
--         , fieldName field3
--         , fieldName field4
--         , fieldName field5
--         , fieldName field6
--         , fieldName field7
--         , fieldName field8
--         , fieldName field9
--         ]
--         recordParser
-- {-| -}
-- record10 :
--     String
--     ->
--         (one
--          -> two
--          -> three
--          -> four
--          -> five
--          -> six
--          -> seven
--          -> eight
--          -> nine
--          -> ten
--          -> data
--         )
--     -> Field one
--     -> Field two
--     -> Field three
--     -> Field four
--     -> Field five
--     -> Field six
--     -> Field seven
--     -> Field eight
--     -> Field nine
--     -> Field ten
--     -> Block data
-- record10 recordName renderer field1 field2 field3 field4 field5 field6 field7 field8 field9 field10 =
--     let
--         recordParser =
--             Parser.succeed renderer
--                 |= fieldParser field1
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field2
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field3
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field4
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field5
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field6
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field7
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field8
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field9
--                 |. Parser.chompIf (\c -> c == '\n') Newline
--                 |= fieldParser field10
--     in
--     masterRecordParser recordName
--         [ fieldName field1
--         , fieldName field2
--         , fieldName field3
--         , fieldName field4
--         , fieldName field5
--         , fieldName field6
--         , fieldName field7
--         , fieldName field8
--         , fieldName field9
--         , fieldName field10
--         ]
--         recordParser


{-| -}
field : String -> Block value -> Field value
field name child =
    Advanced.field name child


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
    { view : Text -> rendered
    , inlines : List (Inline rendered)
    , replacements : List Replacement
    }
    -> Block (List rendered)
text options =
    Advanced.map
        allPresent
        (Advanced.text
            { view = \range txt -> Just (options.view txt)
            , inlines = List.map (Advanced.mapInline Just) options.inlines
            , replacements = options.replacements
            , error = always Nothing
            }
        )


{-| -}
type alias Inline result =
    Advanced.Inline result


{-| Create a custom inline element.

For example, here is how you could parse a link:

    Mark.inline "Link"
        (\txt url ->
            Html.a [ Html.Attriutes.href url ] (List.map renderText txt)
        )
        |> Mark.inlineText
        |> Mark.inlineString "url"

Here's an example of a sentence with the above link:

`Here is my sentence {Link| with a link to a web-comic. |url=http://www.poorlydrawnlines.com/comic/website-bird/}  You're welcome.`

-}
inline : String -> result -> Inline result
inline =
    Advanced.inline


{-| Parse an inline String field
-}
inlineString : String -> Inline (String -> result) -> Inline result
inlineString =
    Advanced.inlineString


{-| -}
inlineText : Inline (List Text -> result) -> Inline result
inlineText =
    Advanced.inlineText


{-| Replace a string with another string. This can be useful to have shortcuts to unicode characters.

For example, in `Mark.Default`, this is used to replace `...` with the unicode ellipses character: `…`.

-}
replacement : String -> String -> Replacement
replacement =
    Advanced.replacement


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
    Advanced.balanced



{- **Reclaimed typography**

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
{- ERROR FORMATTING -}


{-| -}
type alias Error =
    Advanced.Error


{-| -}
errorToString : Error -> String
errorToString =
    Advanced.errorToString
