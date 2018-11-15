module Mark.Custom exposing
    ( parse
    , Options, InlineOptions
    , Inline, inline
    , Block, block, block1, block2, block3
    , Param, bool, int, float, string, oneOf
    , paragraph, indented
    , parser
    , replacement, balanced
    , TextFormatting(..), Link, InlineStyle(..)
    , Context(..), Problem(..)
    )

{-|


# Options

@docs parse

@docs Options, InlineOptions

@docs Inline, inline


## Block functions with arguments

@docs Block, block, block1, block2, block3


## Parameters

`block1`-`block3` can take parameters specified in the markup. Here are the parameter types you can use:

@docs Param, bool, int, float, string, oneOf


## Styled

@docs paragraph, indented


## Advanced

@docs parser

@docs replacement, balanced

@docs TextFormatting, Link, InlineStyle

@docs Context, Problem

-}

import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


{-| -}
replacement : String -> String -> Replacement
replacement =
    Replacement


{-| -}
balanced :
    { end : ( String, String )
    , start : ( String, String )
    }
    -> Replacement
balanced =
    Balanced


{-| Create a custom inline styler.

    Custom.inline "intro"
        (\textFormatting model ->
            let
                txt =
                    String.trim string
            in
            if txt == "" then
                []

            else
                [ Element.el [ Element.alignLeft, Font.size 48 ]
                    (Element.text (String.toUpper (String.slice 0 1 txt)))
                , Element.el [ Font.size 24 ]
                    (Element.text (String.toUpper (String.dropLeft 1 txt)))
                ]
        )

When applied via `parseWith`, can then be used in markup like the following

    {intro| Lorem Ipsum is simply dummy text } of the printing and...

It will turn the first letter into a [dropped capital](https://en.wikipedia.org/wiki/Initial) and lead in with [small caps](https://practicaltypography.com/small-caps.html)

**styling** is the `styling` record that is passed in the options of `parseWith`. This means you can make an inline element that can be paragraph via the options.

-}
inline : String -> (TextFormatting -> Maybe Link -> result) -> Inline result
inline name renderer =
    Inline name
        renderer


{-| A simple block that will insert Elm html.

    import Element
    import Element.Font as Font
    import Mark.Custom as Custom

    myParser =
        Mark.parseWith
            { styling = Mark.defaultStyling
            , inlines = []
            , blocks =
                [ Custom.block "red"
                    (\styling model ->
                        Element.el
                            [ Font.color (Element.rgb 1 0 0) ]
                            (Element.text "Some statically defined, red text!")
                    )
                ]
            }

Which can then be used in your markup like so:

    | red

The element you defined will show up there.

-}
block : String -> result -> Block result
block name renderer =
    Block name
        (\indent_ inlines -> Parser.succeed renderer)


{-| Same as `block`, but you can parse one parameter as well.

For example, here's how the builtin block, `image`, using `block2` and two `Custom.string` parameters.

    Custom.block2 "image"
        (\src description styling model ->
            Element.image
                []
                { src = String.trim src
                , description = String.trim description
                }
        )
        Custom.string
        Custom.string

Which can then be used in your markup:

    | image "http://placekitten/200/500"
        "Here's a great picture of my cat, pookie.""

or as

    | image
        "http://placekitten/200/500"
        "Here's a great picture of my cat, pookie.""

-}
block1 :
    String
    -> (arg -> result)
    -> Param arg
    -> Block result
block1 name renderer (Param param) =
    Block name
        (\indent_ inlines ->
            Parser.succeed renderer
                |= param
        )


{-| -}
block2 :
    String
    -> (arg -> arg2 -> result)
    -> Param arg
    -> Param arg2
    -> Block result
block2 name renderer (Param param1) (Param param2) =
    Block name
        (\indent_ inlines ->
            Parser.succeed renderer
                |= param1
                |= param2
        )


{-| -}
block3 :
    String
    -> (arg -> arg2 -> arg3 -> result)
    -> Param arg
    -> Param arg2
    -> Param arg3
    -> Block result
block3 name renderer (Param param1) (Param param2) (Param param3) =
    Block name
        (\indent_ inlines ->
            Parser.succeed renderer
                |= param1
                |= param2
                |= param3
        )



-- styled : Param TextFormatting
-- styled


{-| Parse a double quoted string.
-}
string : Param String
string =
    Param
        (Parser.succeed identity
            |. Parser.chompWhile (\c -> c == ' ' || c == '\n')
            |. Parser.token (Parser.Token "\"" DoubleQuote)
            |= quotedString
        )


quotedString =
    Parser.loop
        ""
        (\found ->
            Parser.oneOf
                [ Parser.succeed
                    (\new ->
                        Parser.Loop (found ++ new)
                    )
                    |. Parser.token (Parser.Token "\\" Escape)
                    |= Parser.getChompedString
                        (Parser.chompIf (always True) EscapedChar)
                , Parser.map (\_ -> Parser.Done found) (Parser.token (Parser.Token "\"" DoubleQuote))
                , Parser.getChompedString
                    (Parser.chompWhile
                        (\c ->
                            c
                                /= doubleQuote
                                && c
                                /= '\\'
                        )
                    )
                    |> Parser.map
                        (\new ->
                            Parser.Loop (found ++ new)
                        )
                ]
        )


{-| Define a list of options. Useful for working with custom types.
-}
oneOf : List ( String, value ) -> Param value
oneOf opts =
    let
        parseOption ( name, val ) =
            Parser.keyword (Parser.Token name (Expecting name))
                |> Parser.map (always val)
    in
    Param
        (Parser.oneOf
            (List.map parseOption opts)
        )


{-| A parameter to use with `block1` or `block2`.
-}
type Param arg
    = Param (Parser Context Problem arg)


{-| -}
int : Param Int
int =
    Param
        (Parser.succeed identity
            |. Parser.chompIf (\c -> c == ' ') Space
            |= Parser.int Integer InvalidNumber
        )


{-| -}
float : Param Float
float =
    Param
        (Parser.succeed identity
            |. Parser.chompIf (\c -> c == ' ') Space
            |= Parser.float FloatingPoint InvalidNumber
        )


{-| -}
bool : Param Bool
bool =
    Param
        (Parser.succeed identity
            |. Parser.chompIf (\c -> c == ' ') Space
            |= Parser.oneOf
                [ Parser.token (Parser.Token "True" (Expecting "True"))
                    |> Parser.map (always True)
                , Parser.token (Parser.Token "False" (Expecting "False"))
                    |> Parser.map (always False)
                ]
        )


{-| A block that expects a single paragraph of styled text as input. The `header` block that is built in uses this.

    | header
        My super sweet, /styled/ header.

**Note** The actual paragraph text is required to be on the next line and indented four spaces.

-}
paragraph :
    String
    ->
        (List result
         -> result
        )
    -> Block result
paragraph name renderer =
    Block name
        (\indent_ inlineParser ->
            Parser.succeed
                (\almostElements ->
                    renderer almostElements
                )
                |= inlineParser
        )



-- {-| Like `Custom.paragraph`, but parses many styled paragraphs.
-- **Note** Parsing ends when there are three consecutive newlines.
-- -}
-- section :
--     String
--     ->
--         (List (Element msg)
--          -> model
--          -> Element msg
--         )
--     -> Block result
-- section name renderer =
--     Block name
--         (\inlineParser ->
--             Parser.succeed
--                 (\almostElements model ->
--                     renderer (almostElements model) model
--                 )
--                 |. Parser.token (Parser.Token "\n" Newline)
--                 |. Parser.token (Parser.Token "    " ExpectedIndent)
--                 |= Parser.loop []
--                     (\els ->
--                         Parser.oneOf
--                             [ Parser.end End
--                                 |> Parser.map
--                                     (\_ ->
--                                         Parser.Done
--                                             (\model ->
--                                                 List.foldl
--                                                     (\fn elems ->
--                                                         fn model :: elems
--                                                     )
--                                                     []
--                                                     els
--                                             )
--                                     )
--                             , Parser.token (Parser.Token "\n\n" (Expecting "\n\n"))
--                                 |> Parser.map
--                                     (\_ ->
--                                         Parser.Done
--                                             (\model ->
--                                                 List.foldl
--                                                     (\fn elems ->
--                                                         fn model :: elems
--                                                     )
--                                                     []
--                                                     els
--                                             )
--                                     )
--                             , Parser.token (Parser.Token "\n    " ExpectedIndent)
--                                 |> Parser.map
--                                     (\_ ->
--                                         Parser.Loop els
--                                     )
--                             , inlineParser
--                                 |> Parser.map
--                                     (\found ->
--                                         Parser.Loop
--                                             ((\model ->
--                                                 Element.paragraph []
--                                                     (List.map (\viewInline -> viewInline model) found)
--                                              )
--                                                 :: els
--                                             )
--                                     )
--                             ]
--                     )
--         )


{-| Parse a 4-space-indented, unstyled block of text.

Ends once the block is no longer indented.

-}
indented : String -> (String -> result) -> Block result
indented name renderer =
    Block name
        (\indent_ inlineParser ->
            Parser.succeed renderer
                |= Parser.loop ""
                    (\txt ->
                        Parser.oneOf
                            [ Parser.end End
                                |> Parser.map
                                    (\_ ->
                                        Parser.Done txt
                                    )
                            , Parser.succeed (Parser.Loop txt)
                                |. Parser.token (Parser.Token "\n" Newline)
                            , if txt == "" then
                                Parser.token (Parser.Token "    " ExpectedIndent)
                                    |> Parser.map
                                        (\_ ->
                                            Parser.Loop txt
                                        )

                              else
                                Parser.token (Parser.Token "\n    " ExpectedIndent)
                                    |> Parser.map
                                        (\_ ->
                                            Parser.Loop (txt ++ "\n")
                                        )
                            , Parser.getChompedString
                                (Parser.chompWhile (\c -> c /= '\n'))
                                |> Parser.map
                                    (\found ->
                                        if found == "" then
                                            Parser.Done txt

                                        else
                                            Parser.Loop
                                                (txt ++ found)
                                    )
                            ]
                    )
        )


{-| Run a parser created with [elm/parser](https://package.elm-lang.org/packages/elm/parser/latest/) where you can parse whatever you'd like. This is actually how the `list` block is implemented.

I highly recommend using the other `Custom` parsers if possible as this is the only one that can "break" an elm-markup file.

You're in charge of defining when this parser will stop parsing.

Current conventions:

  - stop parsing after three consecutive newlines
  - blocks should be indented 4 spaces

These conventions may be enforced in the future.

Here's a terrible example.

    Custom.parser "bananaTree"
        (\inlines ->
            -- `inlines` is a parser for styled text which you may or maynot need,
            Parser.succeed identity
                |. Parser.token  " "
                |= (Parser.map
                        -- the result of our parser should be a function that
                        -- takes a `styling` and a `model` and returns `Element msg`
                        (\_ styling model ->
                                Element.el [] (Element.text "bananas"
                        )
                        (Parser.token "bananas")
                    )
                |. Parser.token "\n"
        )

It parses bananas and then says `bananas`. If you wanted to do exactly this in real life, you would probably use some form of `Custom.block`.

-}
parser :
    String
    -> (Int -> Parser Context Problem (List result) -> Parser Context Problem result)
    -> Block result
parser name actualParser =
    Block name
        (\baseIndent inlines ->
            Parser.succeed identity
                |= actualParser baseIndent inlines
        )



{- Internal Rendering Logic




-}


{-| -}
type alias Options result =
    { blocks : List (Block result)
    , merge : List result -> result
    , inlines : InlineOptions result
    }


type alias InlineOptions result =
    { view : TextFormatting -> Maybe Link -> result
    , inlines : List (Inline result)
    , merge : List result -> result
    , replacements : List Replacement
    }


type Replacement
    = Replacement String String
    | Balanced
        { start : ( String, String )
        , end : ( String, String )
        }


{-| -}
type Block result
    = Block String (Int -> Parser Context Problem (List result) -> Parser Context Problem result)


{-| -}
type Inline result
    = Inline String (TextFormatting -> Maybe Link -> result)
    | Icon String result


type alias Link =
    { url : String }


type TextFormatting
    = NoFormatting String
    | Styles (List InlineStyle) String
    | Fragments (List Fragment)


type alias Fragment =
    { text : String
    , styles : List InlineStyle
    }


type InlineStyle
    = NoStyleChange
    | Bold
    | Italic
    | Strike
    | Underline
    | Token


type Context
    = InBlock String
    | InInline String


type Problem
    = NoBlocks
    | ExpectedIndent
    | InlineStart
    | InlineBar
    | InlineEnd
    | Expecting String
    | ExpectingBlockName String
    | Escape
    | EscapedChar
    | Dash
    | DoubleQuote
    | Apostrophe
    | Newline
    | Space
    | End
    | Integer
    | FloatingPoint
    | InvalidNumber
    | ExpectingAlphaNumeric



{- Formatted Text -}


{-| -}
type Text rendered
    = Text
        -- Accumulator string
        { text : TextFormatting

        -- Accumulator of element constructors
        , rendered : List rendered
        , balancedReplacements : List String
        }


{-| -}
emptyText : Text rendered
emptyText =
    Text { text = NoFormatting "", rendered = [], balancedReplacements = [] }


{-| -}
parse : Options result -> String -> Result (List (Parser.DeadEnd Context Problem)) result
parse opts source =
    Parser.run (markup opts) source


{-| -}
markup : Options result -> Parser Context Problem result
markup options =
    Parser.map (options.merge << List.reverse) <|
        Parser.loop []
            (blocks options)


{-| -}
blocks : Options result -> List result -> Parser Context Problem (Parser.Step (List result) (List result))
blocks options existing =
    Parser.oneOf
        [ Parser.end End
            |> Parser.map
                (\_ ->
                    Parser.Done existing
                )
        , Parser.token (Parser.Token "\n" Newline)
            |> Parser.map
                (\_ ->
                    Parser.Loop
                        existing
                )

        -- custom blocks
        , Parser.succeed
            (\blockParser ->
                blockParser 0 (Parser.loop emptyText (inlineLoop options.inlines))
            )
            |. Parser.token (Parser.Token "|=" (Expecting "|="))
            |. Parser.chompWhile (\c -> c == ' ')
            |= Parser.oneOf
                (case options.blocks of
                    [] ->
                        [ Parser.problem NoBlocks ]

                    _ ->
                        List.filterMap
                            blockToParser
                            options.blocks
                )
            |. Parser.chompWhile (\c -> c == ' ')
            |. Parser.chompIf (\c -> c == '\n') Newline
            |> Parser.andThen
                (\blockParser ->
                    Parser.map
                        (\found ->
                            Parser.Loop
                                (found :: existing)
                        )
                        blockParser
                )

        -- custom inlines
        , Parser.loop emptyText (inlineLoop options.inlines)
            |> Parser.map
                (\found ->
                    Parser.Loop
                        (options.inlines.merge found :: existing)
                )
        ]


{-| -}
indent : Parser Context Problem Int
indent =
    Parser.oneOf
        [ --     Parser.token (Parser.Token (String.repeat 12 " ") ExpectedIndent)
          --     |> Parser.map (always 3)
          -- , Parser.token (Parser.Token (String.repeat 8 " ") ExpectedIndent)
          --     |> Parser.map (always 2)
          Parser.token (Parser.Token (String.repeat 4 " ") ExpectedIndent)
            |> Parser.map (always 1)
        ]



{- Custom Blocks -}


inlineToParser inlineElement =
    case inlineElement of
        Inline name fn ->
            Parser.keyword (Parser.Token name (Expecting name))
                |> Parser.map
                    (\_ -> inlineElement)

        Icon name result ->
            Parser.keyword (Parser.Token name (Expecting name))
                |> Parser.map
                    (\_ -> inlineElement)


capitalize str =
    case str of
        "" ->
            Nothing

        _ ->
            let
                fst =
                    String.left 1 str

                remaining =
                    String.dropLeft 1 str
            in
            Just (String.toUpper fst ++ remaining)


blockToParser :
    Block result
    -> Maybe (Parser Context Problem (Int -> Parser Context Problem (List result) -> Parser Context Problem result))
blockToParser blockable =
    case blockable of
        Block nonstandardizedName blockParser ->
            let
                maybeName =
                    nonstandardizedName
                        |> String.trim
                        |> capitalize
            in
            case maybeName of
                Nothing ->
                    Nothing

                Just name ->
                    Parser.keyword (Parser.Token name (ExpectingBlockName name))
                        |> Parser.map
                            (\_ ->
                                \i inlineParser -> Parser.inContext (InBlock name) (blockParser i inlineParser)
                            )
                        |> Just


reduceFragments txt =
    case txt of
        Fragments frags ->
            let
                finalized =
                    frags
                        |> List.filter (\f -> f.text /= "")
                        |> List.reverse
            in
            case finalized of
                [] ->
                    txt

                [ frag ] ->
                    Styles frag.styles frag.text

                remaining ->
                    Fragments remaining

        _ ->
            txt


inlineLoop :
    InlineOptions result
    -> Text result
    -> Parser Context Problem (Parser.Step (Text result) (List result))
inlineLoop inlineOptions existing =
    let
        txt =
            case existing of
                Text { text } ->
                    text
    in
    Parser.oneOf
        [ -- Do any character replacements that are necessary
          Parser.oneOf (replace inlineOptions.replacements existing)
            |> Parser.map Parser.Loop

        -- If a char matches the first character of a replacement,
        -- but didn't match the full replacement captured above,
        -- then stash that char.
        , Parser.oneOf (almostReplacement inlineOptions.replacements existing)
            |> Parser.map Parser.Loop

        -- Custom inline block
        , Parser.succeed
            identity
            |. Parser.token
                (Parser.Token "<" InlineStart)
            |= Parser.oneOf
                (List.map inlineToParser inlineOptions.inlines)
            |> Parser.andThen
                (\inlineType ->
                    case inlineType of
                        Inline name customInline ->
                            Parser.succeed
                                (\(Text styled) ->
                                    case changeStyle inlineOptions existing NoStyleChange of
                                        Text current ->
                                            Parser.Loop <|
                                                Text
                                                    { rendered =
                                                        customInline
                                                            (reduceFragments styled.text)
                                                            Nothing
                                                            :: styled.rendered
                                                            ++ current.rendered
                                                    , text =
                                                        case styled.text of
                                                            NoFormatting _ ->
                                                                NoFormatting ""

                                                            Styles styles _ ->
                                                                Styles styles ""

                                                            Fragments frags ->
                                                                case frags of
                                                                    [] ->
                                                                        NoFormatting ""

                                                                    f :: _ ->
                                                                        Styles f.styles ""
                                                    , balancedReplacements = styled.balancedReplacements
                                                    }
                                )
                                |. Parser.token (Parser.Token "|" InlineBar)
                                |= styledText inlineOptions txt [ '>' ]
                                |. Parser.token (Parser.Token ">" InlineEnd)

                        Icon name result ->
                            Parser.succeed
                                (case changeStyle inlineOptions existing NoStyleChange of
                                    Text current ->
                                        Parser.Loop <|
                                            Text
                                                { rendered =
                                                    result :: current.rendered
                                                , text =
                                                    case current.text of
                                                        NoFormatting _ ->
                                                            NoFormatting ""

                                                        Styles styles _ ->
                                                            Styles styles ""

                                                        Fragments frags ->
                                                            case frags of
                                                                [] ->
                                                                    NoFormatting ""

                                                                f :: _ ->
                                                                    Styles f.styles ""
                                                , balancedReplacements = current.balancedReplacements
                                                }
                                )
                                |. Parser.token (Parser.Token ">" InlineEnd)
                )

        -- Link
        , Parser.succeed
            (\(Text linkText) url ->
                case changeStyle inlineOptions existing NoStyleChange of
                    Text current ->
                        Parser.Loop <|
                            Text
                                { rendered =
                                    inlineOptions.view
                                        (reduceFragments linkText.text)
                                        (Just { url = url })
                                        :: linkText.rendered
                                        ++ current.rendered
                                , text =
                                    case linkText.text of
                                        NoFormatting _ ->
                                            NoFormatting ""

                                        Styles styles _ ->
                                            Styles styles ""

                                        Fragments frags ->
                                            case frags of
                                                [] ->
                                                    NoFormatting ""

                                                f :: _ ->
                                                    Styles f.styles ""
                                , balancedReplacements = linkText.balancedReplacements
                                }
            )
            |. Parser.token (Parser.Token "[" (Expecting "["))
            |= styledText inlineOptions txt [ ']' ]
            |. Parser.token (Parser.Token "]" (Expecting "]"))
            |. Parser.token (Parser.Token "(" (Expecting "("))
            |= Parser.getChompedString
                (Parser.chompWhile (\c -> c /= ')' && c /= '\n' && c /= ' '))
            |. Parser.token (Parser.Token ")" (Expecting ")"))

        -- Capture styling
        , Parser.succeed
            (Parser.Loop << changeStyle inlineOptions existing)
            |= Parser.oneOf
                [ Parser.map (always Italic) (Parser.token (Parser.Token "/" (Expecting "/")))
                , Parser.map (always Strike) (Parser.token (Parser.Token "~" (Expecting "~")))
                , Parser.map (always Bold) (Parser.token (Parser.Token "*" (Expecting "*")))
                , Parser.map (always Token) (Parser.token (Parser.Token "`" (Expecting "`")))
                ]

        -- end on newline
        , Parser.token (Parser.Token "\n" Newline)
            |> Parser.map
                (\_ ->
                    finalize inlineOptions existing
                )

        -- chomp until a meaningful character
        , Parser.getChompedString
            (Parser.chompWhile
                (\c ->
                    let
                        meaningful =
                            stylingChars ++ replacementStartingChars inlineOptions.replacements
                    in
                    not (List.member c meaningful)
                )
            )
            |> Parser.map
                (\new ->
                    if new == "" then
                        finalize inlineOptions existing

                    else
                        Parser.Loop (addText new existing)
                )
        ]


finalize inlineOptions (Text cursor) =
    let
        txt =
            case cursor.text of
                NoFormatting x ->
                    x

                Styles _ x ->
                    x

                Fragments frags ->
                    frags
                        |> List.map .text
                        |> String.join ""
    in
    Parser.Done <|
        if txt == "" then
            List.reverse cursor.rendered

        else
            List.reverse (inlineOptions.view cursor.text Nothing :: cursor.rendered)


{-| This parser is used to parse styled text, but the delay rendering it.

That may seem sorta weird, but we need this in cases like for links. We want styled

-}
styledText : InlineOptions result -> TextFormatting -> List Char -> Parser Context Problem (Text result)
styledText options txt until =
    let
        vacantText =
            case txt of
                NoFormatting x ->
                    NoFormatting ""

                Styles styles _ ->
                    Styles styles ""

                -- TODO: is this an issue?
                x ->
                    x

        untilStrings =
            List.map String.fromChar until
    in
    Parser.loop (Text { text = vacantText, rendered = [], balancedReplacements = [] })
        (\found ->
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
                    (Parser.Loop << cacheStyle options found)
                    |= Parser.oneOf
                        [ Parser.map (always Italic) (Parser.token (Parser.Token "/" (Expecting "/")))
                        , Parser.map (always Strike) (Parser.token (Parser.Token "~" (Expecting "~")))
                        , Parser.map (always Bold) (Parser.token (Parser.Token "*" (Expecting "*")))
                        , Parser.map (always Token) (Parser.token (Parser.Token "`" (Expecting "`")))
                        ]

                -- chomp until a meaningful character
                , Parser.getChompedString
                    (Parser.chompWhile
                        (\c ->
                            let
                                meaningful =
                                    '\n' :: until ++ stylingChars ++ replacementStartingChars options.replacements
                            in
                            not (List.member c meaningful)
                        )
                    )
                    |> Parser.map
                        (\new ->
                            if new == "" || new == "\n" then
                                Parser.Done found

                            else if List.member (String.right 1 new) untilStrings then
                                Parser.Done (addText (String.dropRight 1 new) found)

                            else
                                Parser.Loop (addText new found)
                        )
                ]
        )


cacheStyle inlineOptions (Text cursor) styleToken =
    let
        newText =
            case styleToken of
                NoStyleChange ->
                    cursor.text

                Bold ->
                    cacheNewStyle Bold cursor.text

                Italic ->
                    cacheNewStyle Italic cursor.text

                Strike ->
                    cacheNewStyle Strike cursor.text

                Underline ->
                    cacheNewStyle Underline cursor.text

                Token ->
                    cacheNewStyle Token cursor.text
    in
    Text
        { rendered = cursor.rendered
        , text = newText
        , balancedReplacements = cursor.balancedReplacements
        }


cacheNewStyle newStyle text =
    case text of
        NoFormatting str ->
            Fragments [ { text = "", styles = [ newStyle ] }, { text = str, styles = [] } ]

        Styles styles str ->
            if List.member newStyle styles then
                Fragments [ { text = "", styles = List.filter ((/=) newStyle) styles }, { text = str, styles = styles } ]

            else
                Fragments [ { text = "", styles = [ newStyle ] }, { text = str, styles = styles } ]

        Fragments frags ->
            case frags of
                [] ->
                    Fragments [ { text = "", styles = [ newStyle ] } ]

                current :: remain ->
                    if List.member newStyle current.styles then
                        Fragments <| { text = "", styles = List.filter ((/=) newStyle) current.styles } :: current :: remain

                    else
                        Fragments <| { text = "", styles = newStyle :: current.styles } :: current :: remain



-- Fragments ({ text = "", styles = [ newStyle ] } :: frags)


{-| -}
almostReplacement : List Replacement -> Text rendered -> List (Parser Context Problem (Text rendered))
almostReplacement replacements existing =
    let
        captureChar char =
            Parser.succeed
                (\c ->
                    addText c existing
                )
                |= Parser.getChompedString
                    (Parser.chompIf (\c -> c == char && char /= '<') EscapedChar)

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
replace : List Replacement -> Text rendered -> List (Parser Context Problem (Text rendered))
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
                        (addText y existing)
                        |. Parser.token (Parser.Token x (Expecting x))
                        |. Parser.loop ()
                            (\_ ->
                                Parser.oneOf
                                    [ Parser.token (Parser.Token x (Expecting x))
                                        |> Parser.map (always (Parser.Loop ()))
                                    , Parser.succeed (Parser.Done ())
                                    ]
                            )

                Balanced range ->
                    let
                        balanceCache =
                            case existing of
                                Text cursor ->
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
    , '/'
    , '*'
    , '['
    , '\n'
    , '<'
    , '`'
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


addBalance id (Text cursor) =
    Text <|
        { cursor | balancedReplacements = id :: cursor.balancedReplacements }


removeBalance id (Text cursor) =
    Text <|
        { cursor | balancedReplacements = List.filter ((/=) id) cursor.balancedReplacements }


addText newTxt (Text cursor) =
    case cursor.text of
        NoFormatting txt ->
            Text { cursor | text = NoFormatting (txt ++ newTxt) }

        Styles styles txt ->
            Text { cursor | text = Styles styles (txt ++ newTxt) }

        Fragments frags ->
            case frags of
                [] ->
                    Text { cursor | text = Fragments [ { text = newTxt, styles = [] } ] }

                recent :: remain ->
                    Text { cursor | text = Fragments ({ text = recent.text ++ newTxt, styles = recent.styles } :: remain) }


{-| If we accumulating a link style, accumulate it there.
-}
addElement newElements (Text cursor) =
    Text { cursor | rendered = newElements :: cursor.rendered }


changeStyle inlineOptions (Text cursor) styleToken =
    let
        textIsEmpty =
            case cursor.text of
                NoFormatting "" ->
                    True

                Styles _ "" ->
                    True

                _ ->
                    False

        newText =
            case styleToken of
                NoStyleChange ->
                    cursor.text

                Bold ->
                    flipStyle Bold cursor.text

                Italic ->
                    flipStyle Italic cursor.text

                Strike ->
                    flipStyle Strike cursor.text

                Underline ->
                    flipStyle Underline cursor.text

                Token ->
                    flipStyle Token cursor.text
    in
    if textIsEmpty then
        Text { rendered = cursor.rendered, text = newText, balancedReplacements = cursor.balancedReplacements }

    else
        Text
            { rendered = inlineOptions.view cursor.text Nothing :: cursor.rendered
            , text = newText
            , balancedReplacements = cursor.balancedReplacements
            }


flipStyle newStyle text =
    case text of
        NoFormatting str ->
            Styles [ newStyle ] ""

        Styles styles str ->
            if List.member newStyle styles then
                Styles (List.filter ((/=) newStyle) styles) ""

            else
                Styles (newStyle :: styles) ""

        Fragments frags ->
            Fragments frags


doubleQuote =
    '"'
