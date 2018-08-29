module Mark.Custom exposing
    ( Inline, inline
    , Block, block, block1, block2, block3
    , Param(..), bool, int, float, string, oneOf
    , paragraph, section, indented
    , parser
    )

{-|

@docs Inline, inline


## Block functions with arguments

@docs Block, block, block1, block2, block3


## Parameters

@docs Param, bool, int, float, string, oneOf


## Styled

@docs paragraph, section, indented


## Advanced

@docs parser

-}

import Element exposing (Element)
import Internal.Model as Internal
import Parser exposing ((|.), (|=), Parser)


{-| -}
type alias Block style msg =
    Internal.Block style msg


{-| -}
type alias Inline style msg =
    Internal.Inline style msg


{-| Create a custom inline styler.

    Custom.inline "intro"
        (\string styling ->
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

    {drop| Lorem Ipsum is simply dummy text } of the printing and...

It will turn the first letter into a [dropped capital](https://en.wikipedia.org/wiki/Initial) and lead in with [small caps](https://practicaltypography.com/small-caps.html)

**styling** is the `styling` record that is passed in the options of `parseWith`. This means you can make an inline element that can be paragraph via the options.

-}
inline : String -> (String -> style -> List (Element msg)) -> Internal.Inline style msg
inline name renderer =
    Internal.Inline name
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
                    (\styling ->
                        Element.el
                            [ Font.color (Element.rgb 1 0 0) ]
                            (Element.text "Some statically defined, red text!")
                    )
                ]
            }

Which can then be used in your markup like so:

    | red |

The element you defined will show up there.

-}
block : String -> (style -> Element msg) -> Internal.Block style msg
block name renderer =
    Internal.Block name
        (Parser.succeed renderer
            |. Parser.chompWhile (\c -> c == ' ')
            |. Parser.token "|"
        )


{-| Same as `block`, but you can parse one parameter as well.

For example, here's how the builtin block, `image`, using `block2` and two `Custom.string` parameters.

    Custom.block2 "image"
        (\src description styling ->
            Element.image
                []
                { src = String.trim src
                , description = String.trim description
                }
        )
        Custom.string
        Custom.string

Which can then be used in your markup:

    | image http://placekitten/200/500
        Here's a great picture of my cat, pookie.

or as

    | image
        http://placekitten/200/500
        Here's a great picture of my cat, pookie.

-}
block1 :
    String
    -> (arg -> style -> Element msg)
    -> Param arg
    -> Internal.Block style msg
block1 name renderer (Param param) =
    Internal.Block name
        (Parser.succeed renderer
            |= param
        )


{-| -}
block2 :
    String
    -> (arg -> arg2 -> style -> Element msg)
    -> Param arg
    -> Param arg2
    -> Internal.Block style msg
block2 name renderer (Param param1) (Param param2) =
    Internal.Block name
        (Parser.succeed renderer
            |= param1
            |= param2
        )


{-| -}
block3 :
    String
    -> (arg -> arg2 -> arg3 -> style -> Element msg)
    -> Param arg
    -> Param arg2
    -> Param arg3
    -> Internal.Block style msg
block3 name renderer (Param param1) (Param param2) (Param param3) =
    Internal.Block name
        (Parser.succeed renderer
            |= param1
            |= param2
            |= param3
        )


{-| Either parse a double quote and wait for another double quote
Or parse the rest of the line
-}
string : Param String
string =
    Param
        (Parser.succeed identity
            |. Parser.chompWhile (\c -> c == ' ' || c == '\n')
            |= Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.token "\""
                    |= Parser.getChompedString
                        (Parser.chompWhile (\c -> c /= doubleQuote))
                    |. Parser.token "\""
                , Parser.getChompedString
                    (Parser.chompWhile (\c -> c /= '\n'))
                ]
        )


{-| Define a list of options. Useful for working with custom types.
-}
oneOf : List ( String, value ) -> Param value
oneOf opts =
    let
        parseOption ( name, val ) =
            Parser.keyword name
                |> Parser.map (always val)
    in
    Param
        (Parser.oneOf
            (List.map parseOption opts)
        )


{-| A parameter to use with `block1` or `block2`.
-}
type Param arg
    = Param (Parser arg)


{-| -}
int : Param Int
int =
    Param
        (Parser.succeed identity
            |. Parser.chompIf (\c -> c == ' ')
            |= Parser.int
        )


{-| -}
float : Param Float
float =
    Param
        (Parser.succeed identity
            |. Parser.chompIf (\c -> c == ' ')
            |= Parser.float
        )


{-| -}
bool : Param Bool
bool =
    Param
        (Parser.succeed identity
            |. Parser.chompIf (\c -> c == ' ')
            |= Parser.oneOf
                [ Parser.token "True"
                    |> Parser.map (always True)
                , Parser.token "False"
                    |> Parser.map (always False)
                ]
        )


{-| A block that expects a single paragraph of styled text as input. The `header` block that is built in uses this.

    | header
        My super sweet, /styled/ header.

**Note** paragraph is required to be on the next line and indented four spaces.

-}
paragraph :
    String
    ->
        (List (Element msg)
         ->
            { a
                | link : List (Element.Attribute msg)
                , token : List (Element.Attribute msg)
                , root : List (Element.Attribute msg)
                , block : List (Element.Attribute msg)
            }
         -> Element msg
        )
    ->
        Block
            { a
                | link : List (Element.Attribute msg)
                , token : List (Element.Attribute msg)
                , root : List (Element.Attribute msg)
                , block : List (Element.Attribute msg)
            }
            msg
paragraph name renderer =
    Internal.Parse name
        (\opts ->
            Parser.succeed renderer
                |. Parser.chompWhile (\c -> c == '\n')
                |. Parser.token "    "
                |= Internal.text opts
        )


{-| Like `Custom.paragraph`, but parses many styled paragraphs.

**Note** Parsing ends when there are two consecutive newlines.

-}
section :
    String
    ->
        (List (Element msg)
         ->
            { a
                | link : List (Element.Attribute msg)
                , token : List (Element.Attribute msg)
                , root : List (Element.Attribute msg)
                , block : List (Element.Attribute msg)
            }
         -> Element msg
        )
    ->
        Block
            { a
                | link : List (Element.Attribute msg)
                , token : List (Element.Attribute msg)
                , root : List (Element.Attribute msg)
                , block : List (Element.Attribute msg)
            }
            msg
section name renderer =
    Internal.Parse name
        (\opts ->
            Parser.succeed renderer
                |. Parser.chompWhile (\c -> c == '\n')
                |. Parser.token "    "
                |= Parser.loop []
                    (\els ->
                        Parser.oneOf
                            [ Parser.end
                                |> Parser.map
                                    (\_ ->
                                        Parser.Done
                                            (List.reverse els)
                                    )
                            , Parser.token "\n\n"
                                |> Parser.map
                                    (\_ ->
                                        Parser.Done
                                            (List.reverse els)
                                    )
                            , Parser.token "\n    "
                                |> Parser.map
                                    (\_ ->
                                        Parser.Loop els
                                    )
                            , Internal.text opts
                                |> Parser.map
                                    (\found ->
                                        if found == [] then
                                            Parser.Done
                                                (List.reverse els)

                                        else
                                            Parser.Loop
                                                (Element.paragraph []
                                                    found
                                                    :: els
                                                )
                                    )
                            ]
                    )
        )


{-| An 4-space-indented, unstyled block of text.

It ends after two consecutive newline characters.

-}
indented :
    String
    ->
        (String
         -> style
         -> Element msg
        )
    -> Block style msg
indented name renderer =
    Internal.Parse name
        (\opts ->
            Parser.succeed renderer
                |. Parser.chompWhile (\c -> c == '\n')
                |= Parser.loop ""
                    (\txt ->
                        Parser.oneOf
                            [ Parser.end
                                |> Parser.map
                                    (\_ ->
                                        Parser.Done txt
                                    )
                            , Parser.token "\n\n"
                                |> Parser.map
                                    (\_ ->
                                        Parser.Done txt
                                    )
                            , if txt == "" then
                                Parser.token "    "
                                    |> Parser.map
                                        (\_ ->
                                            Parser.Loop txt
                                        )

                              else
                                Parser.token "\n    "
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


{-| -}
parser : String -> (Internal.Options style msg -> Parser (style -> Element msg)) -> Block style msg
parser name actualParser =
    Internal.Parse name
        (\opts ->
            Parser.succeed identity
                |. Parser.chompWhile (\c -> c == '\n')
                |= actualParser opts
        )


doubleQuote =
    '"'
