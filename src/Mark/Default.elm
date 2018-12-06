module Mark.Default exposing
    ( document
    , title, header, list, monospace, image
    , replacements, text
    )

{-| This is a document that renders to [`elm-ui`'s](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/) `Element msg`.

It's a good starting point for writing something like a blog article, but one of the great powers of this library is in [writing custom blocks](https://package.elm-lang.org/packages/mdgriffith/elm-markup/latest/Mark-Custom) to suite your specific domain or style needs!

Check out the source to this module to get an idea of how things fit together.

@docs document

@docs title, header, list, monospace, image

@docs replacements, text

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region
import Html.Attributes
import Mark
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


{-| Parsing this document results in a `view` function, `model -> Element msg`.
-}
document : Mark.Document (model -> Element msg)
document =
    Mark.document
        (\children model ->
            Element.textColumn []
                (List.map (\view -> view model) children)
        )
        (Mark.manyOf
            [ title [ Font.size 48 ] defaultText
            , header [ Font.size 36 ] defaultText
            , list
                { style = listStyles
                , icon = renderIcon
                }
                defaultText
            , image
            , monospace
                [ Element.spacing 5
                , Element.padding 24
                , Background.color
                    (Element.rgba 0 0 0 0.04)
                , Border.rounded 2
                , Font.size 16
                , Font.family
                    [ Font.external
                        { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                        , name = "Source Code Pro"
                        }
                    , Font.sansSerif
                    ]
                ]

            -- Toplevel Text
            , Mark.map (\viewEls model -> Element.paragraph [] (viewEls model)) defaultText
            ]
        )


{-| Create an image.

    | Image
        src = https://placekitten.com/200/300
        description = A cute kitty cat.

-}
image : Mark.Block (model -> Element msg)
image =
    Mark.record2 "Image"
        (\src description model ->
            Element.image []
                { src = src
                , description = description
                }
        )
        (Mark.field "src" Mark.string)
        (Mark.field "description" Mark.string)


defaultText =
    text
        { code =
            [ Background.color
                (Element.rgba 0 0 0 0.04)
            , Border.rounded 2
            , Element.paddingXY 5 3
            , Font.size 16
            , Font.family
                [ Font.external
                    { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                    , name = "Source Code Pro"
                    }
                , Font.sansSerif
                ]
            ]
        , link =
            [ Font.color
                (Element.rgb
                    (17 / 255)
                    (132 / 255)
                    (206 / 255)
                )
            , Element.mouseOver
                [ Font.color
                    (Element.rgb
                        (234 / 255)
                        (21 / 255)
                        (122 / 255)
                    )
                ]
            ]
        }


{-| The title of your document. Renders as an `h1`.

    | Title
        The title of my document.

-}
title : List (Element.Attribute msg) -> Mark.Block (model -> List (Element msg)) -> Mark.Block (model -> Element msg)
title attrs titleText =
    Mark.block "Title"
        (\elements model ->
            Element.paragraph
                (Element.Region.heading 1 :: attrs)
                (elements model)
        )
        titleText


{-| A header.

    | Header
        My header

Renders as an `h2`.

-}
header : List (Element.Attribute msg) -> Mark.Block (model -> List (Element msg)) -> Mark.Block (model -> Element msg)
header attrs textParser =
    Mark.block "Header"
        (\elements model ->
            Element.paragraph
                (Element.Region.heading 2 :: attrs)
                (elements model)
        )
        textParser


{-| A monospaced code block without syntax highlighting.
-}
monospace : List (Element.Attribute msg) -> Mark.Block (model -> Element msg)
monospace attrs =
    Mark.block "Monospace"
        (\string model ->
            Element.paragraph
                (Element.htmlAttribute (Html.Attributes.style "line-height" "1.4em")
                    :: Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
                    :: attrs
                )
                [ Element.text string ]
        )
        Mark.multiline


{-| -}
text :
    { code : List (Element.Attribute msg)
    , link : List (Element.Attribute msg)
    }
    -> Mark.Block (model -> List (Element msg))
text style =
    Mark.map
        (\els model ->
            List.map (\view -> view model) els
        )
        (Mark.text
            { view = textFragment style
            , inlines =
                [ link style
                ]
            , replacements = replacements
            }
        )


link config =
    Mark.inline "Link"
        (\txt url model ->
            Element.link config.link
                { url = url
                , label =
                    Element.row []
                        (List.map (\item -> textFragment config item model) txt)
                }
        )
        |> Mark.inlineText
        |> Mark.inlineString "url"


textFragment config node model_ =
    case node of
        Mark.Text styles txt ->
            Element.el (List.concatMap (toStyles config) styles) (Element.text txt)


toStyles config style =
    case style of
        Mark.Bold ->
            [ Font.bold ]

        Mark.Italic ->
            [ Font.italic ]

        Mark.Strike ->
            [ Font.strike ]

        -- Mark.Underline ->
        --     [ Font.underline ]
        Mark.Code ->
            config.code


{-| This will replace certain characters with improved typographical ones.

  - `...` is converted to the ellipses unicode character.
  - `"` Straight double quotes are [replaced with curly quotes](https://practicaltypography.com/straight-and-curly-quotes.html)
  - `'` Single Quotes are replaced with apostrophes. In the future we might differentiate between curly single quotes and apostrophes.
  - `--` is replaced with an en-dash.
  - `---` is replaced with an em-dash.
  - `<>` - will create a non-breaking space (`&nbsp;`). This is not for manually increasing space (sequential `<>` tokens will only render as one `&nbsp;`), but to signify that the space between two words shouldn't break when wrapping. Think of this like glueing two words together.
  - `//` will change to `/`. Normally `/` starts italic formatting. To escape this, we'd normally do `\/`, though that looks pretty funky. `//` just feels better!

These transformations also don't apply inside inline `code` or inside the `monospace` block.

**Note** Escaping the start of any of these characters will skip the replacement.

**Note** If you're not familiar with `en-dash` or `em-dash`, I definitely [recommend reading a small bit about it](https://practicaltypography.com/hyphens-and-dashes.html)—they're incredibly useful.

-}
replacements : List Mark.Replacement
replacements =
    [ Mark.replacement "..." "…"
    , Mark.replacement "<>" "\u{00A0}"
    , Mark.replacement "---" "—"
    , Mark.replacement "--" "–"
    , Mark.replacement "//" "/"
    , Mark.replacement "'" "’"
    , Mark.balanced
        { start = ( "\"", "“" )
        , end = ( "\"", "”" )
        }
    ]



{-


   Nested List Handling


-}


{-| A nested list with an expected indentation of 4 spaces per level. As far as icons:

  - `-` indicates a bullet. You can have any number of dashes.
  - `->` or `-->` will render as an ➙
  - Any alphabetic character after the dash is considered part of a formatting string for numbered items.
      - `-x.` will be auto-numbered as and renderd `1.`, or `2.` or whatever number you're at.
      - Any punctuation that comes after the alpha character will be maintained. So, `x)` will format as `1)` and `x.` will format as `1.`
      - Nested numbers can be rendered if they're in the right place. So `-x.y)` would render as `1.2)`
      - A literal number instead of an alpha will reset the auto numbering to that literal number. So, `-9.` means start counting up from 9 from here on out.
      - This can also applied in a nested manner as `-x.9`, which will reset the inner number.

-}
list :
    { icon : List Int -> ListIcon -> Element msg
    , style : List Int -> List (Element.Attribute msg)
    }
    -> Mark.Block (model -> List (Element msg))
    -> Mark.Block (model -> Element msg)
list config textParser =
    Mark.block "List"
        (\items model ->
            Element.column
                (config.style [])
                (List.indexedMap (renderListItem config model []) items)
        )
        (Mark.nested
            { item = textParser
            , start =
                Mark.advanced
                    listIconParser
            }
        )


{-| -}
listIconParser : Parser Mark.Context Mark.Problem ListIcon
listIconParser =
    Parser.oneOf
        [ Parser.succeed Arrow
            |. Parser.oneOf
                [ Parser.token (Parser.Token "->" (Mark.Expecting "->"))
                , Parser.token (Parser.Token "-->" (Mark.Expecting "-->"))
                ]
            |. Parser.chompIf (\c -> c == ' ') Mark.Space
        , Parser.succeed identity
            |. Parser.token (Parser.Token "-" (Mark.Expecting "-"))
            |= Parser.oneOf
                [ Parser.succeed Bullet
                    |. Parser.oneOf
                        [ Parser.token (Parser.Token " " Mark.Space)
                        , Parser.token (Parser.Token "-" Mark.Dash)
                        ]
                    |. Parser.chompIf (\c -> c == ' ') Mark.Space
                , Parser.succeed
                    (\( reset, decorations ) ->
                        Number { reset = reset, decorations = decorations }
                    )
                    |= Parser.loop ( [], [] ) numberIconParser
                    |. Parser.chompIf (\c -> c == ' ') Mark.Space
                ]
        ]


{-| -}
numberIconParser :
    ( List (Maybe Int), List String )
    -> Parser Mark.Context Mark.Problem (Parser.Step ( List (Maybe Int), List String ) ( List (Maybe Int), List String ))
numberIconParser ( cursorReset, decorations ) =
    Parser.oneOf
        [ Parser.succeed
            (\reset decoration ->
                Parser.Loop
                    ( reset :: cursorReset
                    , decoration :: decorations
                    )
            )
            |= Parser.oneOf
                [ Parser.succeed
                    (\lead remaining ->
                        case ( String.toInt lead, String.toInt remaining ) of
                            ( Just l, Just r ) ->
                                Just <| (l * 10 * String.length remaining) + r

                            ( Just l, Nothing ) ->
                                Just l

                            _ ->
                                Nothing
                    )
                    |= Parser.getChompedString (Parser.chompIf Char.isDigit Mark.Integer)
                    |= Parser.getChompedString (Parser.chompWhile Char.isDigit)
                , Parser.succeed Nothing
                    |. Parser.chompIf Char.isAlpha Mark.ExpectingAlphaNumeric
                    |. Parser.chompWhile Char.isAlpha
                ]
            |= Parser.getChompedString
                (Parser.chompWhile
                    (\c ->
                        c
                            /= ' '
                            && not (Char.isAlpha c)
                            && not (Char.isDigit c)
                    )
                )
        , Parser.succeed
            (Parser.Done
                ( List.reverse cursorReset
                , List.reverse decorations
                )
            )
        ]



{- LIST -}


{-| -}
listStyles : List Int -> List (Element.Attribute msg)
listStyles cursor =
    case List.length cursor of
        0 ->
            -- top level element
            [ Element.spacing 16 ]

        1 ->
            [ Element.spacing 16 ]

        2 ->
            [ Element.spacing 16 ]

        _ ->
            [ Element.spacing 8 ]


edges =
    { top = 0
    , left = 0
    , right = 0
    , bottom = 0
    }


renderListItem config model stack index (Mark.Nested item) =
    case item.content of
        ( icon, items ) ->
            Element.row []
                [ Element.el [ Element.alignTop ]
                    (renderIcon (index :: stack) icon)
                , Element.textColumn
                    (config.style [])
                    (List.map
                        (\view ->
                            Element.paragraph
                                []
                                (view model)
                        )
                        items
                        ++ List.indexedMap (renderListItem config model (index :: stack)) item.children
                    )
                ]


type alias Index =
    { decoration : String
    , index : Int
    , show : Bool
    }


{-| -}
type ListIcon
    = Bullet
    | Arrow
    | Number
        { reset : List (Maybe Int)
        , decorations : List String
        }


{-| -}
renderIcon : List Int -> ListIcon -> Element msg
renderIcon index symbol =
    let
        pad =
            Element.paddingEach
                { edges
                    | left = 28
                    , right = 12
                }
    in
    case symbol of
        Arrow ->
            Element.el
                [ pad ]
                (Element.text "➙")

        Bullet ->
            let
                icon =
                    case List.length index of
                        1 ->
                            "•"

                        _ ->
                            "◦"
            in
            Element.el [ pad ] (Element.text icon)

        Number numberConfig ->
            Element.el [ pad ]
                (Element.text
                    (index
                        |> List.foldl applyDecoration ( List.reverse numberConfig.decorations, [] )
                        |> Tuple.second
                        |> List.foldl formatIndex ""
                    )
                )


formatIndex index formatted =
    if index.show then
        formatted ++ String.fromInt index.index ++ index.decoration

    else
        formatted


applyDecoration index ( decs, decorated ) =
    case decs of
        [] ->
            -- If there are no decorations, skip.
            ( decs
            , { index = index + 1
              , decoration = ""
              , show = False
              }
                :: decorated
            )

        currentDec :: remaining ->
            ( remaining
            , { index = index + 1
              , decoration = currentDec
              , show = True
              }
                :: decorated
            )


resetCursor reset cursor =
    case List.reverse reset of
        [] ->
            cursor

        top :: remaining ->
            { current =
                Maybe.withDefault cursor.current top
            , stack =
                cursor.stack
                    |> List.foldr resetStack ( remaining, [] )
                    |> Tuple.second
            }


resetStack index ( reset, found ) =
    case reset of
        [] ->
            ( reset, index :: found )

        Nothing :: remain ->
            ( remain, index :: found )

        (Just new) :: remain ->
            ( remain, new :: found )


advanceCursor indent cursor =
    if indent == List.length cursor.stack + 1 then
        { current = cursor.current + 1
        , stack = cursor.stack
        }

    else if indent > List.length cursor.stack + 1 then
        { current = 1
        , stack = cursor.current :: cursor.stack
        }

    else
        let
            indentDelta =
                List.length cursor.stack
                    - indent
        in
        case List.drop (abs indentDelta) cursor.stack of
            [] ->
                cursor

            lower :: remaining ->
                { current = lower + 1
                , stack = remaining
                }
