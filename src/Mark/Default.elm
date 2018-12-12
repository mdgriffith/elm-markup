module Mark.Default exposing
    ( document
    , title, header, monospace, image, ListIcon(..), list, listIcon
    , textWith, defaultTextStyle, code, link, textFragment
    )

{-| This is a document that renders to [`elm-ui`'s](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/) `Element msg`.

**Though remember**, one of the great powers of this library is in [writing custom blocks](https://package.elm-lang.org/packages/mdgriffith/elm-markup/latest/Mark) to suit your specific domain or style needs! You can parse a document to any data structure.

Think of this module as a **starting point** for writing something like a blog article.

Check out the source to get an idea of how things fit together.

_Then feel free to borrow or copy anything you see._

@docs document


# Common Blocks

These are some common blocks that you might need.

Feel free to define your own document and pick and choose what blocks make sense for you.

@docs title, header, monospace, image, ListIcon, list, listIcon


# Text and Inlines

@docs textWith, defaultTextStyle, code, link, textFragment

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region
import Html.Attributes
import Mark
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


{-| This document results in a `view` function, `model -> Element msg`.

It includes all the blocks and inline blocks referenced in this module.

**Note** this document requires a `title` as the first block of each document by using `Mark.startWith`. If that isn't what you want, create your own document with your own constraints.

-}
document : Mark.Document (model -> Element msg)
document =
    let
        defaultText =
            textWith defaultTextStyle
    in
    Mark.document
        (\children model ->
            Element.textColumn []
                (List.map (\view -> view model) children)
        )
        (Mark.startWith
            (\myTitle myContent ->
                myTitle :: myContent
            )
            (title [ Font.size 48 ] defaultText)
            (Mark.manyOf
                [ header [ Font.size 36 ] defaultText
                , list
                    { style = listStyles
                    , icon = listIcon
                    }
                    defaultText
                , image []
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
        )


{-| Create an image.

    | Image
        src = https://placekitten.com/200/300
        description = A cute kitty cat.

-}
image : List (Element.Attribute msg) -> Mark.Block (model -> Element msg)
image attrs =
    Mark.record2 "Image"
        (\src description model ->
            Element.image attrs
                { src = src
                , description = description
                }
        )
        (Mark.field "src" Mark.string)
        (Mark.field "description" Mark.string)


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

    | Monospace
        Everything in this block will be rendered monospaced.

        Including this line.

        And this one.

-}
monospace : List (Element.Attribute msg) -> Mark.Block (model -> Element msg)
monospace attrs =
    Mark.block "Monospace"
        (\string model ->
            Element.el
                (Element.htmlAttribute (Html.Attributes.style "line-height" "1.4em")
                    :: Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
                    :: attrs
                )
                (Element.text (String.trimRight string))
        )
        Mark.multiline


{-| Some default styling for `code` and `link` inline blocks.

Also replaces certain characters with some typographical niceties.

  - `...` is converted to the ellipses unicode character.
  - `"` Straight double quotes are [replaced with curly quotes](https://practicaltypography.com/straight-and-curly-quotes.html)
  - `'` Single Quotes are replaced with apostrophes. In the future we might differentiate between curly single quotes and apostrophes.
  - `--` is replaced with an en-dash.
  - `---` is replaced with an em-dash.
  - `<>` - will create a non-breaking space (`&nbsp;`). This is not for manually increasing space (sequential `<>` tokens will only render as one `&nbsp;`), but to signify that the space between two words shouldn't break when wrapping. Think of this like glueing two words together.
  - `//` will change to `/`. Normally `/` starts italic formatting. To escape this, we'd normally do `\/`, though that looks pretty funky. `//` just feels better!

These transformations also don't apply inside inline `code` or inside the `monospace` block.

**Note** Escaping the start of any of these characters will skip the replacement.

**Also Note** If you're not familiar with `en-dash` or `em-dash`, I definitely [recommend reading a small bit about it](https://practicaltypography.com/hyphens-and-dashes.html)—they're incredibly useful.

-}
defaultTextStyle :
    { code : List (Element.Attribute msg1)
    , link : List (Element.Attribute msg)
    , inlines : List (Mark.Inline (a -> Element msg))
    , replacements : List Mark.Replacement
    }
defaultTextStyle =
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
    , inlines = []
    , replacements =
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
    }


{-| Render text into `Element msg`.

Includes `Mark.Default.link` and `Mark.Default.code` inline blocks, which can be styled as you'd like.

**Note** this is not a complicated function. If it doesn't meet your needs, don't be afraid to write your own using `Mark.text`!

-}
textWith :
    { code : List (Element.Attribute msg)
    , link : List (Element.Attribute msg)
    , inlines : List (Mark.Inline (model -> Element msg))
    , replacements : List Mark.Replacement
    }
    -> Mark.Block (model -> List (Element msg))
textWith config =
    Mark.map
        (\els model ->
            List.map (\view -> view model) els
        )
        (Mark.text
            { view = textFragment
            , inlines =
                [ link config.link
                , code config.code
                ]
                    ++ config.inlines
            , replacements = config.replacements
            }
        )


{-| A custom inline block for code. This is analagous to `backticks` in markdown.

Though, style it however you'd like.

`{Code| Here is my styled inline code block }`

-}
code : List (Element.Attribute msg) -> Mark.Inline (model -> Element msg)
code style =
    Mark.inline "Code"
        (\txt model ->
            Element.row style
                (List.map (\item -> textFragment item model) txt)
        )
        |> Mark.inlineText


{-| A custom inline block for links.

`{Link|My link text|url=http://google.com}`

-}
link : List (Element.Attribute msg) -> Mark.Inline (model -> Element msg)
link style =
    Mark.inline "Link"
        (\txt url model ->
            Element.link style
                { url = url
                , label =
                    Element.row [ Element.htmlAttribute (Html.Attributes.style "display" "inline-flex") ]
                        (List.map (\item -> textFragment item model) txt)
                }
        )
        |> Mark.inlineText
        |> Mark.inlineString "url"


{-| Render a text fragment.
-}
textFragment : Mark.Text -> model -> Element msg
textFragment node model_ =
    case node of
        Mark.Text styles txt ->
            Element.el (List.concatMap toStyles styles) (Element.text txt)


{-| -}
toStyles : Mark.Style -> List (Element.Attribute msg)
toStyles style =
    case style of
        Mark.Bold ->
            [ Font.bold ]

        Mark.Italic ->
            [ Font.italic ]

        Mark.Strike ->
            [ Font.strike ]



{-


   Nested List Handling


-}


{-| A nested list with an expected indentation of 4 spaces per level. As far as icons:

  - `-` or `--` indicates a bullet.
  - `->` or `-->` will render as an ➙
  - `#.` will be auto-numbered as and rendered `1.`, or `2.` or whatever number you're at.
      - Any punctuation that comes after the alpha character will be maintained. So, `#)` will format as `1)` and `#.` will format as `1.`
      - Nested numbers can be rendered if they're in the right place. So `#.#)` would render as `1.2)`
  - A literal number instead of an alpha will reset the auto numbering to that literal number. So, `9.` means start counting up from 9 from here on out.
  - This can also applied in a nested manner as `#.9`, which will reset the inner number.

For configuring this block.

  - `icon` takes in a `List Int`, which is the current index of where that element is in the nested list. So, `[1,2]` means you're at postion `1.2`.
  - `style` is

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
                (List.reverse (Tuple.second (List.foldl (renderListItem config model []) ( 0, [] ) items)))
        )
        (Mark.nested
            { item = textParser
            , start =
                Mark.oneOf
                    [ Mark.exactly "-> " Arrow
                    , Mark.exactly "--> " Arrow
                    , Mark.exactly "- " Bullet
                    , Mark.exactly "-- " Bullet
                    , Mark.advanced
                        (Parser.loop ( [], [] ) numberIconParser)
                    ]
            }
        )


renderListItem config model stack (Mark.Nested item) ( index, accumulated ) =
    case item.content of
        ( icon, items ) ->
            let
                ( newIndex, newStack ) =
                    advanceIndex icon index stack
            in
            ( newIndex
            , Element.row []
                [ Element.el [ Element.alignTop ]
                    (config.icon (newIndex :: newStack) icon)
                , Element.textColumn
                    (config.style (index :: stack))
                    (List.map
                        (\view ->
                            Element.paragraph
                                []
                                (view model)
                        )
                        items
                        ++ List.reverse (Tuple.second (List.foldl (renderListItem config model (newIndex :: newStack)) ( 0, [] ) item.children))
                    )
                ]
                :: accumulated
            )


advanceIndex icon index stack =
    case icon of
        Number { reset } ->
            resetIndex reset (index + 1) stack

        _ ->
            ( index + 1, stack )


resetIndex reset cursor stack =
    case List.reverse reset of
        [] ->
            ( cursor, stack )

        top :: remaining ->
            ( Maybe.withDefault cursor top
            , stack
                |> List.foldr resetStack ( remaining, [] )
                |> Tuple.second
            )


{-| -}
numberIconParser :
    ( List (Maybe Int), List String )
    -> Parser Mark.Context Mark.Problem (Parser.Step ( List (Maybe Int), List String ) ListIcon)
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
                    |. Parser.chompIf (\c -> c == '#') (Mark.Expecting "#")
                ]
            |= Parser.getChompedString
                (Parser.chompWhile
                    (\c ->
                        c
                            /= ' '
                            && (c /= '#')
                            && not (Char.isDigit c)
                    )
                )
        , Parser.succeed
            (Parser.Done
                (Number
                    { reset = List.reverse cursorReset
                    , decorations = List.reverse decorations
                    }
                )
            )
            |. Parser.chompIf (\c -> c == ' ') Mark.Space
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


type alias Index =
    { decoration : String
    , index : Int
    , show : Bool
    }


{-| List icon options.

For `Number`, we have

  - `reset`, which is the desired index reset if there is one.
  - `decorations`, which are the strings to append after each number.

So, a list item like:

`#.5)`

Would result in the following `ListIcon`:

    Number
        { reset = [ Nothing, Just 5 ]
        , decorations = [ ".", ")" ]
        }

-}
type ListIcon
    = Bullet
    | Arrow
    | Number
        { reset : List (Maybe Int)
        , decorations : List String
        }


{-| A default list icon renderer.
-}
listIcon : List Int -> ListIcon -> Element msg
listIcon index symbol =
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
            , { index = index
              , decoration = ""
              , show = False
              }
                :: decorated
            )

        currentDec :: remaining ->
            ( remaining
            , { index = index
              , decoration = currentDec
              , show = True
              }
                :: decorated
            )


resetStack index ( reset, found ) =
    case reset of
        [] ->
            ( reset, index :: found )

        Nothing :: remain ->
            ( remain, index :: found )

        (Just new) :: remain ->
            ( remain, new :: found )
