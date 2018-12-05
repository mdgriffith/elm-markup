module Mark.Default exposing
    ( document
    , title, header, list, monospace, image
    , defaultText, replacements, text
    )

{-|

@docs document

@docs title, header, list, monospace, image

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region
import Html.Attributes
import Mark.Custom
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


{-| -}
document : Mark.Custom.Document (model -> Element msg)
document =
    Mark.Custom.document
        (\children model ->
            Element.textColumn []
                (List.map (\view -> view model) children)
        )
        (Mark.Custom.manyOf
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
            , Mark.Custom.map (\viewEls model -> Element.paragraph [] (viewEls model)) defaultText
            ]
        )


image =
    Mark.Custom.record2 "Image"
        (\src description model ->
            Element.image []
                { src = src
                , description = description
                }
        )
        (Mark.Custom.field "src" Mark.Custom.string)
        (Mark.Custom.field "description" Mark.Custom.string)


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


{-| -}
title : List (Element.Attribute msg) -> Mark.Custom.Block (model -> List (Element msg)) -> Mark.Custom.Block (model -> Element msg)
title attrs titleText =
    Mark.Custom.block "Title"
        (\elements model ->
            Element.paragraph
                (Element.Region.heading 1 :: attrs)
                (elements model)
        )
        titleText


{-| -}
header : List (Element.Attribute msg) -> Mark.Custom.Block (model -> List (Element msg)) -> Mark.Custom.Block (model -> Element msg)
header attrs textParser =
    Mark.Custom.block "Header"
        (\elements model ->
            Element.paragraph
                (Element.Region.heading 2 :: attrs)
                (elements model)
        )
        textParser


{-| -}
monospace : List (Element.Attribute msg) -> Mark.Custom.Block (model -> Element msg)
monospace attrs =
    Mark.Custom.block "Monospace"
        (\string model ->
            Element.paragraph
                (Element.htmlAttribute (Html.Attributes.style "line-height" "1.4em")
                    :: Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
                    :: attrs
                )
                [ Element.text string ]
        )
        Mark.Custom.multiline


{-| -}
text :
    { code : List (Element.Attribute msg)
    , link : List (Element.Attribute msg)
    }
    -> Mark.Custom.Block (model -> List (Element msg))
text style =
    Mark.Custom.textWith
        { view = textView style
        , inlines = []
        , merge =
            \els model ->
                List.map (\view -> view model) els
        , replacements = replacements
        }


textView config textNode =
    textFragment config textNode


textFragment config node model_ =
    case node.style of
        Mark.Custom.NoFormatting txt ->
            case node.link of
                Nothing ->
                    Element.text txt

                Just url ->
                    Element.link config.link
                        { url = url
                        , label = Element.text txt
                        }

        Mark.Custom.Styles styles txt ->
            case node.link of
                Nothing ->
                    Element.el (List.concatMap (toStyles config) styles) (Element.text txt)

                Just url ->
                    Element.link (config.link ++ List.concatMap (toStyles config) styles)
                        { url = url
                        , label = Element.text txt
                        }


toStyles config style =
    case style of
        Mark.Custom.Bold ->
            [ Font.bold ]

        Mark.Custom.Italic ->
            [ Font.italic ]

        Mark.Custom.Strike ->
            [ Font.strike ]

        Mark.Custom.Underline ->
            [ Font.underline ]

        Mark.Custom.Token ->
            config.code


{-| Replace certain characters with improved typographical ones.
Escaping a character will skip the replacement, so this isn't mandatory.

  - `"<>"` -> A non-breaking space which glues words together so that they don't break when wrapping.
  - `"--"` -> en-dash, `–`
  - `"---"` -> em-dash, `—`
  - `//` -> `/`
  - Quotation marks will be replaced with curly quotes.
  - `"..."` -> ellipses, `…`
  - `'` -> `’`

-}
replacements : List Mark.Custom.Replacement
replacements =
    [ Mark.Custom.replacement "..." "…"
    , Mark.Custom.replacement "<>" "\u{00A0}"
    , Mark.Custom.replacement "---" "—"
    , Mark.Custom.replacement "--" "–"
    , Mark.Custom.replacement "//" "/"
    , Mark.Custom.replacement "'" "’"
    , Mark.Custom.balanced
        { start = ( "\"", "“" )
        , end = ( "\"", "”" )
        }
    ]



{-


   Nested List Handling


-}


{-| Parse a nested list
-}
list :
    { icon : List Int -> ListIcon -> Element msg
    , style : List Int -> List (Element.Attribute msg)
    }
    -> Mark.Custom.Block (model -> List (Element msg))
    -> Mark.Custom.Block (model -> Element msg)
list config textParser =
    Mark.Custom.block "List"
        (\items model ->
            Element.column
                (config.style [])
                (List.indexedMap (renderListItem config model []) items)
        )
        (Mark.Custom.nested
            { item = textParser
            , start =
                Mark.Custom.advanced
                    listIconParser
            }
        )


{-| -}
listIconParser : Parser Mark.Custom.Context Mark.Custom.Problem ListIcon
listIconParser =
    Parser.oneOf
        [ Parser.succeed Arrow
            |. Parser.oneOf
                [ Parser.token (Parser.Token "->" (Mark.Custom.Expecting "->"))
                , Parser.token (Parser.Token "-->" (Mark.Custom.Expecting "-->"))
                ]
            |. Parser.chompIf (\c -> c == ' ') Mark.Custom.Space
        , Parser.succeed identity
            |. Parser.token (Parser.Token "-" (Mark.Custom.Expecting "-"))
            |= Parser.oneOf
                [ Parser.succeed Bullet
                    |. Parser.oneOf
                        [ Parser.token (Parser.Token " " Mark.Custom.Space)
                        , Parser.token (Parser.Token "-" Mark.Custom.Dash)
                        ]
                    |. Parser.chompIf (\c -> c == ' ') Mark.Custom.Space
                , Parser.succeed
                    (\( reset, decorations ) ->
                        Number { reset = reset, decorations = decorations }
                    )
                    |= Parser.loop ( [], [] ) numberIconParser
                    |. Parser.chompIf (\c -> c == ' ') Mark.Custom.Space
                ]
        ]


{-| -}
numberIconParser :
    ( List (Maybe Int), List String )
    -> Parser Mark.Custom.Context Mark.Custom.Problem (Parser.Step ( List (Maybe Int), List String ) ( List (Maybe Int), List String ))
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
                    |= Parser.getChompedString (Parser.chompIf Char.isDigit Mark.Custom.Integer)
                    |= Parser.getChompedString (Parser.chompWhile Char.isDigit)
                , Parser.succeed Nothing
                    |. Parser.chompIf Char.isAlpha Mark.Custom.ExpectingAlphaNumeric
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


renderListItem config model stack index (Mark.Custom.Nested item) =
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
