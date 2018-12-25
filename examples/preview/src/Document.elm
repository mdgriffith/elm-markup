module Document exposing (document)

{-| -}

import Browser
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html
import Html.Attributes
import Http
import Mark
import Mark.Default


{-| Here we define our document.

This may seem a bit overwhelming, but 95% of it is copied directly from `Mark.Default.document`. You can then customize as you see fit!

-}
document =
    let
        defaultText =
            Mark.Default.textWith
                { code = Mark.Default.defaultTextStyle.code
                , link = Mark.Default.defaultTextStyle.link
                , inlines =
                    [ Mark.inline "Drop"
                        (\txt model ->
                            Element.row [ Font.variant Font.smallCaps ]
                                (List.map (\item -> Mark.Default.textFragment item model) txt)
                        )
                        |> Mark.inlineText
                    ]
                , replacements = Mark.Default.defaultTextStyle.replacements
                }
    in
    Mark.document
        (\children model ->
            Element.textColumn
                [ Element.spacing 32
                , Element.padding 100
                , Element.centerX
                , Element.width (Element.px 900)
                ]
                (List.map (\view -> view model) children)
        )
        (Mark.startWith
            (\myTitle myContent ->
                myTitle :: myContent
            )
            (Mark.Default.title [ Font.size 48 ] defaultText)
            (Mark.manyOf
                [ Mark.Default.header [ Font.size 36 ] defaultText
                , Mark.Default.list
                    { style = \_ -> [ Element.spacing 16 ]
                    , icon = Mark.Default.listIcon
                    }
                    defaultText
                , Mark.Default.image []
                , Mark.Default.monospace
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
