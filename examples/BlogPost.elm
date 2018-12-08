module Main exposing (main, source)

{-| A simple blog post with a custom inline element for some cool text formatting.

**Note** the blogpost flashes when rendering because it's loading a `font` using `Font.external` from `Elm UI`. To get rid of that, you can add a `link` to the font file directly in the head of your html.

-}

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html
import Html.Attributes
import Mark
import Mark.Default
import Parser


source =
    """| Title
    My Article

{Drop|Lorem Ipsum is simply---}dummy text of the printing and {Link|typesetting industry| url = http://mechanical-elephant.com}. Lorem Ipsum has been the industry's /standard/ dummy text ever since the 1500's, when an "unknown printer" took a galley of type and scrambled it to<>make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was *popularised* in the 1960's with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.

But, for real, here's a kitten.

| Image 
    src = http://placekitten.com/g/200/300
    description = What a cute kitten.

| Header
    My section on lists

What does a list look like?

| List
    #. This is definitely the first thing.
    #. Another thing.
        #.#. sublist
        #.#. more sublist
    #. and yet, another
        #.#. and another one


"""



--


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


main =
    case Mark.parse document source of
        Ok element ->
            Element.layout
                [ Font.family [ Font.typeface "EB Garamond" ]
                ]
                (element ())

        Err errors ->
            let
                _ =
                    Debug.log "Error Parsing Document" errors
            in
            Element.layout
                []
                (Element.text "Error parsing document!")
