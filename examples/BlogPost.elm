module Main exposing (main, myParser, source)

{-| A simple blog post with a custom inline element for some cool text formatting.

**Note** the blogpost flashes when rendering because it's loading a `font` using `Font.external` from `Elm UI`.  To get rid of that, you can add a `link` to the font file directly in the head of your html.


-}

import Element
import Element.Font as Font
import Html
import Html.Attributes
import Mark
import Mark.Custom as Custom
import Parser


source =
    """
| title
    My Article

{drop| Lorem Ipsum is simply---}dummy text of the printing and [`typesetting industry`](http://mechanical-elephant.com). Lorem Ipsum has been the industry's /standard/ dummy text ever since the 1500's, when an "unknown printer" took a galley of type and scrambled it to+make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960's with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.

But, for real, here's a kitten.


| image http://placekitten.com/g/200/300
    What a cute kitten.


| header
    My section on lists


What does a list look like?


| list
    - This is definitely the first thing.
    - Another thing.
        - sublist
        - more sublist
            - and yet, another




"""


myParser =
    Mark.parseWith
        { styling =
            \model -> Mark.defaultStyling
        , blocks =
            Mark.defaultBlocks
        , inlines =
            [ Custom.inline "drop"
                (\string styling model ->
                    let
                        txt =
                            String.trim string
                    in
                    if txt == "" then
                        []

                    else
                        [ Element.el
                            [ Element.alignLeft
                            , Font.size 64
                            , Element.htmlAttribute (Html.Attributes.style "line-height" "0.75em")
                            , Element.moveDown 8
                            ]
                            (Element.text (String.toUpper (String.slice 0 1 txt)))
                        , Element.el [ Font.size 16 ]
                            (Element.text (String.toUpper (String.dropLeft 1 txt)))
                        ]
                )
            ]
        }


main =
    case myParser source of
        Ok element ->
            Element.layout
                [ Font.family [ Font.typeface "EB Garamond" ]
                ]
                (element ())

        Err errors ->
            let
                _ =
                    Debug.log "Errors" errors
            in
            Html.text "Ugh, errors"
