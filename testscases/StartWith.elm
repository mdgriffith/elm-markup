module Debugging exposing (main)

import Browser
import Element exposing (Element)
import Element.Font as Font
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Mark
import Parser.Advanced exposing (DeadEnd)


markup : String
markup =
    """Below we demonstrate that parser breaks when startWith block is nested in another block

| Parent

    | First
    
    | Second

Text after parent block.
"""


document : Mark.Document (Element.Element msg)
document =
    let
        text : Mark.Block (Element msg)
        text =
            Mark.string
                |> Mark.map Element.text

        parent : Mark.Block (Element msg)
        parent =
            Mark.block "Parent"
                (Element.column [])
                (Mark.startWith
                    group
                    first
                    second
                )

        first : Mark.Block (Element msg)
        first =
            Mark.stub "First" (Element.text "I'm first")

        second : Mark.Block (Element msg)
        second =
            Mark.stub "Second" (Element.text "I'm second")

        group : Element msg -> Element msg -> List (Element msg)
        group a b =
            [ a, b ]
    in
    Mark.document
        (Element.column [])
        (Mark.manyOf
            [ text
            , parent
            ]
        )


main : Html msg
main =
    case Mark.parse document markup of
        Ok elements ->
            elements
                |> Element.layout []

        Err deadEnds ->
            deadEnds
                |> Debug.log "Error Parsing Document"
                |> List.map Debug.toString
                |> List.map Element.text
                |> List.map List.singleton
                |> List.map (Element.paragraph [ Font.family [ Font.monospace ] ])
                |> Element.column []
                |> Element.layout []
