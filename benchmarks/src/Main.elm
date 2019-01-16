module Main exposing (document, main, source, suite)

import Benchmark exposing (..)
import Benchmark.LowLevel
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Browser
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region
import Html
import Html.Attributes
import Mark
import Mark.Default
import Mark.Internal
import Task


main =
    Browser.element
        { init = \() -> ( 0, Task.attempt NewResults (Benchmark.LowLevel.sample 100 (Benchmark.LowLevel.operation oldParser)) )

        -- , init = \() -> ( 0, Task.attempt NewResults (Benchmark.LowLevel.sample 100 (Benchmark.LowLevel.operation newParser)) )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = NewResults (Result Benchmark.LowLevel.Error Float)


update msg model =
    case msg of
        NewResults result ->
            case result of
                Ok i ->
                    let
                        _ =
                            Debug.log "result" (newParser ())
                    in
                    ( i, Cmd.none )

                Err error ->
                    let
                        _ =
                            Debug.log "error" error
                    in
                    ( model, Cmd.none )


view model =
    Html.text (String.fromFloat model)


suite : Benchmark
suite =
    describe "Mark"
        [ -- nest as many descriptions as you like
          benchmark "Parse a Simple Blogpost" <|
            oldParser

        --   benchmark "test" <|
        --     \_ -> 1 + 1
        ]


oldParser _ =
    Mark.compile document source


newParser _ =
    Mark.Internal.parse newDocument source


{--}
source =
    """| Title
    My Article

Lorem Ipsum is simply--- dummy text of the printing and {Link|typesetting industry| url = http://mechanical-elephant.com}. Lorem Ipsum has been the industry's /standard/ dummy text ever since the 1500's, when an "unknown printer" took a galley of type and scrambled it to<>make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was *popularised* in the 1960's with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.

But, for real, here's a kitten.

| Image 
    description = What a cute kitten.
    src = http://placekitten.com/g/200/300

| Header
    My section on lists

| Doodad
    pitch = 0.3
    adjustment = 50

What does a list look like?


| Monospace
    This is a code block

    With Multiple lines


"""



-- | List
--     #. This is definitely the first thing.
--     #. Another thing.
--         #.#. sublist
--         #.#. more sublist
--             #.#.#. indented
--         #.#. other sublist
--             #.#.#. subthing
--             #.#.#. other subthing
--     #. and yet, another
--         #.#. and another one


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
                (List.map (\v -> v model) children)
        )
        (Mark.startWith
            (\myTitle myContent ->
                myTitle :: myContent
            )
            (Mark.Default.title [ Font.size 48 ] defaultText)
            (Mark.manyOf
                [ Mark.Default.header [ Font.size 36 ] defaultText

                -- , Mark.Default.list
                --     { style = \_ -> [ Element.spacing 16 ]
                --     , icon = Mark.Default.listIcon
                --     }
                --     defaultText
                , Mark.record2 "Doodad"
                    (\src description model ->
                        Element.text "doodad"
                    )
                    (Mark.field "adjustment" (Mark.intBetween 0 100))
                    (Mark.field "pitch" (Mark.floatBetween 0 1))
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


{-| Render a text fragment.
-}
textFragment : Mark.Internal.Text -> model -> Element.Element msg
textFragment node model_ =
    case node of
        Mark.Internal.Text s txt ->
            Element.el (List.concatMap toStyles s) (Element.text txt)


{-| -}
toStyles : Mark.Internal.Style -> List (Element.Attribute msg)
toStyles style =
    case style of
        Mark.Internal.Bold ->
            [ Font.bold ]

        Mark.Internal.Italic ->
            [ Font.italic ]

        Mark.Internal.Strike ->
            [ Font.strike ]


{-| Here we define our document.

This may seem a bit overwhelming, but 95% of it is copied directly from `Mark.Default.document`. You can then customize as you see fit!

-}
newDocument =
    let
        defaultText =
            Mark.Internal.map
                (\els model ->
                    List.map (\v -> v model) els
                )
                (Mark.Internal.text
                    { view = textFragment
                    , inlines =
                        [ Mark.Internal.inline "Link"
                            (\txt url model ->
                                Element.link [ Font.color (Element.rgb 0.8 0.8 0.9) ]
                                    { url = url
                                    , label =
                                        Element.row [ Element.htmlAttribute (Html.Attributes.style "display" "inline-flex") ]
                                            (List.map (\item -> textFragment item model) txt)
                                    }
                            )
                            |> Mark.Internal.inlineText
                            |> Mark.Internal.inlineString "url"
                        ]

                    -- [ link config.link
                    -- , code config.code
                    -- ]
                    --     ++ config.inlines
                    , replacements = []

                    -- Mark.Default.defaultTextStyle.replacements
                    }
                )
    in
    Mark.Internal.document
        (\pos children model ->
            Element.textColumn
                [ Element.spacing 32
                , Element.padding 100
                , Element.centerX
                , Element.width (Element.px 900)
                ]
                (List.map (\v -> v model) children)
        )
        (Mark.Internal.startWith
            (\pos myTitle myContent ->
                myTitle :: myContent
            )
            (Mark.Internal.block "Title"
                (\pos elements model ->
                    Element.paragraph
                        (Element.Region.heading 1 :: [ Font.size 48 ])
                        (elements model)
                )
                defaultText
            )
            (Mark.Internal.manyOf
                [ --Mark.Internal.Default.header [ Font.size 36 ] defaultText
                  Mark.Internal.block "Header"
                    (\pos elements model ->
                        Element.paragraph
                            (Element.Region.heading 2 :: [ Font.size 36 ])
                            (elements model)
                    )
                    defaultText

                -- , Mark.Internal.Default.list
                --     { style = \_ -> [ Element.spacing 16 ]
                --     , icon = Mark.Internal.Default.listIcon
                --     }
                --     defaultText
                , Mark.Internal.record2 "Doodad"
                    (\src description model ->
                        Element.text "doodad"
                    )
                    (Mark.Internal.field "adjustment" (Mark.Internal.intBetween 0 100))
                    (Mark.Internal.field "pitch" (Mark.Internal.floatBetween 0 1))
                , Mark.Internal.record2 "Image"
                    (\src description model ->
                        Element.image []
                            { src = src
                            , description = description
                            }
                    )
                    (Mark.Internal.field "src" Mark.Internal.string)
                    (Mark.Internal.field "description" Mark.Internal.string)
                , Mark.Internal.block "Monospace"
                    (\pos string model ->
                        Element.el
                            (Element.htmlAttribute (Html.Attributes.style "line-height" "1.4em")
                                :: Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
                                :: [ Element.spacing 5
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
                            )
                            (Element.text (String.trimRight string))
                    )
                    Mark.Internal.multiline

                -- Toplevel Text
                , Mark.Internal.map (\viewEls model -> Element.paragraph [] (viewEls model)) defaultText
                ]
            )
        )
