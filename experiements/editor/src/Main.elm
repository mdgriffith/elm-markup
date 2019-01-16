module Main exposing (main, source)

{-| A simple blog post with a custom inline element for some cool text formatting.

**Note** the blogpost flashes when rendering because it's loading a `font` using `Font.external` from `Elm UI`. To get rid of that, you can add a `link` to the font file directly in the head of your html.

-}

import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region
import Html
import Html.Attributes
import Mark
import Mark.Default
import Mark.Internal
import Parser


source1 =
    """| Doodad
    pitch = 0.3
    adjustment = 50
"""


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



| List
    -> This is definitely the first thing.
    -> Another thing.
        -> sublist
        -> more sublist
            -> indented
        -> other sublist
            -> subthing
            -> other subthing
    -> and yet, another
        -> and another one


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
-- {- Here we define our document.
--    This may seem a bit overwhelming, but 95% of it is copied directly from `Mark.Default.document`. You can then customize as you see fit!
-- -}
-- document =
--     let
--         defaultText =
--             Mark.Default.textWith
--                 { code = Mark.Default.defaultTextStyle.code
--                 , link = Mark.Default.defaultTextStyle.link
--                 , inlines =
--                     [ Mark.inline "Drop"
--                         (\txt model ->
--                             Element.row [ Font.variant Font.smallCaps ]
--                                 (List.map (\item -> Mark.Default.textFragment item model) txt)
--                         )
--                         |> Mark.inlineText
--                     ]
--                 , replacements = Mark.Default.defaultTextStyle.replacements
--                 }
--     in
--     Mark.document
--         (\children model ->
--             Element.textColumn
--                 [ Element.spacing 32
--                 , Element.padding 100
--                 , Element.centerX
--                 , Element.width (Element.px 900)
--                 ]
--                 (List.map (\v -> v model) children)
--         )
--         (Mark.startWith
--             (\myTitle myContent ->
--                 myTitle :: myContent
--             )
--             (Mark.Default.title [ Font.size 48 ] defaultText)
--             (Mark.manyOf
--                 [ --Mark.Default.header [ Font.size 36 ] defaultText
--                 -- , Mark.Default.list
--                 --     { style = \_ -> [ Element.spacing 16 ]
--                 --     , icon = Mark.Default.listIcon
--                 --     }
--                 --     defaultText
--                 , Mark.record2 "Doodad"
--                     (\src description model ->
--                         Element.text "doodad"
--                     )
--                     (Mark.field "adjustment" (Mark.intBetween 0 100))
--                     (Mark.field "pitch" (Mark.floatBetween 0 1))
--                 , Mark.Default.image []
--                 , Mark.Default.monospace
--                     [ Element.spacing 5
--                     , Element.padding 24
--                     , Background.color
--                         (Element.rgba 0 0 0 0.04)
--                     , Border.rounded 2
--                     , Font.size 16
--                     , Font.family
--                         [ Font.external
--                             { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
--                             , name = "Source Code Pro"
--                             }
--                         , Font.sansSerif
--                         ]
--                     ]
--                 -- Toplevel Text
--                 , Mark.map (\viewEls model -> Element.paragraph [] (viewEls model)) defaultText
--                 ]
--             )
--         )
{- Here we define our document.

   This may seem a bit overwhelming, but 95% of it is copied directly from `Mark.Default.document`. You can then customize as you see fit!

-}


document =
    let
        defaultText =
            Mark.Internal.map
                (\els model ->
                    List.map (\v -> v model) els
                )
                (Mark.Internal.text
                    { view = always textFragment
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
                , list
                    { style = \_ -> [ Element.spacing 16 ]
                    , icon = listIcon
                    }
                    defaultText
                , Mark.Internal.record2 "Doodad"
                    (\pos src description model ->
                        Element.text "doodad"
                    )
                    (Mark.Internal.field "adjustment" (Mark.Internal.intBetween 0 100))
                    (Mark.Internal.field "pitch" (Mark.Internal.floatBetween 0 1))
                , Mark.Internal.record2 "Image"
                    (\pos src description model ->
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


type ListIcon
    = Bullet
    | Arrow
    | Number
        { reset : List (Maybe Int)
        , decorations : List String
        }


list :
    { icon : List Int -> ListIcon -> Element msg
    , style : List Int -> List (Element.Attribute msg)
    }
    -> Mark.Internal.Block (model -> List (Element msg))
    -> Mark.Internal.Block (model -> Element msg)
list config textParser =
    Mark.Internal.block "List"
        (\pos items model ->
            Element.column
                (config.style [])
                (List.reverse (Tuple.second (List.foldl (renderListItem config model []) ( 0, [] ) items)))
        )
        (Mark.Internal.nested
            { item = textParser
            , start =
                Mark.Internal.oneOf
                    [ Mark.Internal.exactly "-> " Arrow
                    , Mark.Internal.exactly "--> " Arrow
                    , Mark.Internal.exactly "- " Bullet
                    , Mark.Internal.exactly "-- " Bullet

                    -- , Mark.advanced
                    --     (Parser.loop ( [], [] ) numberIconParser)
                    ]
            }
        )


renderListItem config model stack (Mark.Internal.Nested item) ( index, accumulated ) =
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
                        (\v ->
                            Element.paragraph
                                []
                                (v model)
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


resetStack index ( reset, found ) =
    case reset of
        [] ->
            ( reset, index :: found )

        Nothing :: remain ->
            ( remain, index :: found )

        (Just new) :: remain ->
            ( remain, new :: found )


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



-- document =
--     let
--         defaultText =
--             Mark.Default.textWith
--                 { code = Mark.Default.defaultTextStyle.code
--                 , link = Mark.Default.defaultTextStyle.link
--                 , inlines =
--                     [ Mark.inline "Drop"
--                         (\txt model ->
--                             Element.row [ Font.variant Font.smallCaps ]
--                                 (List.map (\item -> Mark.Default.textFragment item model) txt)
--                         )
--                         |> Mark.inlineText
--                     ]
--                 , replacements = Mark.Default.defaultTextStyle.replacements
--                 }
--     in
--     Mark.Internal.document
--         (\range children model ->
--             Element.textColumn
--                 [ Element.spacing 32
--                 , Element.padding 100
--                 , Element.centerX
--                 , Element.width (Element.px 900)
--                 ]
--                 [ children model ]
--         )
--         -- (Mark.Internal.startWith
--         --     (\range myTitle myContent model ->
--         --         myTitle model :: [ Element.text myContent ]
--         --     )
--         -- (Mark.Internal.block "Title"
--         --     (\range child model ->
--         --         Element.el [ Font.size 48 ] (Element.text child)
--         --     )
--         --     Mark.Internal.string
--         -- )
--         (Mark.Internal.record2 "Doodad"
--             (\src description model ->
--                 Element.text ("doodad" ++ String.fromInt src ++ String.fromFloat description)
--             )
--             (Mark.Internal.field "adjustment" Mark.Internal.int)
--             (Mark.Internal.field "pitch" Mark.Internal.float)
--         )
-- Mark.Internal.string
-- (Mark.manyOf
--     [ --Mark.Default.header [ Font.size 36 ] defaultText
--     -- , Mark.Default.list
--     --     { style = \_ -> [ Element.spacing 16 ]
--     --     , icon = Mark.Default.listIcon
--     --     }
--     --     defaultText
--     -- , Mark.record2 "Doodad"
--     --     (\src description model ->
--     --         Element.text "doodad"
--     --     )
--     --     (Mark.field "adjustment" (Mark.intBetween 0 100))
--     --     (Mark.field "pitch" (Mark.floatBetween 0 1))
--     , Mark.Default.image []
--     , Mark.Default.monospace
--         [ Element.spacing 5
--         , Element.padding 24
--         , Background.color
--             (Element.rgba 0 0 0 0.04)
--         , Border.rounded 2
--         , Font.size 16
--         , Font.family
--             [ Font.external
--                 { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
--                 , name = "Source Code Pro"
--                 }
--             , Font.sansSerif
--             ]
--         ]
--     -- Toplevel Text
--     , Mark.map (\viewEls model -> Element.paragraph [] (viewEls model)) defaultText
--     ]
-- )
-- )


type Msg
    = UpdatedSource String


main =
    Browser.sandbox
        { init = { source = source }
        , update = update
        , view = view
        }


update msg model =
    case msg of
        UpdatedSource src ->
            { model | source = src }


view model =
    Element.layout
        [ Element.height Element.shrink
        , Element.width Element.fill
        ]
        (Element.row [ Element.width Element.fill ]
            [ -- case Mark.Internal.parse document model.source of
              -- Err errors ->
              --     let
              --         _ =
              --             Debug.log "parse errors" errors
              --     in
              --     Element.text "parsing errors"
              -- Ok parsed ->
              --     Element.el []
              --         (Element.text (Mark.Internal.toString 0 parsed))
              Input.multiline
                [ Font.family
                    [ Font.external
                        { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                        , name = "Source Code Pro"
                        }
                    , Font.sansSerif
                    ]
                , Element.width
                    Element.fill
                , Element.height Element.fill
                , Element.alignTop
                , Element.focused
                    []
                ]
                { text = model.source
                , onChange = UpdatedSource
                , label = Input.labelAbove [] (Element.text "Source")
                , spellcheck = False
                , placeholder = Nothing
                }
            , case Mark.Internal.compile document model.source of
                Ok doc ->
                    let
                        _ =
                            Debug.log "document" doc
                    in
                    doc ()

                -- Element.text "totally parsed"
                Err errors ->
                    let
                        _ =
                            Debug.log "Errs" errors
                    in
                    Element.column
                        [ Font.family
                            [ Font.external
                                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                                , name = "Source Code Pro"
                                }
                            , Font.sansSerif
                            ]
                        , Background.color (Element.rgb 0 0 0)
                        , Element.width Element.fill
                        ]
                        []

            -- (List.map (Element.html << Mark.errorToHtml Mark.Dark) errors)
            ]
        )


edges =
    { top = 0
    , left = 0
    , right = 0
    , bottom = 0
    }


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
