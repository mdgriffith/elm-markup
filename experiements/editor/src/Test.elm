port module Main exposing (main)

import Browser
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region
import Html
import Html.Attributes
import Mark.Internal


source =
    """| Note
    Yup here's /my/ note.

| Int
    5

| Note
    Yup

| IntBetween
    20


| Image 
    description = What a cute kitten.
    src = http://placekitten.com/g/200/300

| Int
    5


"""


document =
    Mark.Internal.document
        (\data model ->
            case data.found of
                Mark.Internal.Found range children ->
                    Element.textColumn
                        [ Element.spacing 32
                        , Element.padding 100
                        , Element.centerX
                        , Element.width (Element.px 900)
                        ]
                        [ children model ]

                -- (List.map (\v -> v model) children)
                Mark.Internal.Unexpected unexpected ->
                    Element.text ("\u{1F914}  Hmm, Doc: " ++ Debug.toString unexpected.problem)
        )
        textBlock


startWith =
    Mark.Internal.startWith
        (\found model ->
            case found of
                Mark.Internal.Found _ ( start, end ) ->
                    Element.textColumn []
                        (start model :: List.map (\v -> v model) end)

                Mark.Internal.Unexpected unexpected ->
                    viewError unexpected model
        )
        title
        many


textBlock =
    Mark.Internal.block "Note"
        (\data model ->
            case data.found of
                Mark.Internal.Found range elements ->
                    Element.paragraph
                        (Element.Region.heading 1
                            :: [ Font.size 48
                               , highlightBorderIfMatch range model.cursor
                               , Border.width 4
                               , Border.rounded 4
                               ]
                        )
                        (elements model)

                Mark.Internal.Unexpected unexpected ->
                    viewError unexpected model
        )
        text


text =
    Mark.Internal.map
        (\els model ->
            List.map (\v -> v model) els
        )
        (Mark.Internal.text
            { view = textFragment
            , error = \unexpected model -> Element.text "oh no"
            , inlines =
                [ Mark.Internal.inline "!" (\model -> Element.text "~!!!~")
                , Mark.Internal.inline "Exclaim" (\str model -> Element.text (str ++ "!"))
                    |> Mark.Internal.inlineString "exp"
                , Mark.Internal.inline "Link"
                    (\txt str model ->
                        Element.text
                            ("[" ++ String.join "" (List.map inlineTextFragment txt) ++ "](" ++ str ++ ")")
                    )
                    |> Mark.Internal.inlineText
                    |> Mark.Internal.inlineString "exp"
                ]
            , replacements =
                [ Mark.Internal.replacement "..." "…"
                , Mark.Internal.replacement "<>" "\u{00A0}"
                , Mark.Internal.replacement "---" "—"
                , Mark.Internal.replacement "--" "–"
                , Mark.Internal.replacement "//" "/"
                , Mark.Internal.replacement "'" "’"
                , Mark.Internal.balanced
                    { start = ( "\"", "“" )
                    , end = ( "\"", "”" )
                    }
                ]
            }
        )


inlineTextFragment : Mark.Internal.Text -> String
inlineTextFragment node =
    case node of
        Mark.Internal.Text styles txt ->
            txt


{-| Render a text fragment.
-}
textFragment : Mark.Internal.Range Mark.Internal.Position -> Mark.Internal.Text -> model -> Element.Element msg
textFragment pos node model_ =
    case node of
        Mark.Internal.Text styles txt ->
            Element.el (List.concatMap toStyles styles) (Element.text txt)


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


many =
    Mark.Internal.manyOf
        viewError
        [ intBlock
        , title
        , intBetweenBlock
        , image
        ]


withinOffsetRange range offset =
    range.start.offset <= offset.start && range.end.offset >= offset.end


pink =
    Element.rgb255 244 0 245


highlightBorderIfMatch range offset =
    if withinOffsetRange range offset then
        Border.color pink

    else
        Border.color (Element.rgba 0 0 0 0)


image =
    Mark.Internal.record2 "Image"
        (\pos src description model ->
            Element.image []
                { description = description
                , src = src
                }
        )
        viewError
        (Mark.Internal.field "src" Mark.Internal.string)
        (Mark.Internal.field "description" Mark.Internal.string)


intBlock =
    Mark.Internal.block "Int"
        (\data model ->
            case data.found of
                Mark.Internal.Found range str ->
                    Element.paragraph
                        (Element.Region.heading 1
                            :: [ Font.size 48
                               , highlightBorderIfMatch range model.cursor
                               , Border.width 4
                               , Border.rounded 4
                               ]
                        )
                        [ Element.text (String.fromInt str)
                        ]

                Mark.Internal.Unexpected unexpected ->
                    viewError unexpected model
        )
        Mark.Internal.int


intBetweenBlock =
    Mark.Internal.block "IntBetween"
        (\data model ->
            case data.found of
                Mark.Internal.Found range str ->
                    Element.paragraph
                        (Element.Region.heading 1
                            :: [ Font.size 48
                               , highlightBorderIfMatch range model.cursor
                               , Border.width 4
                               , Border.rounded 4
                               ]
                        )
                        [ Element.text (String.fromInt str) ]

                Mark.Internal.Unexpected unexpected ->
                    viewError unexpected model
        )
        (Mark.Internal.intBetween
            0
            20
        )


title =
    Mark.Internal.block "Note"
        (\data model ->
            case data.found of
                Mark.Internal.Found range str ->
                    Element.paragraph
                        (Element.Region.heading 1
                            :: [ Font.size 48
                               , highlightBorderIfMatch range model.cursor
                               , Border.width 4
                               , Border.rounded 4
                               ]
                        )
                        [ Element.text str ]

                Mark.Internal.Unexpected unexpected ->
                    viewError unexpected model
        )
        Mark.Internal.string


viewError unexpected model =
    Element.textColumn
        [ Element.padding 16
        , Element.spacing 32
        , Background.color (Element.rgb 0.96 0.96 0.96)
        , highlightBorderIfMatch unexpected.range model.cursor
        , Border.width 4
        , Border.rounded 4
        ]
        [ Element.paragraph []
            [ Element.text
                "\u{1F914} is that really what you meant?"
            ]
        , Element.text (Debug.toString unexpected.problem)
        ]



{- Program -}


main =
    Browser.element
        { init =
            \() ->
                ( { source = source
                  , cursor = { start = 0, end = 0 }
                  }
                , Cmd.none
                )
        , update = update
        , view = view
        , subscriptions =
            \_ ->
                cursor UpdatedCursor

        -- Sub.none
        }


type alias Cursor =
    { start : Int
    , end : Int
    }


port cursor : (Cursor -> msg) -> Sub msg


type Msg
    = UpdatedSource String
    | UpdatedCursor Cursor


update msg model =
    case msg of
        UpdatedSource src ->
            ( { model | source = src }, Cmd.none )

        UpdatedCursor curs ->
            ( { model | cursor = curs }
            , Cmd.none
            )


view model =
    Element.layout
        [ Element.height Element.shrink
        , Element.width Element.fill
        , Element.inFront
            (Element.el [ Element.alignBottom, Element.alignLeft ]
                (Element.text ("cursor: " ++ String.fromInt model.cursor.start ++ "," ++ String.fromInt model.cursor.end))
            )
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
                , Element.htmlAttribute (Html.Attributes.id "editor")
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
                        parsed =
                            Mark.Internal.parse document model.source

                        -- _ =
                        --     Debug.log "parsed"
                        --         parsed
                        _ =
                            Result.map
                                (\ast ->
                                    let
                                        desc =
                                            Debug.log "desc" (Mark.Internal.getDesc model.cursor ast)

                                        _ =
                                            List.map (Debug.log "desc" << Mark.Internal.toString 0) desc

                                        -- _ =
                                        --     Debug.log "num terms" (List.length desc)
                                    in
                                    desc
                                 -- List.map (Debug.log "description") desc
                                )
                                parsed

                        -- -- _ =
                        -- --     Debug.log "prettyParsed"
                        -- --         (Result.map Mark.Internal.prettyParsed parsed)
                        -- _ =
                        --     Debug.log "document" doc
                    in
                    doc model

                -- Element.text "totally parsed"
                Err errors ->
                    let
                        parsed =
                            Mark.Internal.parse document model.source

                        _ =
                            case parsed of
                                Ok _ ->
                                    Debug.log "parsing success" ""

                                Err err ->
                                    Debug.log "error parsing"
                                        (Debug.toString err)

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
