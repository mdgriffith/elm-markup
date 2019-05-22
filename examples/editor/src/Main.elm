module Main exposing (document, main)

{-| -}

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Http
import Mark
import Mark.Error
import Ports


main =
    Browser.document
        { init =
            \() ->
                ( { parsed = Nothing
                  , errors = []
                  , cursor = Nothing
                  }
                , Http.get
                    { url = "/articles/Ipsum.emu"
                    , expect = Http.expectString GotSrc
                    }
                )
        , view = view
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Ports.receive EditorSent EditorMsgError
                    ]
        }


type alias Model =
    -- Instead of storing the source in our model,
    -- we're storing `Mark.Parsed`,
    -- which is a data structure representing the document
    { parsed : Maybe Mark.Parsed
    , errors : List Mark.Error
    , cursor : Maybe Ports.Cursor
    }


type Msg
    = GotSrc (Result Http.Error String)
    | EditorMsgError String
    | EditorSent Ports.Incoming


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditorMsgError str ->
            let
                _ =
                    Debug.log "error incoming" str
            in
            ( model, Cmd.none )

        EditorSent incoming ->
            case incoming of
                Ports.Select cursor ->
                    ( { model | cursor = Just cursor }
                    , Cmd.none
                    )

        GotSrc result ->
            case result of
                Ok src ->
                    case Mark.parse document src of
                        Mark.Success parsed ->
                            ( { model | parsed = Just parsed }
                            , Cmd.none
                            )

                        Mark.Almost partial ->
                            -- We almost made it.
                            -- This happens when there was an error,
                            -- but `Mark.onError` made it so the document is still renderable
                            -- In this case, we have both the rendered document,
                            -- and the error that occurred
                            ( { model
                                | parsed = Just partial.result
                                , errors = partial.errors
                              }
                            , Cmd.none
                            )

                        Mark.Failure errors ->
                            ( { model | errors = errors }
                            , Cmd.none
                            )

                Err err ->
                    let
                        _ =
                            Debug.log "err" err
                    in
                    ( model, Cmd.none )


view model =
    { title = "Elm Markup Editor"
    , body =
        [ Maybe.map viewCursor model.cursor
            |> Maybe.withDefault (Html.text "")
        , case model.parsed of
            Nothing ->
                Html.text "Source not received yet"

            Just source ->
                case Mark.render document source of
                    Mark.Success html ->
                        Html.div [] html.body

                    Mark.Almost { result, errors } ->
                        -- This is the case where there has been an error,
                        -- but it has been caught by `Mark.onError` and is still rendereable.
                        Html.div []
                            [ Html.div [] (viewErrors errors)
                            , Html.div [] result.body
                            ]

                    Mark.Failure errors ->
                        Html.div []
                            (viewErrors errors)
        ]
    }


viewCursor cursor =
    case cursor of
        Ports.Cursor curs ->
            Html.div
                [ Attr.id "cursor"
                , Attr.style "height" (String.fromFloat curs.box.height ++ "px")
                , Attr.style "left" (String.fromFloat (curs.box.x - 2) ++ "px")
                , Attr.style "top" (String.fromFloat curs.box.y ++ "px")
                , Attr.style "pointer-events" "none"
                ]
                []

        Ports.Range sel ->
            -- A selection cursor is alrady rendered by the browser
            Html.text ""


viewErrors errors =
    List.map
        (Mark.Error.toHtml Mark.Error.Light)
        errors



{- Markup Document -}


document =
    Mark.documentWith
        (\meta body ->
            { metadata = meta
            , body =
                Html.h1 [] meta.title
                    :: body
            }
        )
        -- We have some required metadata that starts our document.
        { metadata = metadata
        , body =
            Mark.manyOf
                [ header
                , image
                , list
                , code
                , Mark.map (Html.p []) text
                ]
        }



{- Handle Text -}


text =
    Mark.textWith
        { view =
            \styles string ->
                viewText styles string
        , replacements = Mark.commonReplacements
        , inlines =
            [ viewLink
            , droppedCapital
            ]
        }


viewText styles string =
    if styles.bold || styles.italic || styles.strike then
        Html.span
            [ Attr.classList
                [ ( "bold", styles.bold )
                , ( "italic", styles.italic )
                , ( "strike", styles.strike )
                ]
            ]
            [ Html.text string ]

    else
        Html.text string


viewLink =
    Mark.annotation "link"
        (\texts url ->
            Html.a [ Attr.href url ]
                (List.map (applyTuple viewText) texts)
        )
        |> Mark.field "url" Mark.string


applyTuple fn ( one, two ) =
    fn one two


droppedCapital =
    Mark.verbatim "drop"
        (\str ->
            let
                drop =
                    String.left 1 str

                lede =
                    String.dropLeft 1 str
            in
            Html.span []
                [ Html.span [ Attr.class "drop-capital" ]
                    [ Html.text drop ]
                , Html.span [ Attr.class "lede" ]
                    [ Html.text lede ]
                ]
        )



{- Metadata -}


metadata =
    Mark.record "Article"
        (\author description title ->
            { author = author
            , description = description
            , title = title
            }
        )
        |> Mark.field "author" Mark.string
        |> Mark.field "description" text
        |> Mark.field "title" text
        |> Mark.toBlock



{- Common Blocks -}


header =
    Mark.block "H1"
        (\children ->
            Html.h1 []
                children
        )
        text


image =
    Mark.record "Image"
        (\src description ->
            Html.img
                [ Attr.src src
                , Attr.alt description
                , Attr.style "float" "left"
                , Attr.style "margin-right" "48px"
                ]
                []
        )
        |> Mark.field "src" Mark.string
        |> Mark.field "description" Mark.string
        |> Mark.toBlock


code =
    Mark.block "Code"
        (\str ->
            Html.pre
                [ Attr.style "padding" "12px"
                , Attr.style "background-color" "#eee"
                ]
                [ Html.text str ]
        )
        Mark.string



{- Handling bulleted and numbered lists -}


list : Mark.Block (Html msg)
list =
    Mark.tree "List" renderList (Mark.map (Html.div []) text)


{-| Note: we have to define this as a separate function because
`Enumerated` and `Item` are a pair of mutually recursive data structures.
It's easiest to render them using two separate functions: renderList and renderItem
-}
renderList : Mark.Enumerated (Html msg) -> Html msg
renderList (Mark.Enumerated enum) =
    let
        group =
            case enum.icon of
                Mark.Bullet ->
                    Html.ul

                Mark.Number ->
                    Html.ol
    in
    group []
        (List.map renderItem enum.items)


renderItem : Mark.Item (Html msg) -> Html msg
renderItem (Mark.Item item) =
    Html.li []
        [ Html.div [] item.content
        , renderList item.children
        ]
