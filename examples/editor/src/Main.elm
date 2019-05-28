module Main exposing (document, main)

{-| -}

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed
import Html.Lazy
import Http
import Json.Decode as Decode
import Mark
import Mark.Edit
import Mark.Error
import Mark.New
import Ports
import Selection
import Time


main =
    Browser.element
        { init =
            \() ->
                ( { parsed = Nothing
                  , errors = []
                  , cursor = Nothing
                  , characterLayout = Nothing
                  , selecting = Nothing
                  }
                , Http.get
                    { url = "/articles/Ipsum.emu"
                    , expect = Http.expectString GotSrc
                    }
                )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ Ports.receive EditorSent EditorMsgError
                    , if model.selecting /= Nothing then
                        Sub.batch
                            [ Browser.Events.onMouseMove (Decode.map SelectTo decodeCoords)
                            , Browser.Events.onMouseUp (Decode.succeed StopSelection)
                            , Browser.Events.onVisibilityChange (always StopSelection)
                            ]

                      else
                        Sub.none

                    -- We're disabling global events because they seem to override /everything/
                    -- , Browser.Events.onMouseDown (Decode.map SelectTo decodeCoords)
                    -- , Browser.Events.onKeyPress (Decode.map KeyPressed keyDecoder)
                    -- , Browser.Events.onKeyDown (Decode.map KeyPressed controlDecoder)
                    ]
        }


type alias Model =
    -- Instead of storing the source in our model,
    -- we're storing `Mark.Parsed`,
    -- which is a data structure representing the document
    { parsed : Maybe Mark.Parsed
    , errors : List Mark.Error.Error
    , cursor : Maybe Cursor
    , characterLayout : Maybe Selection.CharLayout

    -- If a selection has been started, this is the starting coord.
    , selecting : Maybe ( Float, Float )
    }


type Cursor
    = Caret Selection.CharBox
    | Range Selection.CharBox (List Selection.CharBox) Selection.CharBox


type Msg
    = GotSrc (Result Http.Error String)
    | EditorMsgError String
    | EditorSent Ports.Incoming
    | KeyPressed Key
    | SelectTo ( Float, Float )
    | StopSelection
    | ClearSelection
    | StyleSelection Style


type Style
    = Bold
    | Italic
    | Strike
    | Normal


type Key
    = Character Char
    | Control String
    | Enter
    | Space
    | Delete
    | Arrow Direction


type Direction
    = Up
    | Right
    | Down
    | Left


{-| We dont use Browser events because we need to prevent defaults.

Specifically for spacebar, but likely for others as well.

-}
editEvents =
    [ Attr.tabindex 1
    , Events.preventDefaultOn "keypress" (Decode.map (\key -> ( KeyPressed key, True )) keyDecoder)
    , Events.preventDefaultOn "keydown" (Decode.map (\key -> ( KeyPressed key, True )) controlDecoder)
    , Events.on "mousedown" (Decode.map SelectTo decodeCoords)
    ]


decodeCoords =
    Decode.map2 Tuple.pair
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)


controlDecoder : Decode.Decoder Key
controlDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Backspace" ->
                        Decode.succeed Delete

                    "ArrowDown" ->
                        Decode.succeed (Arrow Down)

                    "ArrowUp" ->
                        Decode.succeed (Arrow Up)

                    "ArrowRight" ->
                        Decode.succeed (Arrow Right)

                    "ArrowLeft" ->
                        Decode.succeed (Arrow Left)

                    _ ->
                        Decode.fail "Unknown"
            )


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Key
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            -- https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key/Key_Values
            case string of
                "Enter" ->
                    Enter

                " " ->
                    Space

                _ ->
                    Control string


syncCursor layout cursor =
    case cursor of
        Caret char ->
            Caret (Selection.resync layout char)

        Range start middle end ->
            Range
                (Selection.resync layout start)
                (List.map (Selection.resync layout) middle)
                (Selection.resync layout end)


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
                Ports.NewCharLayout layout ->
                    ( { model
                        | characterLayout = Just layout
                        , cursor = Maybe.map (syncCursor layout) model.cursor
                      }
                    , Cmd.none
                    )

        SelectTo coords ->
            case model.characterLayout of
                Nothing ->
                    ( model, Cmd.none )

                Just layout ->
                    case model.selecting of
                        Just base ->
                            case Selection.selectMany base coords layout of
                                Nothing ->
                                    ( model, Cmd.none )

                                Just (Selection.Single caret) ->
                                    ( { model
                                        | selecting = Just coords
                                        , cursor = Just (Caret caret)
                                      }
                                    , Cmd.none
                                    )

                                Just (Selection.Many start middle end) ->
                                    ( { model
                                        | cursor = Just (Range start middle end)
                                      }
                                    , Cmd.none
                                    )

                        Nothing ->
                            case Selection.select coords layout of
                                Just caret ->
                                    ( { model
                                        | selecting = Just coords
                                        , cursor = Just (Caret caret)
                                      }
                                    , Cmd.none
                                    )

                                Nothing ->
                                    ( model
                                    , Cmd.none
                                    )

        StopSelection ->
            ( { model | selecting = Nothing }
            , Cmd.none
            )

        ClearSelection ->
            ( { model
                | selecting = Nothing
                , cursor = Nothing
              }
            , Cmd.none
            )

        StyleSelection style ->
            case ( model.parsed, model.cursor ) of
                ( Just parsed, Just (Range start middle end) ) ->
                    let
                        updatedDocument =
                            Mark.Edit.update document
                                (case style of
                                    Normal ->
                                        Mark.Edit.restyle
                                            start.id
                                            start.offset
                                            end.offset
                                            { bold = False
                                            , italic = False
                                            , strike = False
                                            }

                                    _ ->
                                        Mark.Edit.addStyles
                                            start.id
                                            start.offset
                                            end.offset
                                            { bold = style == Bold
                                            , italic = style == Italic
                                            , strike = style == Strike
                                            }
                                )
                                parsed
                    in
                    case updatedDocument of
                        Err errors ->
                            let
                                _ =
                                    Debug.log "err" errors
                            in
                            ( model, Cmd.none )

                        Ok newDoc ->
                            ( { model
                                | parsed = Just newDoc

                                -- , renderCmds = Just Rescan
                              }
                            , Ports.send Ports.Rescan
                            )

                _ ->
                    -- no parsed document or cursor, then edits dont make sense
                    ( model, Cmd.none )

        KeyPressed key ->
            case ( model.parsed, model.cursor, model.characterLayout ) of
                ( Just parsed, Just cursor, Just charLayout ) ->
                    case updateDocument charLayout parsed cursor key of
                        Err errors ->
                            let
                                _ =
                                    Debug.log "err" errors
                            in
                            ( model, Cmd.none )

                        Ok ( newCursor, newDoc ) ->
                            ( { model
                                | parsed = Just newDoc
                                , cursor = Just newCursor
                              }
                            , Ports.send Ports.Rescan
                            )

                _ ->
                    -- no parsed document or cursor, then edits dont make sense
                    ( model, Cmd.none )

        GotSrc result ->
            case result of
                Ok src ->
                    case Mark.parse document src of
                        Mark.Success parsed ->
                            ( { model
                                | parsed = Just parsed
                              }
                            , Ports.send Ports.Rescan
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
                            , Ports.send Ports.Rescan
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


updateDocument :
    Selection.CharLayout
    -> Mark.Parsed
    -> Cursor
    -> Key
    -> Result (List Mark.Error.Error) ( Cursor, Mark.Parsed )
updateDocument charLayout parsed cursor key =
    case key of
        Character char ->
            case cursor of
                Caret caret ->
                    -- TODO: what if offset is 0?
                    Mark.Edit.update document
                        (Mark.Edit.insertText
                            caret.id
                            caret.offset
                            [ Mark.New.unstyled (String.fromChar char) ]
                        )
                        parsed
                        |> Result.map (Tuple.pair (Caret (Selection.move 1 caret)))

                Range start middle end ->
                    Mark.Edit.update document
                        (Mark.Edit.insertText
                            start.id
                            start.offset
                            [ Mark.New.unstyled (String.fromChar char) ]
                        )
                        parsed
                        |> Result.map (Tuple.pair (Caret (Selection.move 1 start)))

        Delete ->
            case cursor of
                Caret caret ->
                    -- TODO: what if offset is 0?
                    let
                        newCursor =
                            Caret (Selection.move -1 caret)
                    in
                    Mark.Edit.update document
                        (Mark.Edit.deleteText
                            caret.id
                            caret.offset
                            (caret.offset - 1)
                        )
                        parsed
                        |> Result.map (Tuple.pair newCursor)

                Range start middle end ->
                    let
                        newCursor =
                            Caret (Selection.move -1 start)
                    in
                    Mark.Edit.update document
                        (Mark.Edit.deleteText
                            start.id
                            start.offset
                            end.offset
                        )
                        parsed
                        |> Result.map (Tuple.pair newCursor)

        Arrow dir ->
            let
                collapsed =
                    case cursor of
                        Caret caret ->
                            caret

                        Range start _ _ ->
                            start
            in
            case dir of
                Up ->
                    Ok
                        ( Caret (Selection.moveUp charLayout collapsed)
                        , parsed
                        )

                Down ->
                    Ok
                        ( Caret (Selection.moveDown charLayout collapsed)
                        , parsed
                        )

                Left ->
                    Ok
                        ( Caret (Selection.move -1 collapsed)
                        , parsed
                        )

                Right ->
                    Ok
                        ( Caret (Selection.move 1 collapsed)
                        , parsed
                        )

        Control ctrl ->
            -- These are as yet uncaptured control characters.
            -- We still capture them here incase we want to extend in the future.
            -- Here's what's available:
            -- https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key/Key_Values
            Err []

        Enter ->
            Err []

        Space ->
            Err []


view model =
    Html.div []
        [ Maybe.map viewCursor model.cursor
            |> Maybe.withDefault (Html.text "")
        , Html.Lazy.lazy viewDocument model.parsed
        ]


viewDocument parsed =
    case parsed of
        Nothing ->
            Html.text "Source not received yet"

        Just source ->
            case Mark.render document source of
                Mark.Success html ->
                    Html.div (Attr.id "root" :: editEvents) html.body

                Mark.Almost { result, errors } ->
                    -- This is the case where there has been an error,
                    -- but it has been caught by `Mark.onError` and is still rendereable.
                    Html.div []
                        [ Html.div [] (viewErrors errors)
                        , Html.div (Attr.id "root" :: editEvents) result.body
                        ]

                Mark.Failure errors ->
                    Html.div []
                        (viewErrors errors)


viewCursor cursor =
    case cursor of
        Caret curs ->
            Html.div []
                [ viewCharBoxLeft curs.box
                ]

        Range start middle end ->
            Html.div []
                [ viewControlsAbove start.box
                , viewCharBoxLeft start.box
                , Html.div []
                    -- *note* middle is in reversed order
                    (viewHighlightFromBoxes middle)
                , viewCharBoxRight end.box
                ]


viewControlsAbove box =
    Html.div
        [ Attr.id "controls"
        , Attr.style "left" (String.fromInt (floor box.x) ++ "px")
        , Attr.style "top" (String.fromInt (floor (box.y - 50)) ++ "px")
        ]
        [ Html.span [ Events.onClick (StyleSelection Normal) ] [ Html.text "A" ]
        , Html.span [ Attr.class "bold", Events.onClick (StyleSelection Bold) ] [ Html.text "B" ]
        , Html.span [ Attr.class "italic", Events.onClick (StyleSelection Italic) ] [ Html.text "I" ]
        , Html.span [ Attr.class "strike", Events.onClick (StyleSelection Strike) ] [ Html.text "S" ]
        ]


viewHighlightFromBoxes boxes =
    -- It would be super cool and likely more performant
    --- to calculate something like an Svg polyline from all these boxes
    -- List.foldl addBox (boxToPoints start) boxes
    List.map (viewCharBox << .box) boxes


boxToPoints box =
    [ ( box.x, box.y )
    , ( box.x + box.width, box.y )
    , ( box.x + box.width, box.y + box.height )
    , ( box.x, box.y + box.height )
    ]


viewCharBoxLeft box =
    Html.div []
        [ Html.div
            [ Attr.id "cursor"
            , Attr.style "height" (String.fromInt (floor box.height) ++ "px")
            , Attr.style "left" (String.fromInt (floor box.x - 1) ++ "px")
            , Attr.style "top" (String.fromInt (floor box.y) ++ "px")
            , Attr.style "pointer-events" "none"
            ]
            []
        , Html.div
            [ Attr.id "cursor-box"
            , Attr.style "height" (String.fromInt (floor box.height) ++ "px")
            , Attr.style "left" (String.fromInt (floor box.x) ++ "px")
            , Attr.style "width" (String.fromInt (floor box.width) ++ "px")
            , Attr.style "top" (String.fromInt (floor box.y) ++ "px")
            , Attr.style "pointer-events" "none"
            ]
            []
        ]


viewCharBox box =
    Html.div
        [ Attr.id "cursor-box"
        , Attr.style "height" (String.fromInt (floor box.height) ++ "px")
        , Attr.style "left" (String.fromInt (floor box.x) ++ "px")
        , Attr.style "width" (String.fromInt (floor box.width) ++ "px")
        , Attr.style "top" (String.fromInt (floor box.y) ++ "px")
        , Attr.style "pointer-events" "none"
        ]
        []


viewCharBoxRight box =
    Html.div []
        [ Html.div
            [ Attr.id "cursor"
            , Attr.style "height" (String.fromInt (floor box.height) ++ "px")
            , Attr.style "left" (String.fromInt (floor (box.x + box.width - 1)) ++ "px")
            , Attr.style "top" (String.fromInt (floor box.y) ++ "px")
            , Attr.style "pointer-events" "none"
            ]
            []
        , Html.div
            [ Attr.id "cursor-box"
            , Attr.style "height" (String.fromInt (floor box.height) ++ "px")
            , Attr.style "left" (String.fromInt (floor box.x) ++ "px")
            , Attr.style "width" (String.fromInt (floor box.width) ++ "px")
            , Attr.style "top" (String.fromInt (floor box.y) ++ "px")
            , Attr.style "pointer-events" "none"
            ]
            []
        ]


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
                , Mark.withId
                    (\id els ->
                        Html.p [ Attr.id (Mark.idToString id) ] els
                    )
                    text
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
