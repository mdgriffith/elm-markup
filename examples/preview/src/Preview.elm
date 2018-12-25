module Preview exposing (program)

{-|

@docs program

-}

import Browser
import Browser.Navigation as Nav
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Http
import Mark
import Mark.Default
import Url
import Url.Parser as Parser exposing ((</>), (<?>), Parser, custom, fragment, map, oneOf, s, top)
import Url.Parser.Query as Query


{-| -}
program : Mark.Document doc -> (doc -> Html Msg) -> Program () (Previewer doc) Msg
program doc viewer =
    Browser.application
        { init = init doc viewer
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Previewer doc =
    { key : Nav.Key
    , page : Page
    , document : Mark.Document doc
    , text : Maybe String
    , viewer : doc -> Html Msg
    }


init : Mark.Document doc -> (doc -> Html Msg) -> () -> Url.Url -> Nav.Key -> ( Previewer doc, Cmd Msg )
init doc viewer flags url key =
    stepUrl url
        { key = key
        , page = NotFound
        , document = doc
        , text = Nothing
        , viewer = viewer
        }


type Page
    = Main
    | Preview String
    | NotFound


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotText (Result Http.Error String)


update : Msg -> Previewer doc -> ( Previewer doc, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            stepUrl url model

        GotText result ->
            case result of
                Ok txt ->
                    ( { model | text = Just txt }
                    , Cmd.none
                    )

                Err httpErr ->
                    ( model
                    , Cmd.none
                    )


stepUrl : Url.Url -> Previewer doc -> ( Previewer doc, Cmd Msg )
stepUrl url model =
    let
        parser =
            oneOf
                [ Parser.map Main top
                ]
    in
    case Parser.parse parser url of
        Just page ->
            ( { model | page = page }
            , Cmd.none
            )

        Nothing ->
            if String.startsWith "/preview" url.path then
                let
                    fileUrl =
                        String.dropLeft 8 url.path ++ ".emu"
                in
                ( { model | page = Preview fileUrl }
                , Http.get
                    { url = fileUrl
                    , expect = Http.expectString GotText
                    }
                )

            else
                ( { model | page = NotFound }
                , Cmd.none
                )



-- else
--     ( model, Cmd.none )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


view model =
    { title = "Elm Markup Preview"
    , body =
        [ case model.text of
            Nothing ->
                Html.text "No document to show!"

            Just txt ->
                case Mark.parse model.document txt of
                    Ok element ->
                        model.viewer element

                    Err errors ->
                        Html.div []
                            (List.map viewError errors)
        ]
    }


viewError error =
    Html.div []
        [ Html.div [] [ Html.text ("row: " ++ String.fromInt error.row) ]
        , Html.div [] [ Html.text ("col: " ++ String.fromInt error.col) ]
        , Html.div []
            [ Html.text "Uhhh, some problem" ]
        ]
