module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Mark
import Mark.Edit as Edit


type alias Model =
    { parsed : Maybe Mark.Parsed }


document : Mark.Document (Html Msg)
document =
    Mark.document
        (\html -> div [] [ html ])
        (Mark.block "Title"
            (\( id, s ) ->
                h1 [ onClick (Click id) ] [ text s ]
            )
            editableText
        )


editableText =
    Mark.withId Tuple.pair Mark.string


initialModel : Model
initialModel =
    { parsed =
        case Mark.parse document "|> Title \n    some text" of
            Mark.Success parsed ->
                Just parsed

            _ ->
                Nothing
    }


type Msg
    = Click Edit.Id


update : Msg -> Model -> Model
update (Click id) model =
    case model.parsed of
        Just parsed ->
            case Edit.update document (Edit.deleteText id 0 5) parsed of
                Ok newParsed ->
                    { model | parsed = Just newParsed }

                Err errors ->
                    let
                        e =
                            Debug.log "errors" errors
                    in
                    model

        _ ->
            model


view : Model -> Html Msg
view model =
    case model.parsed of
        Nothing ->
            div [] []

        Just parsed ->
            case Mark.render document parsed of
                Mark.Success html ->
                    html

                _ ->
                    div [] []


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
