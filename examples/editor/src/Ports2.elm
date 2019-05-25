port module Ports2 exposing
    ( Cursor(..)
    , Incoming(..)
    , Outgoing(..)
    , Selection
    , receive
    , send
    )

{-| -}

import Json.Decode as Decode
import Json.Encode as Encode
import Mark
import Mark.Edit


port infoForElm : (Payload -> msg) -> Sub msg


port infoForWorld : Payload -> Cmd msg


send : Outgoing -> Cmd msg
send out =
    case out of
        NoOp ->
            infoForWorld
                { tag = "NoOp"
                , data = Encode.bool True
                }


receive : (Incoming -> msg) -> (String -> msg) -> Sub msg
receive onSuccess onError =
    infoForElm
        (\payload ->
            case payload.tag of
                "Selection" ->
                    case Decode.decodeValue decodeCursor payload.data of
                        Ok cursor ->
                            onSuccess (Select cursor)

                        Err e ->
                            onError (Decode.errorToString e)

                _ ->
                    onError ""
        )


type alias Payload =
    { tag : String
    , data : Encode.Value
    }


type Incoming
    = Select Cursor


type Outgoing
    = NoOp


type alias Offset =
    Int


type Cursor
    = Cursor CursorDetails
    | Range Selection


decodeCursor =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "Cursor" ->
                        Decode.field "data"
                            (Decode.map Cursor decodeCursorDetails)

                    "Range" ->
                        Decode.field "data"
                            (Decode.map Range decodeSelection)

                    _ ->
                        Decode.fail "Unexpected cursor"
            )


type alias CursorDetails =
    { box : BoundingBox
    , id : Mark.Edit.Id
    , offset : Offset
    }


type alias Selection =
    { box : BoundingBox
    , startId : Mark.Edit.Id
    , startOffset : Offset
    , endId : Mark.Edit.Id
    , endOffset : Offset
    }


decodeCursorDetails : Decode.Decoder CursorDetails
decodeCursorDetails =
    Decode.map3 CursorDetails
        (Decode.field "box" decodeBoundingBox)
        (Decode.field "id" decodeId)
        (Decode.field "offset" decodeOffset)


decodeSelection : Decode.Decoder Selection
decodeSelection =
    Decode.map5 Selection
        (Decode.field "box" decodeBoundingBox)
        (Decode.field "startId" decodeId)
        (Decode.field "startOffset" decodeOffset)
        (Decode.field "endId" decodeId)
        (Decode.field "endOffset" decodeOffset)


decodeId : Decode.Decoder Mark.Edit.Id
decodeId =
    Decode.string
        |> Decode.andThen
            (\str ->
                case Mark.stringToId str of
                    Nothing ->
                        Decode.fail "Invalid Id"

                    Just id ->
                        Decode.succeed id
            )


decodeOffset : Decode.Decoder Offset
decodeOffset =
    Decode.int


{-| x,y are document absolute.
-}
type alias BoundingBox =
    { x : Float
    , y : Float
    , height : Float
    , width : Float
    }


decodeBoundingBox : Decode.Decoder BoundingBox
decodeBoundingBox =
    Decode.map4
        BoundingBox
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)
        (Decode.field "height" Decode.float)
        (Decode.field "width" Decode.float)
