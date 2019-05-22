port module Ports exposing
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


type alias Id =
    String


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
    , id : Id
    , offset : Offset
    }


type alias Selection =
    { box : BoundingBox
    , startId : Id
    , startOffset : Offset
    , endId : Id
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


decodeId : Decode.Decoder Id
decodeId =
    Decode.string


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
