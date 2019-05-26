port module Ports exposing
    ( Incoming(..)
    , Outgoing(..)
    , receive
    , send
    )

{-| -}

import Json.Decode as Decode
import Json.Encode as Encode
import Mark
import Mark.Edit
import Selection


port infoForElm : (Payload -> msg) -> Sub msg


port infoForWorld : Payload -> Cmd msg


type alias Payload =
    { tag : String
    , data : Encode.Value
    }


type Incoming
    = NewCharLayout Selection.CharLayout


type Outgoing
    = Rescan


send : Outgoing -> Cmd msg
send out =
    case out of
        Rescan ->
            infoForWorld
                { tag = "Rescan"
                , data = Encode.bool True
                }


receive : (Incoming -> msg) -> (String -> msg) -> Sub msg
receive onSuccess onError =
    infoForElm
        (\payload ->
            case payload.tag of
                "CharLayout" ->
                    case Decode.decodeValue Selection.decode payload.data of
                        Ok cursor ->
                            onSuccess (NewCharLayout cursor)

                        Err e ->
                            onError (Decode.errorToString e)

                _ ->
                    onError ""
        )
