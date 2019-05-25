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
    = NoOp


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
                -- "Selection" ->
                --     case Decode.decodeValue decodeCursor payload.data of
                --         Ok cursor ->
                --             onSuccess (Select cursor)
                --         Err e ->
                --             onError (Decode.errorToString e)
                "CharLayout" ->
                    case Decode.decodeValue Selection.decode payload.data of
                        Ok cursor ->
                            onSuccess (NewCharLayout cursor)

                        Err e ->
                            onError (Decode.errorToString e)

                _ ->
                    onError ""
        )
