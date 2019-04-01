module Mark.Internal.Outcome exposing
    ( Outcome(..)
    , mapSuccess
    )

{-|

@docs Outcome

-}


{-| -}
type Outcome failure almost success
    = Success success
    | Almost almost
    | Failure failure


mapSuccess : (success -> otherSuccess) -> Outcome f a success -> Outcome f a otherSuccess
mapSuccess fn outcome =
    case outcome of
        Success s ->
            Success (fn s)

        Almost a ->
            Almost a

        Failure f ->
            Failure f
