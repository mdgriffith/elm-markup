module Mark.Internal.Outcome exposing (Outcome(..))

{-|

@docs Outcome

-}


{-| -}
type Outcome failure almost success
    = Success success
    | Almost almost
    | Failure failure
