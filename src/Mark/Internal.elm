module Mark.Internal exposing (Parsed(..))

{-| -}


{-| -}
type Parsed
    = Parsed
        { errors : List ErrorMessage
        , found : Found Description
        , expected : Expectation
        , focus : Maybe Position
        }
