module Mark exposing
    ( parse, parseWith
    , Options, default
    )

{-|

@docs parse, parseWith

@docs Options, default

@docs Index, ListIcon, defaultListIcon

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region
import Html.Attributes
import Internal.Model as Internal
import Mark.Custom as Custom
import Mark.Default
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


type alias Context =
    Internal.Context


type alias Problem =
    Internal.Problem


{-| -}
parse : String -> Result (List (Parser.DeadEnd Context Problem)) (Element msg)
parse source =
    parseWith default source
        |> Result.map (\x -> x ())


{-| -}
parseWith :
    Options model msg
    -> String
    -> Result (List (Parser.DeadEnd Context Problem)) (model -> Element msg)
parseWith options source =
    Parser.run (Internal.markup options) source


{-| -}
type alias Options model msg =
    { blocks : List (Custom.Block model msg)
    , inlines : List (Custom.Inline model msg)
    }


{-| A default set of block and inline elements as well as some `defaultStyling` to style them.
-}
default : Options model msg
default =
    { blocks =
        Mark.Default.blocks
    , inlines =
        []
    }
