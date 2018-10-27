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
import Parser exposing ((|.), (|=), Parser)


{-| -}
parse : String -> Result (List Parser.DeadEnd) (Element msg)
parse source =
    parseWith default source
        |> Result.map (\x -> x ())


{-| -}
parseWith :
    Options model
        { a
            | link : List (Element.Attribute msg)
            , token : List (Element.Attribute msg)
            , list : List Mark.Default.Index -> List (Element.Attribute msg)
            , root : List (Element.Attribute msg)
            , block : List (Element.Attribute msg)
        }
        msg
    -> String
    -> Result (List Parser.DeadEnd) (model -> Element msg)
parseWith options source =
    Parser.run (Internal.markup options) source


{-| -}
type alias Options model styling msg =
    { styling : model -> styling
    , blocks : List (Custom.Block model styling msg)
    , inlines : List (Custom.Inline model styling msg)
    }


{-| A default set of block and inline elements as well as some `defaultStyling` to style them.
-}
default : Options model (Mark.Default.Styling msg) msg
default =
    { styling =
        always Mark.Default.styling
    , blocks =
        Mark.Default.blocks
    , inlines =
        []
    }
