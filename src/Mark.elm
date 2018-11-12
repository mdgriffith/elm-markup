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
import Parser.Advanced as Parser exposing (Parser)


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
    Options result
    -> String
    -> Result (List (Parser.DeadEnd Context Problem)) result
parseWith options source =
    Parser.run (Internal.markup options) source


{-| A default set of block and inline elements as well as some `defaultStyling` to style them.
-}
default : Options (model -> Element msg)
default =
    { blocks =
        Mark.Default.blocks
    , merge = \els model -> Element.textColumn [] (List.map (\el -> el model) els)
    , inlines =
        { view = Mark.Default.inline

        -- TextFormatting -> String -> result
        , inlines = []
        , merge = \els model -> Element.paragraph [] (List.map (\el -> el model) els)
        , replacements =
            [ Internal.Replacement "..." "…"
            , Internal.Replacement "<>" "\u{00A0}"
            , Internal.Replacement "---" "—"
            , Internal.Replacement "--" "–"
            , Internal.Replacement "'" "’"
            , Internal.Balanced
                { start = ( "\"", "“" )
                , end = ( "\"", "”" )
                }
            ]
        }
    }


{-| -}
type alias Options result =
    Internal.Options result


type alias InlineOptions result =
    Internal.InlineOptions result
