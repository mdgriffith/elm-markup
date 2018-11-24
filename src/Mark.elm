module Mark exposing
    ( parse, parseWith
    , Context, Problem
    )

{-|

@docs parse, parseWith

@docs Options

@docs Index, ListIcon, defaultListIcon

@docs Context, Problem

-}

import Element exposing (Element)
import Mark.Custom
import Mark.Default
import Parser.Advanced as Parser exposing (Parser)


{-| -}
type alias Context =
    Mark.Custom.Context


{-| -}
type alias Problem =
    Mark.Custom.Problem


{-| -}
parse : String -> Result (List (Parser.DeadEnd Context Problem)) (Element msg)
parse source =
    parseWith Mark.Default.document source
        |> Result.map (\x -> x ())


{-| -}
parseWith :
    Mark.Custom.Root result
    -> String
    -> Result (List (Parser.DeadEnd Context Problem)) result
parseWith options source =
    Mark.Custom.parse options source
