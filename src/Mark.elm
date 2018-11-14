module Mark exposing
    ( parse, parseWith
    , Options, InlineOptions
    , Context, Problem
    )

{-|

@docs parse, parseWith

@docs Options, InlineOptions, default

@docs Index, ListIcon, defaultListIcon

@docs Context, Problem

-}

import Element exposing (Element)
import Mark.Custom as Custom
import Mark.Default
import Parser.Advanced as Parser exposing (Parser)


{-| -}
type alias Context =
    Custom.Context


{-| -}
type alias Problem =
    Custom.Problem


{-| -}
parse : String -> Result (List (Parser.DeadEnd Context Problem)) (Element msg)
parse source =
    parseWith Mark.Default.default source
        |> Result.map (\x -> x ())


{-| -}
parseWith :
    Options result
    -> String
    -> Result (List (Parser.DeadEnd Context Problem)) result
parseWith options source =
    Custom.parse options source


{-| -}
type alias Options result =
    Custom.Options result


{-| -}
type alias InlineOptions result =
    Custom.InlineOptions result
