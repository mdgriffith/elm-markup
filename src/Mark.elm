module Mark exposing
    ( parse, parseWith
    , Options
    , Context, Problem
    )

{-|

@docs parse, parseWith

@docs Options, default

@docs Index, ListIcon, defaultListIcon

@docs Context, Problem

-}

import Element exposing (Element)
import Internal.Model as Internal
import Mark.Custom as Custom
import Mark.Default
import Mark.Error
import Parser.Advanced as Parser exposing (Parser)


{-| -}
type alias Context =
    Mark.Error.Context


{-| -}
type alias Problem =
    Mark.Error.Problem


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
    Parser.run (Internal.markup options) source


{-| -}
type alias Options result =
    Internal.Options result


type alias InlineOptions result =
    Internal.InlineOptions result
