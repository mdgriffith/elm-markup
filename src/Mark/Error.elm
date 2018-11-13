module Mark.Error exposing (Context(..), Problem(..))

{-|

@docs Context, Problem

-}


type Context
    = InBlock String
    | InInline String


type Problem
    = UnknownBlock
    | NoBlocks
    | UnknownInline
    | EmptyBlock
    | ExpectedIndent
    | InlineStart
    | InlineBar
    | InlineEnd
    | Expecting String
    | Escape
    | EscapedChar
    | ExpectedNonBreaking
    | Dash
    | DoubleQuote
    | Apostrophe
    | Newline
    | DoubleNewline
    | Space
    | End
    | Integer
    | FloatingPoint
    | InvalidNumber
    | ExpectingAlphaNumeric
