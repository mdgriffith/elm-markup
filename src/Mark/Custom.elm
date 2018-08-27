module Mark.Custom
    exposing
        ( Custom
        , block
        , bool
        , done
        , float
        , int
        , parser
        , string
        , styled
        )

{-|

@docs Custom, block

@docs bool, int, float, string

@docs done

@docs parser

-}

import Element exposing (Element)
import Internal.Model as Internal
import Parser exposing ((|.), (|=), Parser)


type alias Block style msg =
    Internal.Block style msg


{-| -}
type Custom block
    = Custom String (Parser block)


{-| -}
block : String -> block -> Custom block
block name renderer =
    Custom name
        (Parser.succeed renderer)


{-| Either parse a double quote and wait for another double quote
Or parse
-}
string : Custom (String -> other) -> Custom other
string =
    onArgument
        (\renderer ->
            renderer
                |. Parser.chompWhile (\c -> c == ' ' || c == '\n')
                |= Parser.oneOf
                    [ Parser.succeed identity
                        |. Parser.token "\""
                        |= Parser.getChompedString
                            (Parser.chompWhile (\c -> c /= doubleQuote))
                        |. Parser.token "\""
                    , Parser.getChompedString
                        (Parser.chompWhile (\c -> c /= '\n'))
                    ]
        )


onArgument : (Parser block -> Parser other) -> Custom block -> Custom other
onArgument fn blockable =
    case blockable of
        Custom name arg ->
            Custom name (fn arg)


{-| -}
int : Custom (Int -> other) -> Custom other
int =
    onArgument
        (\renderer ->
            renderer
                |. Parser.chompIf (\c -> c == ' ')
                |= Parser.int
        )


{-| -}
bool : Custom (Bool -> other) -> Custom other
bool =
    onArgument
        (\renderer ->
            renderer
                |. Parser.chompIf (\c -> c == ' ')
                |= Parser.oneOf
                    [ Parser.token "True"
                        |> Parser.map (always True)
                    , Parser.token "False"
                        |> Parser.map (always False)
                    ]
        )


{-| -}
float : Custom (Float -> other) -> Custom other
float =
    onArgument
        (\renderer ->
            renderer
                |. Parser.chompIf (\c -> c == ' ')
                |= Parser.float
        )


{-| -}
done : Custom (style -> Element msg) -> Internal.Block style msg
done (Custom name actualParser) =
    Internal.Block name
        actualParser


{-| -}
styled :
    Custom
        (List (Element msg)
         ->
            { a
                | link : List (Element.Attribute msg)
                , token : List (Element.Attribute msg)
                , blockSpacing : Int
            }
         -> Element msg
        )
    ->
        Internal.Block
            { a
                | link : List (Element.Attribute msg)
                , token : List (Element.Attribute msg)
                , blockSpacing : Int
            }
            msg
styled (Custom name customParser) =
    Internal.Parse name
        (\options ->
            customParser
                |. Parser.chompWhile (\c -> c == '\n')
                |= Internal.text options
        )


{-| -}
parser : String -> (Internal.Options style msg -> Parser (style -> Element msg)) -> Internal.Block style msg
parser name actualParser =
    Internal.Parse name
        (\options ->
            Parser.succeed identity
                |. Parser.chompWhile (\c -> c == '\n')
                |= actualParser options
        )


doubleQuote =
    '"'
