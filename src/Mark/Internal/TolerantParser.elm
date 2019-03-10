module Mark.Internal.TolerantParser exposing
    ( Parser
    , Token, token, keyword, symbol
    , succeed, ignore, keep
    )

{-|

@docs Parser

@docs Token, token, keyword, symbol

@docs succeed, ignore, keep

-}

import Parser.Advanced as Parser exposing ((|.), (|=))


type alias Parser context problem data =
    Parser.Parser context problem (Result (List problem) data)


{-| -}
succeed : data -> Parser context problem data
succeed x =
    Parser.succeed (Ok x)


{-| -}
keep : Parser c p (a -> b) -> Parser c p a -> Parser c p b
keep fnParser newDataParser =
    (|=)
        (Parser.map
            (\existing new ->
                case ( existing, new ) of
                    ( Ok fn, Ok d ) ->
                        Ok (fn d)

                    ( Err err1, Err err2 ) ->
                        Err (err1 ++ err2)

                    ( Err err1, _ ) ->
                        Err err1

                    ( _, Err err2 ) ->
                        Err err2
            )
            fnParser
        )
        newDataParser


{-| -}
ignore : Parser c p keep -> Parser c p ignore -> Parser c p keep
ignore fnParser newDataParser =
    (|=)
        (Parser.map
            (\existing new ->
                case ( existing, new ) of
                    ( Ok fn, Ok _ ) ->
                        Ok fn

                    ( Err err1, Err err2 ) ->
                        Err (err1 ++ err2)

                    ( Err err1, _ ) ->
                        Err err1

                    ( _, Err err2 ) ->
                        Err err2
            )
            fnParser
        )
        newDataParser



{- Basic Tokens -}


type alias Token problem =
    { string : String
    , problem : problem
    , skipTo : List Char
    }


{-| -}
token : Token problem -> Parser context problem ()
token details =
    Parser.oneOf
        [ Parser.map Ok (Parser.token (Parser.Token details.string details.problem))
        , Parser.succeed (Err [ details.problem ])
            |. till details.skipTo details.problem
        ]


{-| -}
keyword : Token problem -> Parser context problem ()
keyword details =
    Parser.oneOf
        [ Parser.map Ok (Parser.keyword (Parser.Token details.string details.problem))
        , Parser.succeed (Err [ details.problem ])
            |. till details.skipTo details.problem
        ]


{-| -}
symbol : Token problem -> Parser context problem ()
symbol details =
    Parser.oneOf
        [ Parser.map Ok (Parser.symbol (Parser.Token details.string details.problem))
        , Parser.succeed (Err [ details.problem ])
            |. till details.skipTo details.problem
        ]


till : List Char -> problem -> Parser.Parser context problem ()
till chars prob =
    Parser.succeed ()
        |. Parser.chompWhile (\c -> not <| List.member c chars)
        |. Parser.oneOf
            [ Parser.map (always True) (Parser.chompIf (\c -> List.member c chars) prob)
            , Parser.succeed False
            ]
