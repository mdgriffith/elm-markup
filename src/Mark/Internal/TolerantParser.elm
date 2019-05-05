module Mark.Internal.TolerantParser exposing
    ( Parser, try
    , Token, token, keyword, symbol, OnError(..), skip, fastForwardTo, stopWith
    , succeed, ignore, keep
    , oneOf
    , chompWhile
    , map
    )

{-|

@docs Parser, try

@docs Token, token, keyword, symbol, OnError, skip, fastForwardTo, stopWith

@docs succeed, ignore, keep

@docs oneOf

@docs chompWhile

@docs map

-}

import Parser.Advanced as Parser exposing ((|.), (|=))


type alias Parser context problem data =
    Parser.Parser context problem (Result (List problem) data)


{-| -}
map : (a -> b) -> Parser c p a -> Parser c p b
map =
    Parser.map << Result.map



{- ONEOF -}


{-| A tolerant `oneOf` means it has to succeed.

So, you provide an error to return if everything else fails.

-}
oneOf : problem -> List (Parser context problem data) -> Parser context problem data
oneOf prob options =
    Parser.oneOf
        (options ++ [ Parser.succeed (Err [ prob ]) ])



{- PIPELINES -}


{-| -}
succeed : data -> Parser context problem data
succeed x =
    Parser.succeed (Ok x)


{-| This might be a little odd, but it essentially means the parser its attached to should run normally, meaning it's ok if it fails.

This is generally useful with a `oneOf`, to ensure that an option can fail in the normal way and skip to try the next option.

-}
try : Parser.Parser c p d -> Parser c p d
try =
    Parser.map Ok


{-| Almost like `|=`, except
-}
keep : Parser c p a -> Parser c p (a -> b) -> Parser c p b
keep newDataParser fnParser =
    fnParser
        |> Parser.andThen
            (\existing ->
                case existing of
                    Err err ->
                        -- We already had an error and already fast-forwarded
                        Parser.succeed (Err err)

                    Ok fn ->
                        Parser.map
                            (\possiblyNew ->
                                case possiblyNew of
                                    Ok new ->
                                        Ok (fn new)

                                    Err newErr ->
                                        Err newErr
                            )
                            newDataParser
            )


{-| -}
ignore : Parser c p ignore -> Parser c p keep -> Parser c p keep
ignore ignorePls keepPls =
    keepPls
        |> Parser.andThen
            (\possiblyKeepThisOne ->
                case possiblyKeepThisOne of
                    Err err ->
                        -- We already had an error and already fast-forwarded
                        Parser.succeed (Err err)

                    Ok keepThisOne ->
                        Parser.map
                            (\possibly ->
                                case possibly of
                                    Ok _ ->
                                        Ok keepThisOne

                                    Err newErr ->
                                        Err newErr
                            )
                            ignorePls
            )



{- Basic Tokens -}


type OnError error
    = FastForwardTo (List Char)
    | Skip
    | StopWith error


fastForwardTo =
    FastForwardTo


skip =
    Skip


stopWith err =
    StopWith err


type alias Token problem =
    { match : String
    , problem : problem
    , onError : OnError problem
    }


{-| -}
token : Token problem -> Parser context problem ()
token details =
    runToken details (Parser.token (Parser.Token details.match details.problem))


{-| -}
keyword : Token problem -> Parser context problem ()
keyword details =
    runToken details (Parser.keyword (Parser.Token details.match details.problem))


{-| -}
symbol : Token problem -> Parser context problem ()
symbol details =
    runToken details (Parser.symbol (Parser.Token details.match details.problem))


runToken details tokenParser =
    case details.onError of
        FastForwardTo skipTo ->
            Parser.oneOf
                [ Parser.map Ok tokenParser
                , Parser.succeed (Err [ details.problem ])
                    |. till skipTo details.problem
                ]

        Skip ->
            Parser.map Ok tokenParser

        StopWith err ->
            Parser.oneOf
                [ Parser.map Ok tokenParser
                , Parser.succeed (Err [ err ])
                ]


till : List Char -> problem -> Parser.Parser context problem ()
till chars prob =
    Parser.succeed ()
        |. Parser.chompWhile (\c -> not <| List.member c chars)
        |. Parser.oneOf
            [ Parser.map (always True) (Parser.chompIf (\c -> List.member c chars) prob)
            , Parser.succeed False
            ]


chompWhile while =
    Parser.succeed okUnit
        |. Parser.chompWhile while


okUnit =
    Ok ()
