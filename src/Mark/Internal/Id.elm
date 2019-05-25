module Mark.Internal.Id exposing
    ( Id(..)
    , Seed
    , fromString
    , initialSeed
    , reseed
    , step
    , thread
    , threadThrough
    , toString
    )

{-| -}


initialSeed : Seed
initialSeed =
    Seed [ 0 ]


{-| We might want to move to a list based ID.

So, when we `reseed`, we're actually adding a level.

    Seed [0]

    Seed [1] --reseed -> Seed [1, 0]
                            |
                            v
                         Seed [1, 1]


    Seed [2]

Is there a performance issue with allocating a bunch of lists intead of base Ints?

-}
type Seed
    = Seed (List Int)


toString (Id ids) =
    ids
        |> List.map String.fromInt
        |> String.join "-"
        |> (\x -> "m-" ++ x)


fromString str =
    case String.split "-" str of
        "m" :: remain ->
            let
                parsed =
                    List.filterMap String.toInt remain
            in
            if List.length parsed == List.length remain then
                Just (Id parsed)

            else
                Nothing

        _ ->
            Nothing


{-| -}
reseed : Seed -> Seed
reseed (Seed seed) =
    Seed (0 :: seed)


step : Seed -> ( Id, Seed )
step (Seed seed) =
    case seed of
        [] ->
            ( Id [ 0 ], Seed [ 0 ] )

        current :: remain ->
            ( Id seed, Seed (current + 1 :: remain) )


thread : Seed -> List (Seed -> ( Seed, thing )) -> ( Seed, List thing )
thread seed steps =
    List.foldl threadThrough ( seed, [] ) steps
        |> Tuple.mapSecond List.reverse


threadThrough current ( seed, past ) =
    let
        ( newSeed, result ) =
            current seed
    in
    ( newSeed, result :: past )


type Id
    = Id (List Int)
