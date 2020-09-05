module Mark.Internal.Id exposing
    ( Id(..)
    , Seed
    , dedent
    , fromString
    , indent
    , initialSeed
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

So, when we `indent`, we're actually adding a level.

    Seed [0]

    Seed [1] --indent -> Seed [1, 0]
                            |
                            v
                         Seed [1, 1]


    Seed [2]

Is there a performance issue with allocating a bunch of lists intead of base Ints?

A very common pattern is to

    1. step a seed forward and pass that seed for the next block
    2. indent the same original seed, and pass that to the children

So

    [0]
        -> stepped [1]  (goes to next element by handing the seed back in the parser)
        -> indented [0,0] (handed to child parser to go nuts with)

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
indent : Seed -> Seed
indent (Seed seed) =
    Seed (0 :: seed)


{-| -}
dedent : Int -> Seed -> Seed
dedent num (Seed seed) =
    Seed (List.drop num seed)


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
