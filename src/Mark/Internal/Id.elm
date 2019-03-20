module Mark.Internal.Id exposing
    ( Choice(..)
    , Id(..)
    , ManyOptions(..)
    , Options(..)
    , Seed
    , initialSeed
    , reseed
    , step
    , thread
    , threadThrough
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


{-| -}
reseed : Seed -> Seed
reseed (Seed seed) =
    Seed (0 :: seed)


step : Seed -> ( Id category, Seed )
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


type Id category
    = Id (List Int)


{-| -}
type Choice id expectation
    = Choice id expectation


type ManyOptions
    = ManyOptions


type Options
    = Options
