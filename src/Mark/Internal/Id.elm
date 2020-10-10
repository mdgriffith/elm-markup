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


initialSeed : String -> Seed
initialSeed doc =
    Seed doc [ 0 ]


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
    = Seed String (List Int)


type Id
    = Id String (List Int)


toString (Id str ids) =
    ids
        |> List.map String.fromInt
        |> String.join "."
        |> (\x -> "m." ++ str ++ "." ++ x)


fromString str =
    case String.split "." str of
        "m" :: docId :: remain ->
            let
                parsed =
                    List.filterMap String.toInt remain
            in
            if List.length parsed == List.length remain then
                Just (Id docId parsed)

            else
                Nothing

        _ ->
            Nothing


{-| -}
indent : Seed -> Seed
indent (Seed doc seed) =
    Seed doc (0 :: seed)


{-| -}
dedent : Int -> Seed -> Seed
dedent num (Seed doc seed) =
    Seed doc (List.drop num seed)


step : Seed -> ( Id, Seed )
step (Seed doc seed) =
    case seed of
        [] ->
            ( Id doc [ 0 ], Seed doc [ 0 ] )

        current :: remain ->
            ( Id doc seed, Seed doc (current + 1 :: remain) )


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
