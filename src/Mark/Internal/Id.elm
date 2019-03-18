module Mark.Internal.Id exposing
    ( Choice(..)
    , Id(..)
    , ManyOptions(..)
    , Options(..)
    , Seed
    , getRange
    , initialSeed
    , reseed
    , step
    , thread
    , threadThrough
    )

{-| -}


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
            ( Id 0, Seed [ 0 ] )

        current :: remain ->
            ( Id current, Seed (current + 1 :: remain) )


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
    = Id Int


type alias Position =
    { offset : Int
    , line : Int
    , column : Int
    }


{-| -}
type Choice id expectation
    = Choice id expectation


type alias Range =
    { start : Position
    , end : Position
    }


{-| -}
getRange : Id anything -> Range
getRange (Id range) =
    Debug.todo "ohno"



-- {-| -}
-- manyOptionId : Range -> Id ManyOptions
-- manyOptionId =
--     Id
-- {-| -}
-- optionId : Range -> Id Options
-- optionId =
--     Id


type ManyOptions
    = ManyOptions


type Options
    = Options
