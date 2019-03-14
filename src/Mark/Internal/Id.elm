module Mark.Internal.Id exposing
    ( Choice(..)
    , Id(..)
    , ManyOptions(..)
    , Options(..)
    , getRange
    , reseed
    , step
    , thread
    , threadThrough
    )

{-| -}

import Random


idGenerator =
    Random.map Id (Random.int Random.minInt Random.maxInt)


{-| This is dubious but I think it will work for this case.

We need to create a seed that's orthogonal to the original due to how `Mark.manyOf` needs to generate ids based on what it finds vs what the parser structure is.

Meaning, normally each parser is given a unique seed based on where it is in the parser tree.

However, manyOf will need to issue some number of IDs based on the # of items it finds, as opposed ot the parser structure.

Essentially we need somethign like this.


## Normal, deterministic number generation

    1

    8

    3

    5

    6


## With branching

    1
    8
    3 -> 12 -> 18
    5
    6 -> 20 -> 21

-}
reseed : Random.Seed -> Random.Seed
reseed seed =
    let
        ( Id i, newSeed ) =
            step seed
    in
    Random.initialSeed i


step : Random.Seed -> ( Id category, Random.Seed )
step seed =
    Random.step idGenerator seed


thread : Random.Seed -> List (Random.Seed -> ( Random.Seed, thing )) -> ( Random.Seed, List thing )
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



-- {-| -}
-- type Id category
--     = Id Range


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
