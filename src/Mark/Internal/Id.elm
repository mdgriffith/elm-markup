module Mark.Internal.Id exposing
    ( Choice(..)
    , Id(..)
    , ManyOptions(..)
    , Options(..)
    , getRange
    , manyOptionId
    , optionId
    )

{-| -}


{-| -}
type Id category
    = Id Range


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
    range


{-| -}
manyOptionId : Range -> Id ManyOptions
manyOptionId =
    Id


{-| -}
optionId : Range -> Id Options
optionId =
    Id


type ManyOptions
    = ManyOptions


type Options
    = Options
