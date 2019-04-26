module Mark.Internal.Index exposing
    ( Index(..)
    , increment
    , indent
    , toList
    , zero
    )

{-| -}


type Index
    = Index Int (List Int)


zero : Index
zero =
    Index 0 []


increment : Index -> Index
increment (Index i base) =
    Index (i + 1) base


indent : Index -> Index
indent (Index i base) =
    Index 0 (i :: base)


toList : Index -> List Int
toList (Index i base) =
    i :: base
