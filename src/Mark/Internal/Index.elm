module Mark.Internal.Index exposing
    ( Index(..)
    , dedent
    , increment
    , indent
    , toList
    , top
    , zero
    )

{-| -}


{-| -}
type Index
    = Index Int (List Int)


top : Index -> Int
top (Index i _) =
    i


zero : Index
zero =
    Index 0 []


increment : Index -> Index
increment (Index i base) =
    Index (i + 1) base


dedent : Index -> Index
dedent (Index i base) =
    case base of
        [] ->
            zero

        topI :: newBase ->
            Index topI newBase


indent : Index -> Index
indent (Index i base) =
    Index 0 (i :: base)


toList : Index -> ( Int, List Int )
toList (Index i base) =
    ( i, base )
