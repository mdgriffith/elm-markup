module Internal.Flag exposing (..)

{-| -}

import Bitwise


type Field
    = Field Int


type Flag
    = Flag Int


none : Field
none =
    Field 0


{-| If the query is in the truth, return True
-}
present : Flag -> Field -> Bool
present myFlag (Field field) =
    case myFlag of
        Flag first ->
            Bitwise.and first field == first


{-| Add a flag to a field.
-}
add : Flag -> Field -> Field
add myFlag (Field field) =
    case myFlag of
        Flag existingFlag ->
            Field (Bitwise.or existingFlag field)


{-| Add a flag to a field.
-}
remove : Flag -> Field -> Field
remove myFlag (Field field) =
    case myFlag of
        Flag existingFlag ->
            Field
                (Bitwise.and
                    (Bitwise.complement existingFlag)
                    field
                )


flip : Flag -> Field -> Field
flip myFlag field =
    if present myFlag field then
        remove myFlag field
    else
        add myFlag field


{-| -}
flag : Int -> Flag
flag i =
    Flag
        (Bitwise.shiftLeftBy i 1)


italic =
    flag 0


bold =
    flag 1


underline =
    flag 2


strike =
    flag 3


token =
    flag 4


link =
    flag 5


doubleQuote =
    flag 6
