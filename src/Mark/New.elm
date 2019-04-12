module Mark.New exposing
    ( Block, from, field
    , bool, float, int, string
    )

{-|

@docs Block, from, field

@docs bool, float, int, string

-}

import Mark.Format as Format
import Mark.Internal.Description exposing (..)
import Mark.Internal.Error as Error
import Mark.Internal.Id as Id exposing (..)
import Mark.Internal.Outcome as Outcome
import Mark.Internal.Parser as Parse
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)



{-
   General Use of Setters.


   makeCircle default =
       New.record "Circle"
           [ ("label", (New.string "Heres my circle!"))
           , ( "x", (New.int 10))
           , ( "y", (New.int 10))
           ]



-}


{-| -}
type alias Block =
    Expectation


{-| -}
block : String -> Block -> Block
block =
    ExpectBlock


{-| -}
record : String -> List ( String, Block )
record =
    ExpectRecord


{-| -}
int : Int -> Block
int =
    ExpectInteger


{-| -}
string : String -> Block
string =
    ExpectString


{-| -}
float : Float -> Block
float =
    ExpectFloat


{-| -}
bool : Bool -> Block
bool =
    ExpectBoolean


{-| -}
many : List Block -> Block
many =
    ExpectManyOf


{-| -}
type alias Text =
    InlineExpectation


{-| -}
text : List Text -> Block
text =
    ExpectText


{-| -}
annotation : Text -> List Attributes -> Text
annotation =
    ExpectAnnotation


{-| -}
token : String -> List Attributes -> Text
token =
    ExpectToken


{-| -}
verbatim : String -> List Attributes -> Text
verbatim =
    ExpectVerbatim


{-| -}
styled : Styling -> String -> Text
styled =
    Debug.todo "Expectation doesn't support this"


{-| -}
italicized : String -> Text
italicized =
    Debug.todo "not yet"


{-| -}
bold : String -> Text
bold =
    Debug.todo "nope"


{-| -}
strike : String -> Text
strike =
    Debug.todo "nada"
