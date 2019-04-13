module Mark.New exposing
    ( Block, block, record
    , string, int, float, bool
    , many, tree, Tree(..), Icon(..)
    , text, unstyled, bold, italicized, strike, styled
    , Attribute, annotation, token, verbatim, verbatimWith
    )

{-|

@docs Block, block, record

@docs string, int, float, bool

@docs many, tree, Tree, Icon

@docs text, unstyled, bold, italicized, strike, styled

@docs Attribute, annotation, token, verbatim, verbatimWith

-}

import Mark
import Mark.Edit
import Mark.Internal.Description as Desc exposing (..)
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
record : String -> List ( String, Block ) -> Block
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
type Tree
    = Tree
        { icon : Icon
        , content : List Block
        , children : List Tree
        }


{-| -}
type Icon
    = Bullet
    | Number


{-| -}
tree : List Tree -> Block
tree treeContents =
    -- BUGBUG: ExpectNothing is not right
    ExpectTree ExpectNothing
        (List.map convertToTreeExpectation treeContents)


{-| This is necessary to make the types work out, but would be nice to remove.
-}
convertToTreeExpectation (Tree details) =
    TreeExpectation
        { icon =
            case details.icon of
                Bullet ->
                    Desc.Bullet

                Number ->
                    Desc.AutoNumber
        , content = details.content
        , children =
            List.map convertToTreeExpectation details.children
        }


{-| -}
type alias Text =
    InlineExpectation


{-| -}
text : List Text -> Block
text =
    ExpectTextBlock


{-| -}
type alias Attribute =
    AttrExpectation


{-| -}
attrString : String -> String -> Attribute
attrString =
    ExpectAttrString


{-| -}
annotation : List Text -> String -> List Attribute -> Text
annotation content name attrs =
    ExpectAnnotation name attrs (List.filterMap onlyText content)


onlyText txt =
    case txt of
        ExpectText t ->
            Just t

        _ ->
            Nothing


{-| -}
token : String -> List Attribute -> Text
token =
    ExpectToken


{-| -}
verbatim : String -> Text
verbatim =
    ExpectVerbatim "" []


{-| -}
verbatimWith : String -> String -> List Attribute -> Text
verbatimWith content name attributes =
    ExpectVerbatim name attributes content


{-| -}
styled : Styling -> String -> Text
styled styling str =
    ExpectText (Text styling str)


{-| -}
italicized : String -> Text
italicized str =
    ExpectText (Text italicStyle str)


{-| -}
bold : String -> Text
bold str =
    ExpectText (Text boldStyle str)


{-| -}
strike : String -> Text
strike str =
    ExpectText (Text strikeStyle str)


{-| -}
unstyled : String -> Text
unstyled str =
    ExpectText (Text emptyStyles str)
