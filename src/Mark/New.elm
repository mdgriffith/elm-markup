module Mark.New exposing
    ( Block
    , string, int, float, bool
    , block, record, many, tree, Tree(..), Icon(..)
    , Text, text, unstyled, bold, italics, strike, Styles, styled
    , annotation, verbatim
    )

{-| Create a new `Mark.Block` that can be added to your document using `Mark.Edit.replace` or `Mark.Edit.insertAt`.

Let's say we are capturing a basic diagram in our document and we want to dynamically insert a circle.

We can make a new circle by writing the following function

    import Mark.New as New

    type alias Circle =
        { x : Int
        , y : Int
        , label : String
        }

    circle : Circle -> Mark.New.Block
    circle details =
        New.record "Circle"
            [ ( "label", New.string details.label )
            , ( "x", New.int details.x )
            , ( "y", New.int details.y )
            ]

And then insert our newly made circle using `Mark.Edit.insertAt`.

**Note:** The document will only accept edits which are valid.

@docs Block


# Primitives

@docs string, int, float, bool


# Blocks

@docs block, record, many, tree, Tree, Icon


# Text

Here's an example of creating some text.

    newText =
        New.text
            [ Mark.unstyled "Look at my "
            , Mark.bold "cool"
            , Mark.unstyled " new text!"
            ]

@docs Text, text, unstyled, bold, italics, strike, Styles, styled


# Text Annotation

@docs annotation, verbatim

-}

import Mark.Internal.Description as Desc exposing (..)
import Mark.Internal.Error as Error
import Mark.Internal.Id as Id exposing (..)
import Mark.Internal.Outcome as Outcome
import Mark.Internal.Parser as Parse
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


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
    -- But Likely won't affect anything at the moment.
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
                    Desc.AutoNumber 1
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


{-|

    New.annotation
        { name = "link"
        , text =
            [ New.unstyled "my link to the elm website!" ]
        , fields =
            [ ( "url", Mark.string "https://elm-lang.com" ) ]
        }

-}
annotation :
    { name : String
    , text : List ( Styles, String )
    , fields : List ( String, Block )
    }
    -> Text
annotation config =
    ExpectInlineBlock
        { name = config.name
        , kind = SelectText (List.map toText config.text)
        , fields = config.fields
        }


toText : ( Styles, String ) -> Desc.Text
toText ( styles, str ) =
    Text styles str


{-| -}
verbatim :
    { name : String
    , text : String
    , fields : List ( String, Block )
    }
    -> Text
verbatim config =
    ExpectInlineBlock
        { name = config.name
        , kind = SelectString config.text
        , fields = config.fields
        }



-- {-| -}
-- type alias Attribute =
--     AttrExpectation
-- {-| -}
-- attrString : String -> String -> Attribute
-- attrString =
--     ExpectAttrString
-- {-| -}
-- attrFloat : String -> Float -> Attribute
-- attrFloat name fl =
--     ExpectAttrFloat name ( String.fromFloat fl, fl )
-- {-| -}
-- attrInt : String -> Int -> Attribute
-- attrInt =
--     ExpectAttrInt
-- {-| -}
-- annotation : List Text -> String -> List Attribute -> Text
-- annotation content name attrs =
--     ExpectAnnotation name attrs (List.filterMap onlyText content)
-- onlyText txt =
--     case txt of
--         ExpectText t ->
--             Just t
--         _ ->
--             Nothing
-- {-| -}
-- token : String -> List Attribute -> Text
-- token =
--     ExpectToken
-- {-| -}
-- verbatim : String -> Text
-- verbatim =
--     ExpectVerbatim "" []
-- {-| -}
-- verbatimWith : String -> String -> List Attribute -> Text
-- verbatimWith content name attributes =
--     ExpectVerbatim name attributes content


{-| -}
type alias Styles =
    { bold : Bool
    , italic : Bool
    , strike : Bool
    }


{-| -}
styled : Styles -> String -> Text
styled styling str =
    ExpectText (Text styling str)


{-| -}
italics : String -> Text
italics str =
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
