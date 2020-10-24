module Mark.New exposing
    ( Block
    , string, int, float, bool
    , block, record, many
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

@docs block, record, many

@docs bullet, numbered


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
    Desc.New


{-| -}
block : String -> Block -> Block
block =
    NewBlock


{-| -}
record : String -> List ( String, Block ) -> Block
record =
    NewRecord


{-| -}
int : Int -> Block
int =
    NewInteger


{-| -}
string : String -> Block
string =
    NewString


{-| -}
float : Float -> Block
float =
    NewFloat


{-| -}
bool : Bool -> Block
bool =
    NewBoolean


{-| -}
many : List Block -> Block
many =
    NewGroup


{-| -}
bullet : List Block -> Block
bullet content =
    NewItem Desc.Bullet content


{-| -}
numbered : Int -> List Block -> Block
numbered i content =
    NewItem (Desc.AutoNumber i) content


{-| -}
type alias Text =
    NewInline


{-| -}
text : List Text -> Block
text =
    NewTextBlock


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
    NewInlineBlock
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
    NewInlineBlock
        { name = config.name
        , kind = SelectString config.text
        , fields = config.fields
        }


{-| -}
type alias Styles =
    { bold : Bool
    , italic : Bool
    , strike : Bool
    }


{-| -}
styled : Styles -> String -> Text
styled styling str =
    NewText (Text styling str)


{-| -}
italics : String -> Text
italics str =
    NewText (Text italicStyle str)


{-| -}
bold : String -> Text
bold str =
    NewText (Text boldStyle str)


{-| -}
strike : String -> Text
strike str =
    NewText (Text strikeStyle str)


{-| -}
unstyled : String -> Text
unstyled str =
    NewText (Text emptyStyles str)
