module Mark.Internal.Format exposing (Text, bold, cyan, red, text, underline, yellow)

{-| -}


type alias Text =
    { text : String
    , bold : Bool
    , underline : Bool
    , color : Maybe String
    }


text : String -> Text
text str =
    { text = str
    , color = Nothing
    , bold = False
    , underline = False
    }


underline : Text -> Text
underline txt =
    { txt | underline = True }


bold : Text -> Text
bold txt =
    { txt | bold = True }


red : Text -> Text
red txt =
    { txt | color = Just "red" }


yellow : Text -> Text
yellow txt =
    { txt | color = Just "yellow" }


cyan : Text -> Text
cyan txt =
    { txt | color = Just "cyan" }
