module Mark.New exposing
    ( Block, block, record
    , string, int, float, bool
    , many, tree, Tree(..), Icon(..)
    , Text, text, unstyled, bold, italicized, strike, styled
    , Attribute, annotation, token, verbatim, verbatimWith
    , attrString, attrFloat, attrInt
    , select, restyle, addStyle, removeStyle, unstyle, insertToken
    )

{-|

@docs Block, block, record

@docs string, int, float, bool

@docs many, tree, Tree, Icon

@docs Text, text, unstyled, bold, italicized, strike, styled

@docs Attribute, annotation, token, verbatim, verbatimWith

@docs attrString, attrFloat, attrInt

@docs select, restyle, addStyle, removeStyle, unstyle, insertToken

-}

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
attrFloat : String -> Float -> Attribute
attrFloat name fl =
    ExpectAttrFloat name ( String.fromFloat fl, fl )


{-| -}
attrInt : String -> Int -> Attribute
attrInt =
    ExpectAttrInt


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
italicized : Styles
italicized =
    italicStyle


{-| -}
bold : Styles
bold =
    boldStyle


{-| -}
strike : Styles
strike =
    strikeStyle


{-| -}
unstyled : String -> Text
unstyled str =
    ExpectText (Text emptyStyles str)


{-| Modifying existing text
-}
type Selection
    = -- offset start, offset end
      Relative Offset Offset


{-| -}
type alias Offset =
    Int


{-| Create a selection of text with starting and ending offset of the text.
-}
select : Int -> Int -> Selection
select =
    Relative


{-| Clear styles on the selection and restyle it.
-}
restyle : Selection -> Styles -> List Text -> List Text
restyle selection styling txts =
    List.foldl (doRestyle selection (Restyle styling)) emptySelectionEdit txts
        |> .elements
        |> List.foldl mergeStyles []


{-| Add styles, but don't clear existing ones.
-}
addStyle : Selection -> Styles -> List Text -> List Text
addStyle selection styling txts =
    List.foldl (doRestyle selection (AddStyle styling)) emptySelectionEdit txts
        |> .elements
        |> List.foldl mergeStyles []


{-| Add styles, but don't clear existing ones.
-}
removeStyle : Selection -> Styles -> List Text -> List Text
removeStyle selection styling txts =
    List.foldl (doRestyle selection (RemoveStyle styling)) emptySelectionEdit txts
        |> .elements
        |> List.foldl mergeStyles []


{-| Remove styles on selection.
-}
unstyle : Selection -> List Text -> List Text
unstyle selection txts =
    List.foldl (doRestyle selection (Restyle emptyStyles)) emptySelectionEdit txts
        |> .elements
        |> List.foldl mergeStyles []


{-| Wrap the selection in an annotation.

**Note** this will remove any existing annotations within or overlapping the selection.

-}
annotate : Selection -> String -> List Attribute -> List Text -> List Text
annotate seletion name attrs txt =
    txt


{-| Wrap the selection as `Mark.verbatim` with no attributes.

**Note** this will remove any existing annotations within or overlapping the selection.

-}
makeVerbatim : Selection -> List Text -> List Text
makeVerbatim selection txt =
    txt


{-| Wrap the selection as `Mark.verbatim` with attributes.

**Note** this will remove any existing annotations within or overlapping the selection.

-}
makeVerbatimWith : Selection -> String -> List Attribute -> List Text -> List Text
makeVerbatimWith selection name attrs txt =
    txt


{-| Insert a token at the existing offset.
-}
insertToken : Offset -> String -> List Attribute -> List Text -> List Text
insertToken offset name attrs txts =
    List.foldl (insert offset (ExpectToken name attrs)) emptyEdit txts
        |> .elements
        |> List.foldl mergeStyles []


finalizeEdit cursor =
    List.reverse cursor.elements


emptyEdit =
    { offset = 0
    , elements = []
    }


emptySelectionEdit =
    { offset = 0
    , elements = []
    , selection = Nothing
    }


{-| Folds over a list of styles and merges them if they're compatible
-}
mergeStyles : Text -> List Text -> List Text
mergeStyles inlineEl gathered =
    case gathered of
        [] ->
            [ inlineEl ]

        prev :: tail ->
            case attemptMerge inlineEl prev of
                Nothing ->
                    inlineEl :: prev :: tail

                Just merged ->
                    merged :: tail


attemptMerge one two =
    case ( one, two ) of
        ( ExpectText (Text stylingOne strOne), ExpectText (Text stylingTwo strTwo) ) ->
            if stylingOne == stylingTwo then
                Just (ExpectText (Text stylingOne (strOne ++ strTwo)))

            else
                Nothing

        ( ExpectAnnotation name1 attrs1 els1, ExpectAnnotation name2 attrs2 els2 ) ->
            if name1 == name2 && attrs1 == attrs2 then
                Just (ExpectAnnotation name1 attrs1 (els1 ++ els2))

            else
                Nothing

        ( ExpectVerbatim name1 attrs1 str1, ExpectVerbatim name2 attrs2 str2 ) ->
            Nothing

        ( _, _ ) ->
            Nothing


type Restyle
    = Restyle Styles
    | AddStyle Styles
    | RemoveStyle Styles


doRestyle :
    Selection
    -> Restyle
    -> Text
    ->
        { elements : List Text
        , offset : Int
        , selection : Maybe (List Text)
        }
    -> { elements : List Text, offset : Int, selection : Maybe (List Text) }
doRestyle (Relative start end) newStyles current cursor =
    let
        len =
            length current
    in
    case cursor.selection of
        Nothing ->
            if cursor.offset <= start && cursor.offset + len >= start then
                {- Start Selection -}
                if cursor.offset + len >= end then
                    {- We finish the selection in this element -}
                    let
                        ( before, afterLarge ) =
                            splitAt (start - cursor.offset) current

                        ( selected, after ) =
                            splitAt (end - cursor.offset) afterLarge
                    in
                    { offset = cursor.offset + len
                    , elements =
                        after :: applyStyles newStyles selected :: before :: cursor.elements
                    , selection =
                        Nothing
                    }

                else
                    let
                        ( before, after ) =
                            splitAt (start - cursor.offset) current
                    in
                    { offset = cursor.offset + len
                    , elements =
                        before :: cursor.elements
                    , selection =
                        Just [ after ]
                    }

            else
                { offset = cursor.offset + len
                , elements = current :: cursor.elements
                , selection = cursor.selection
                }

        Just selection ->
            if cursor.offset + len >= end then
                let
                    ( before, after ) =
                        splitAt (end - cursor.offset) current

                    fullSelection =
                        before :: selection
                in
                { offset = cursor.offset + len
                , elements = after :: List.map (applyStyles newStyles) fullSelection ++ cursor.elements
                , selection = Nothing
                }

            else
                { offset = cursor.offset + len
                , elements = cursor.elements
                , selection = Just (current :: selection)
                }


applyStyles : Restyle -> Text -> Text
applyStyles styling inlineEl =
    case inlineEl of
        ExpectText txt ->
            ExpectText (applyStylesToText styling txt)

        ExpectAnnotation name attrs textElements ->
            ExpectAnnotation name attrs (List.map (applyStylesToText styling) textElements)

        ExpectToken name attrs ->
            ExpectToken name attrs

        ExpectVerbatim name attrs str ->
            ExpectVerbatim name attrs str


applyStylesToText styling (Text styles str) =
    case styling of
        Restyle newStyle ->
            Text newStyle str

        RemoveStyle toRemove ->
            Text
                { bold = styles.bold && not toRemove.bold
                , italic = styles.italic && not toRemove.italic
                , strike = styles.strike && not toRemove.strike
                }
                str

        AddStyle toAdd ->
            Text
                { bold = styles.bold || toAdd.bold
                , italic = styles.italic || toAdd.italic
                , strike = styles.strike || toAdd.strike
                }
                str


insert offset newToken current cursor =
    let
        len =
            length current
    in
    if cursor.offset == offset then
        { offset = cursor.offset + len
        , elements =
            current
                :: newToken
                :: cursor.elements
        }

    else if cursor.offset + len == offset then
        { offset = cursor.offset + len
        , elements =
            newToken
                :: current
                :: cursor.elements
        }

    else if cursor.offset + len > offset && cursor.offset < offset then
        let
            ( before, after ) =
                splitAt offset current
        in
        { offset = cursor.offset + len
        , elements =
            after :: newToken :: before :: cursor.elements
        }

    else
        { offset = cursor.offset + len
        , elements = current :: cursor.elements
        }


{-| Splits the current element based on an index.

THe index

This function should only be called when the offset is definitely contained within the element provided, not on the edges.

-}
splitAt : Offset -> Text -> ( Text, Text )
splitAt offset inlineEl =
    case inlineEl of
        ExpectText (Text styling txt) ->
            ( ExpectText (Text styling (String.left offset txt))
            , ExpectText (Text styling (String.dropLeft offset txt))
            )

        ExpectAnnotation name attrs textElements ->
            let
                { left, right } =
                    List.foldl (splitTextElements offset)
                        { offset = 0
                        , left = []
                        , right = []
                        }
                        textElements

                splitTextElements off (Text styling txt) cursor =
                    if off >= cursor.offset && off <= cursor.offset + String.length txt then
                        { offset = cursor.offset + String.length txt
                        , left = Text styling (String.left (offset - cursor.offset) txt) :: cursor.left
                        , right = Text styling (String.dropLeft (offset - cursor.offset) txt) :: cursor.right
                        }

                    else if off < cursor.offset then
                        { offset = cursor.offset + String.length txt
                        , left = cursor.left
                        , right = Text styling txt :: cursor.right
                        }

                    else
                        { offset = cursor.offset + String.length txt
                        , left = Text styling txt :: cursor.left
                        , right = cursor.right
                        }
            in
            ( ExpectAnnotation name attrs (List.reverse left)
            , ExpectAnnotation name attrs (List.reverse right)
            )

        ExpectToken name attrs ->
            -- This shoudn't happen because we're expecting the offset
            -- to be within the range, and this range is 0
            ( ExpectText (Text emptyStyles "")
            , ExpectToken name attrs
            )

        ExpectVerbatim name attrs str ->
            ( ExpectVerbatim name attrs (String.left offset str)
            , ExpectVerbatim name attrs (String.dropLeft offset str)
            )


length : Text -> Int
length inlineEl =
    case inlineEl of
        ExpectText txt ->
            textLength txt

        ExpectAnnotation name attrs textElements ->
            List.sum (List.map textLength textElements)

        ExpectToken name attrs ->
            0

        ExpectVerbatim name attrs str ->
            String.length str


textLength : Desc.Text -> Int
textLength (Text _ str) =
    String.length str



-- doRestyle (Relative start end) newStyles current cursor =
--     case current of
--         ExpectText (Text styling str) ->
--             let
--                 elementEnd = String.length str ++ cursor.offset
--             in
--             if cursor.offset >= start
--                 && cursor.offset <= end then
--                 { cursor | elements = current :: cursor.elements }
--             else
--                 { cursor | elements = current :: cursor.elements }
--         ExpectAnnotation name attrs textElements ->
--             { cursor | elements = current :: cursor.elements }
--         ExpectToken name attrs ->
--             { cursor | elements = current :: cursor.elements }
--         ExpectVerbatim name attrs str ->
--             { cursor | elements = current :: cursor.elements }
-- {-| Making text edits:
--   - Cycle until you're at or contain the offset beginning.
--   - Being Edit
--       - Change style
--         -> break starting stretch if different from requested
--         -> create new stretch with updated styles.
-- -}
-- makeTextEdit current existingEdit =
--     case current of
--         ExpectText (Text styling str) ->
--             existingEdit
--         ExpectAnnotation name attrs textElements ->
--             existingExit
--         -- tokens have no placeholder
--         ExpectToken name attrs ->
--             existingExit
--         -- name, attrs, placeholder content
--         ExpectVerbatim name attrs str ->
--             existingExit
