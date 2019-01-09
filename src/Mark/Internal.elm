module Mark.Internal exposing
    ( AstError(..)
    , Block(..)
    , Context(..)
    , Description(..)
    , Document(..)
    , ErrorMessage
    , Field(..)
    , Inline(..)
    , InlineDescription(..)
    , Level(..)
    , Mapped
    , Message
    , Nested(..)
    , NestedIndex
    , Position
    , PotentialInline(..)
    , PotentialInlineVal(..)
    , Problem(..)
    , ProblemMessage(..)
    , Range
    , Replacement(..)
    , Similarity
    , Style(..)
    , Text(..)
    , TextAccumulator(..)
    , TextCursor(..)
    , TextDescription(..)
    , Theme(..)
    , TreeBuilder(..)
    , addBalance
    , addIndent
    , addItem
    , addText
    , addTextToText
    , almostReplacement
    , balanceId
    , balanced
    , block
    , blocksOrNewlines
    , bool
    , buildTree
    , changeStyle
    , changeStyleOnText
    , collapseLevel
    , compile
    , currentStyles
    , date
    , descending
    , document
    , empty
    , emptyText
    , emptyTreeBuilder
    , errorToHtml
    , errorToString
    , exactly
    , expectIndentation
    , field
    , fieldName
    , fieldParser
    , fieldToString
    , firstChar
    , flipStyle
    , float
    , floatBetween
    , floating
    , focusPrevWord
    , focusSpace
    , focusWord
    , foregroundClr
    , formatErrorHtml
    , formatErrorString
    , getDescriptionPosition
    , getErrorPosition
    , getField
    , getLine
    , getParser
    , getPosition
    , getPotentialInline
    , getPrevWord
    , getRemap
    , getWord
    , highlightPreviousWord
    , highlightSpace
    , highlightUntil
    , highlightWord
    , hint
    , indentedBlocksOrNewlines
    , indentedString
    , inline
    , inlineString
    , inlineText
    , int
    , intBetween
    , integer
    , manyOf
    , map
    , matchField
    , measure
    , mergeErrors
    , multiline
    , nested
    , newline
    , oneOf
    , parse
    , parseFields
    , parseInline
    , parseInlineComponents
    , parseInlineText
    , parseRecord
    , peek
    , record10
    , record2
    , record3
    , record4
    , record5
    , record6
    , record7
    , record8
    , record9
    , redClr
    , reduceRender
    , removeBalance
    , render
    , renderErrors
    , renderInline
    , renderLevels
    , renderMessageHtml
    , renderText
    , renderTextComponent
    , renderTreeNodeSmall
    , replace
    , replacement
    , replacementStartingChars
    , rev
    , reverseTree
    , similarity
    , singleLine
    , skipBlankLinesWith
    , startWith
    , string
    , stub
    , styleChars
    , styleNames
    , styledTextParser
    , styledTextParserLoop
    , stylingChars
    , text
    , toString
    , withFieldName
    , withRange
    , yellowClr
    )

{-| -}

import Html
import Html.Attributes
import Iso8601
import Mark.Format as Format
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)
import Time


{-| -}
type Document data
    = Document (Description -> Result AstError data) (Parser Context Problem Description)


{-| A `Block data` is just a parser that results in `data`.

You'll be building up your `Document` in terms of the `Blocks`.

A block starts with `|` and has a name(already built into the parser)

A value is just a raw parser.

-}
type Block data
    = Block String (Description -> Result AstError data) (Parser Context Problem Description)
    | Value (Description -> Result AstError data) (Parser Context Problem Description)


{-| -}
type Inline data
    = Inline String (List InlineDescription -> Result AstError (List data)) (List Style -> PotentialInline)


{-| -}
inline : String -> result -> Inline result
inline name result =
    Inline name
        (\descriptors ->
            let
                _ =
                    Debug.log "inline" descriptors
            in
            Ok [ result ]
        )
        (\styles ->
            PotentialInline name []
        )


{-| -}
inlineString : String -> Inline (String -> result) -> Inline result
inlineString name (Inline inlineName inlineRenderer inlinePotential) =
    Inline inlineName
        (\descriptors ->
            let
                _ =
                    Debug.log "inline String" descriptors
            in
            case descriptors of
                [] ->
                    Err InvalidAst

                (DescribeInlineString key range str) :: remaining ->
                    case inlineRenderer remaining of
                        Err err ->
                            Err err

                        Ok renderers ->
                            Ok (List.map (\x -> x str) renderers)

                _ ->
                    Err InvalidAst
        )
        (\styles ->
            case inlinePotential styles of
                PotentialInline parentName vals ->
                    PotentialInline parentName (vals ++ [ PotentialInlineString name ])
        )


{-| -}
inlineText : Inline (List Text -> result) -> Inline result
inlineText (Inline inlineName inlineRenderer inlinePotential) =
    Inline inlineName
        (\descriptors ->
            let
                _ =
                    Debug.log "inlineText" descriptors
            in
            case descriptors of
                [] ->
                    Err InvalidAst

                (DescribeInlineText range str) :: remaining ->
                    case inlineRenderer remaining of
                        Err err ->
                            Err err

                        Ok renderers ->
                            Ok (List.map (\x -> x str) renderers)

                _ ->
                    Err InvalidAst
        )
        (\styles ->
            case inlinePotential styles of
                PotentialInline parentName vals ->
                    PotentialInline parentName (vals ++ [ PotentialInlineText ])
        )


getParser : Block data -> Parser Context Problem Description
getParser fromBlock =
    case fromBlock of
        Block name _ p ->
            Parser.succeed identity
                |. Parser.token (Parser.Token "|" (ExpectingBlockName name))
                |. Parser.chompIf (\c -> c == ' ') Space
                |= p

        Value _ p ->
            p


render : Block data -> Description -> Result AstError data
render fromBlock =
    case fromBlock of
        Block name r _ ->
            r

        Value r _ ->
            r


type alias Position =
    { offset : Int
    , line : Int
    , column : Int
    }


type alias Mapped =
    { original : Range Position
    , new : Range Position
    }


type alias Range x =
    { start : x
    , end : x
    }


{-| -}
type Context
    = InBlock String
    | InInline String
    | InRecord String
    | InRecordField String
    | InRemapped Position


{-| -}
type Style
    = Bold
    | Italic
    | Strike


{-| -}
type Problem
    = ExpectingIndent Int
    | InlineStart
    | InlineEnd
    | BlockStart
    | Expecting String
    | ExpectingBlockName String
    | ExpectingInlineName String
    | ExpectingFieldName String
    | NonMatchingFields
        { expecting : List String
        , found : List String
        }
    | MissingField String
    | RecordError
    | Escape
    | EscapedChar
    | Newline
    | Space
    | End
    | Integer
    | FloatingPoint
    | InvalidNumber
    | UnexpectedEnd
    | CantStartTextWithSpace
    | UnclosedStyles (List Style)
    | BadDate String
    | IntOutOfRange
        { found : Int
        , min : Int
        , max : Int
        }
    | FloatOutOfRange
        { found : Float
        , min : Float
        , max : Float
        }
    | UnexpectedField
        { found : String
        , options : List String
        , recordName : String
        }


type Description
    = DescribeDocument (Range Position) Description
    | DescribeBlock String (Range Position) Description
    | DescribeUnexpected (Range Position) String
    | DescribeStub String (Range Position)
    | Record String (Range Position) (List ( String, Description ))
    | OneOf (Range Position) (List Description) Description
    | ManyOf (Range Position) (List Description) (List Description)
    | StartsWith (Range Position) Description Description
    | DescribeBoolean (Range Position) Bool
    | DescribeInteger (Range Position) Int
    | DescribeFloat (Range Position) Float
    | DescribeFloatBetween Float Float (Range Position) Float
    | DescribeIntBetween Int Int (Range Position) Int
    | DescribeText (Range Position) (List TextDescription)
    | DescribeString (Range Position) String
    | DescribeMultiline (Range Position) String
    | DescribeStringExactly (Range Position) String
    | DescribeDate (Range Position) Time.Posix
    | DescribeBadDate (Range Position) String
    | DescribeTree (Range Position) (List (Nested ( Description, List Description )))


type TextDescription
    = Styled (Range Position) Text
    | DescribeInline String (Range Position) (List InlineDescription)



-- {-| -}
-- type Inline data
--     = Inline String (TextDescription -> Result AstError data) (List Style -> PotentialInline)


type PotentialInline
    = PotentialInline String (List PotentialInlineVal)


type PotentialInlineVal
    = PotentialInlineString String
    | PotentialInlineText


type InlineDescription
    = DescribeInlineString String (Range Position) String
    | DescribeInlineText (Range Position) (List Text)


{-| A text fragment with some styling.
-}
type Text
    = Text (List Style) String


getDescriptionPosition desc =
    case desc of
        DescribeDocument range child ->
            range

        DescribeBlock name range child ->
            range

        DescribeUnexpected range _ ->
            range

        DescribeStub name range ->
            range

        Record name range fields ->
            range

        OneOf range options found ->
            range

        ManyOf range options foundEls ->
            range

        StartsWith range fst snd ->
            range

        DescribeInteger range _ ->
            range

        DescribeFloat range _ ->
            range

        DescribeFloatBetween bound1 bound2 range _ ->
            range

        DescribeIntBetween bound1 bound2 range _ ->
            range

        DescribeText range _ ->
            range

        DescribeString range _ ->
            range

        DescribeMultiline range _ ->
            range

        DescribeStringExactly range value ->
            range

        DescribeDate range _ ->
            range

        DescribeBadDate range _ ->
            range

        DescribeBoolean range _ ->
            range

        DescribeTree range _ ->
            range


{-| -}
type TextAccumulator rendered
    = TextAccumulator
        -- Accumulator string
        { text : Text

        -- Accumulator of element constructors
        , rendered : List rendered
        , balancedReplacements : List String
        }


{-| -}
type Replacement
    = Replacement String String
    | Balanced
        { start : ( String, String )
        , end : ( String, String )
        }


{-| -}
toString : Int -> Description -> String
toString indent description =
    case description of
        DescribeDocument range child ->
            toString indent child

        DescribeBlock name range child ->
            String.repeat (indent * 4) " "
                ++ "| "
                ++ name
                ++ "\n"
                ++ toString (indent + 1) child

        DescribeUnexpected range str ->
            str

        DescribeStub name range ->
            String.repeat (indent * 4) " "
                ++ "| "
                ++ name
                ++ "\n"

        Record name range fields ->
            String.repeat (indent * 4) " "
                ++ "| "
                ++ name
                ++ "\n"
                ++ String.join "\n" (List.map (fieldToString (indent + 1)) fields)

        OneOf range options found ->
            toString indent found

        ManyOf range options found ->
            found
                |> List.map (toString indent)
                |> String.join "\n\n"

        StartsWith range start end ->
            toString indent start ++ toString indent end

        DescribeBoolean range b ->
            if b then
                "True"

            else
                "False"

        DescribeInteger range i ->
            String.fromInt i

        DescribeFloat range f ->
            String.fromFloat f

        DescribeFloatBetween low high range f ->
            String.fromFloat f

        DescribeIntBetween low high range i ->
            String.fromInt i

        DescribeText range textNodes ->
            ""

        DescribeString range str ->
            str

        DescribeMultiline range str ->
            str

        DescribeStringExactly range str ->
            str

        DescribeDate range posix ->
            ""

        DescribeBadDate range str ->
            str

        DescribeTree _ tree ->
            ""


fieldToString indent ( name, val ) =
    String.repeat (indent * 4) " " ++ name ++ " = " ++ toString indent val


{-| -}
parse :
    Document data
    -> String
    -> Result (List (Parser.DeadEnd Context Problem)) Description
parse (Document renderer blocks) source =
    Parser.run blocks source


{-| -}
compile : Document data -> String -> Result AstError data
compile (Document renderAst blocks) source =
    case Parser.run blocks source of
        Ok ast ->
            renderAst ast

        Err err ->
            Err InvalidAst


{-| -}
document : (Range Position -> child -> result) -> Block child -> Document result
document renderer child =
    Document
        (\desc ->
            case desc of
                DescribeDocument range childDesc ->
                    case render child childDesc of
                        Err err ->
                            Err err

                        Ok renderedChild ->
                            Ok
                                (renderer range renderedChild)

                _ ->
                    Err InvalidAst
        )
        (Parser.map
            (\( range, val ) ->
                DescribeDocument range val
            )
            (withRange (Parser.withIndent 0 (getParser child)))
        )


{-| Change the result of a block by applying a function to it.
-}
map : (a -> b) -> Block a -> Block b
map fn child =
    case child of
        Block name renderer parser ->
            Block name
                (Result.map fn << renderer)
                parser

        Value renderer parser ->
            Value (Result.map fn << renderer) parser


{-| -}
block : String -> (Range Position -> child -> result) -> Block child -> Block result
block name renderer child =
    Block name
        (\desc ->
            case desc of
                DescribeBlock actualBlockName range childDesc ->
                    if actualBlockName == name then
                        case render child childDesc of
                            Err err ->
                                Err err

                            Ok renderedChild ->
                                Ok
                                    (renderer range renderedChild)

                    else
                        Err InvalidAst

                _ ->
                    Err InvalidAst
        )
        (Parser.map
            (\( range, val ) ->
                DescribeBlock name range val
            )
         <|
            withRange
                (Parser.getIndent
                    |> Parser.andThen
                        (\indent ->
                            Parser.succeed
                                identity
                                |. Parser.keyword (Parser.Token name (ExpectingBlockName name))
                                |. Parser.chompWhile (\c -> c == ' ')
                                |. Parser.chompIf (\c -> c == '\n') Newline
                                |. Parser.oneOf
                                    [ Parser.succeed ()
                                        |. Parser.backtrackable (Parser.chompWhile (\c -> c == ' '))
                                        |. Parser.backtrackable (Parser.chompIf (\c -> c == '\n') Newline)
                                    , Parser.succeed ()
                                    ]
                                |= Parser.oneOf
                                    [ Parser.succeed identity
                                        |. Parser.token (Parser.Token (String.repeat (indent + 4) " ") (ExpectingIndent (indent + 4)))
                                        |= Parser.withIndent (indent + 4) (Parser.inContext (InBlock name) (getParser child))

                                    -- If we're here, it's because the indentation failed.
                                    , Parser.succeed (\( pos, str ) -> DescribeUnexpected pos str)
                                        |= withRange (Parser.loop "" (indentedString (indent + 4)))
                                    ]
                        )
                )
        )


{-| -}
stub : String -> (Range Position -> result) -> Block result
stub name renderer =
    Block name
        (\desc ->
            case desc of
                DescribeStub actualBlockName range ->
                    if actualBlockName == name then
                        Ok (renderer range)

                    else
                        Err InvalidAst

                _ ->
                    Err InvalidAst
        )
        (Parser.map
            (\( range, _ ) ->
                DescribeStub name range
            )
         <|
            withRange
                (Parser.succeed ()
                    |. Parser.keyword (Parser.Token name (ExpectingBlockName name))
                    |. Parser.chompWhile (\c -> c == ' ')
                )
        )


{-| -}
record2 :
    String
    -> (Range Position -> one -> two -> data)
    -> Field one
    -> Field two
    -> Block data
record2 recordName renderer field1 field2 =
    Block recordName
        (\desc ->
            case desc of
                Record name pos fields ->
                    if name == recordName then
                        Ok (renderer pos)
                            |> Result.map2 (|>) (getField field1 fields)
                            |> Result.map2 (|>) (getField field2 fields)

                    else
                        Err InvalidAst

                _ ->
                    Err InvalidAst
        )
        (parseRecord recordName [ fieldParser field1, fieldParser field2 ])


{-| -}
record3 :
    String
    -> (Range Position -> one -> two -> three -> data)
    -> Field one
    -> Field two
    -> Field three
    -> Block data
record3 recordName renderer field1 field2 field3 =
    Block recordName
        (\desc ->
            case desc of
                Record name pos fields ->
                    if name == recordName then
                        Ok (renderer pos)
                            |> Result.map2 (|>) (getField field1 fields)
                            |> Result.map2 (|>) (getField field2 fields)
                            |> Result.map2 (|>) (getField field3 fields)

                    else
                        Err InvalidAst

                _ ->
                    Err InvalidAst
        )
        (parseRecord recordName [ fieldParser field1, fieldParser field2, fieldParser field3 ])


{-| -}
record4 :
    String
    -> (Range Position -> one -> two -> three -> four -> data)
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Block data
record4 recordName renderer field1 field2 field3 field4 =
    Block recordName
        (\desc ->
            case desc of
                Record name pos fields ->
                    if name == recordName then
                        Ok (renderer pos)
                            |> Result.map2 (|>) (getField field1 fields)
                            |> Result.map2 (|>) (getField field2 fields)
                            |> Result.map2 (|>) (getField field3 fields)
                            |> Result.map2 (|>) (getField field4 fields)

                    else
                        Err InvalidAst

                _ ->
                    Err InvalidAst
        )
        (parseRecord recordName
            [ fieldParser field1
            , fieldParser field2
            , fieldParser field3
            , fieldParser field4
            ]
        )


{-| -}
record5 :
    String
    -> (Range Position -> one -> two -> three -> four -> five -> data)
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Field five
    -> Block data
record5 recordName renderer field1 field2 field3 field4 field5 =
    Block recordName
        (\desc ->
            case desc of
                Record name pos fields ->
                    if name == recordName then
                        Ok (renderer pos)
                            |> Result.map2 (|>) (getField field1 fields)
                            |> Result.map2 (|>) (getField field2 fields)
                            |> Result.map2 (|>) (getField field3 fields)
                            |> Result.map2 (|>) (getField field4 fields)
                            |> Result.map2 (|>) (getField field5 fields)

                    else
                        Err InvalidAst

                _ ->
                    Err InvalidAst
        )
        (parseRecord recordName
            [ fieldParser field1
            , fieldParser field2
            , fieldParser field3
            , fieldParser field4
            , fieldParser field5
            ]
        )


{-| -}
record6 :
    String
    -> (Range Position -> one -> two -> three -> four -> five -> six -> data)
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Field five
    -> Field six
    -> Block data
record6 recordName renderer field1 field2 field3 field4 field5 field6 =
    Block recordName
        (\desc ->
            case desc of
                Record name pos fields ->
                    if name == recordName then
                        Ok (renderer pos)
                            |> Result.map2 (|>) (getField field1 fields)
                            |> Result.map2 (|>) (getField field2 fields)
                            |> Result.map2 (|>) (getField field3 fields)
                            |> Result.map2 (|>) (getField field4 fields)
                            |> Result.map2 (|>) (getField field5 fields)
                            |> Result.map2 (|>) (getField field6 fields)

                    else
                        Err InvalidAst

                _ ->
                    Err InvalidAst
        )
        (parseRecord recordName
            [ fieldParser field1
            , fieldParser field2
            , fieldParser field3
            , fieldParser field4
            , fieldParser field5
            , fieldParser field6
            ]
        )


{-| -}
record7 :
    String
    -> (Range Position -> one -> two -> three -> four -> five -> six -> seven -> data)
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Field five
    -> Field six
    -> Field seven
    -> Block data
record7 recordName renderer field1 field2 field3 field4 field5 field6 field7 =
    Block recordName
        (\desc ->
            case desc of
                Record name pos fields ->
                    if name == recordName then
                        Ok (renderer pos)
                            |> Result.map2 (|>) (getField field1 fields)
                            |> Result.map2 (|>) (getField field2 fields)
                            |> Result.map2 (|>) (getField field3 fields)
                            |> Result.map2 (|>) (getField field4 fields)
                            |> Result.map2 (|>) (getField field5 fields)
                            |> Result.map2 (|>) (getField field6 fields)
                            |> Result.map2 (|>) (getField field7 fields)

                    else
                        Err InvalidAst

                _ ->
                    Err InvalidAst
        )
        (parseRecord recordName
            [ fieldParser field1
            , fieldParser field2
            , fieldParser field3
            , fieldParser field4
            , fieldParser field5
            , fieldParser field6
            , fieldParser field7
            ]
        )


{-| -}
record8 :
    String
    -> (Range Position -> one -> two -> three -> four -> five -> six -> seven -> eight -> data)
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Field five
    -> Field six
    -> Field seven
    -> Field eight
    -> Block data
record8 recordName renderer field1 field2 field3 field4 field5 field6 field7 field8 =
    Block recordName
        (\desc ->
            case desc of
                Record name pos fields ->
                    if name == recordName then
                        Ok (renderer pos)
                            |> Result.map2 (|>) (getField field1 fields)
                            |> Result.map2 (|>) (getField field2 fields)
                            |> Result.map2 (|>) (getField field3 fields)
                            |> Result.map2 (|>) (getField field4 fields)
                            |> Result.map2 (|>) (getField field5 fields)
                            |> Result.map2 (|>) (getField field6 fields)
                            |> Result.map2 (|>) (getField field7 fields)
                            |> Result.map2 (|>) (getField field8 fields)

                    else
                        Err InvalidAst

                _ ->
                    Err InvalidAst
        )
        (parseRecord recordName
            [ fieldParser field1
            , fieldParser field2
            , fieldParser field3
            , fieldParser field4
            , fieldParser field5
            , fieldParser field6
            , fieldParser field7
            , fieldParser field8
            ]
        )


{-| -}
record9 :
    String
    -> (Range Position -> one -> two -> three -> four -> five -> six -> seven -> eight -> nine -> data)
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Field five
    -> Field six
    -> Field seven
    -> Field eight
    -> Field nine
    -> Block data
record9 recordName renderer field1 field2 field3 field4 field5 field6 field7 field8 field9 =
    Block recordName
        (\desc ->
            case desc of
                Record name pos fields ->
                    if name == recordName then
                        Ok (renderer pos)
                            |> Result.map2 (|>) (getField field1 fields)
                            |> Result.map2 (|>) (getField field2 fields)
                            |> Result.map2 (|>) (getField field3 fields)
                            |> Result.map2 (|>) (getField field4 fields)
                            |> Result.map2 (|>) (getField field5 fields)
                            |> Result.map2 (|>) (getField field6 fields)
                            |> Result.map2 (|>) (getField field7 fields)
                            |> Result.map2 (|>) (getField field8 fields)
                            |> Result.map2 (|>) (getField field9 fields)

                    else
                        Err InvalidAst

                _ ->
                    Err InvalidAst
        )
        (parseRecord recordName
            [ fieldParser field1
            , fieldParser field2
            , fieldParser field3
            , fieldParser field4
            , fieldParser field5
            , fieldParser field6
            , fieldParser field7
            , fieldParser field8
            , fieldParser field9
            ]
        )


{-| -}
record10 :
    String
    -> (Range Position -> one -> two -> three -> four -> five -> six -> seven -> eight -> nine -> ten -> data)
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Field five
    -> Field six
    -> Field seven
    -> Field eight
    -> Field nine
    -> Field ten
    -> Block data
record10 recordName renderer field1 field2 field3 field4 field5 field6 field7 field8 field9 field10 =
    Block recordName
        (\desc ->
            case desc of
                Record name pos fields ->
                    if name == recordName then
                        Ok (renderer pos)
                            |> Result.map2 (|>) (getField field1 fields)
                            |> Result.map2 (|>) (getField field2 fields)
                            |> Result.map2 (|>) (getField field3 fields)
                            |> Result.map2 (|>) (getField field4 fields)
                            |> Result.map2 (|>) (getField field5 fields)
                            |> Result.map2 (|>) (getField field6 fields)
                            |> Result.map2 (|>) (getField field7 fields)
                            |> Result.map2 (|>) (getField field8 fields)
                            |> Result.map2 (|>) (getField field9 fields)
                            |> Result.map2 (|>) (getField field10 fields)

                    else
                        Err InvalidAst

                _ ->
                    Err InvalidAst
        )
        (parseRecord recordName
            [ fieldParser field1
            , fieldParser field2
            , fieldParser field3
            , fieldParser field4
            , fieldParser field5
            , fieldParser field6
            , fieldParser field7
            , fieldParser field8
            , fieldParser field9
            , fieldParser field10
            ]
        )


parseRecord :
    String
    -> List ( String, Parser Context Problem ( String, Description ) )
    -> Parser Context Problem Description
parseRecord recordName fields =
    Parser.succeed
        (\( pos, foundFields ) ->
            Record recordName pos foundFields
        )
        |= withRange
            (Parser.getIndent
                |> Parser.andThen
                    (\indent ->
                        Parser.succeed identity
                            |. Parser.keyword (Parser.Token recordName (ExpectingBlockName recordName))
                            |. Parser.chompWhile (\c -> c == ' ')
                            |. Parser.chompIf (\c -> c == '\n') Newline
                            |= Parser.withIndent (indent + 4)
                                (Parser.loop
                                    ( fields
                                    , []
                                    )
                                    parseFields
                                )
                    )
            )


getField : Field value -> List ( String, Description ) -> Result AstError value
getField (Field name fieldBlock) fields =
    List.foldl (matchField name fieldBlock) (Err InvalidAst) fields


matchField targetName targetBlock ( name, description ) existing =
    case existing of
        Ok _ ->
            existing

        Err err ->
            if name == targetName then
                render targetBlock description

            else
                existing


{-| -}
parseFields :
    ( List ( String, Parser Context Problem ( String, Description ) ), List ( String, Description ) )
    -> Parser Context Problem (Parser.Step ( List ( String, Parser Context Problem ( String, Description ) ), List ( String, Description ) ) (List ( String, Description )))
parseFields ( fields, found ) =
    case fields of
        [] ->
            Parser.succeed (Parser.Done found)

        _ ->
            Parser.getIndent
                |> Parser.andThen
                    (\indent ->
                        Parser.oneOf
                            [ skipBlankLinesWith (Parser.Loop ( fields, found ))
                            , Parser.succeed identity
                                |. Parser.token (Parser.Token (String.repeat indent " ") (ExpectingIndent indent))
                                |= Parser.map
                                    (\( foundFieldname, fieldValue ) ->
                                        Parser.Loop
                                            ( List.filter
                                                (\( fieldParserName, _ ) -> fieldParserName /= foundFieldname)
                                                fields
                                            , ( foundFieldname, fieldValue ) :: found
                                            )
                                    )
                                    (Parser.oneOf (List.map Tuple.second fields))
                            ]
                    )


withFieldName name parser =
    Parser.getIndent
        |> Parser.andThen
            (\indent ->
                Parser.succeed Tuple.pair
                    |= Parser.map (always name) (Parser.keyword (Parser.Token name (ExpectingFieldName name)))
                    |. Parser.chompIf (\c -> c == ' ') Space
                    |. Parser.chompIf (\c -> c == '=') (Expecting "=")
                    |. Parser.chompIf (\c -> c == ' ') Space
                    |= Parser.withIndent (indent + 4) (Parser.inContext (InRecordField name) parser)
                    |. Parser.token (Parser.Token "\n" Newline)
            )


skipBlankLinesWith x =
    Parser.succeed x
        |. Parser.token (Parser.Token "\n" Newline)
        |. Parser.oneOf
            [ Parser.succeed ()
                |. Parser.backtrackable (Parser.chompWhile (\c -> c == ' '))
                |. Parser.backtrackable (Parser.token (Parser.Token "\n" Newline))
            , Parser.succeed ()
            ]


{-| -}
field : String -> Block value -> Field value
field name child =
    case child of
        Block blockName renderer childParser ->
            Field name (Block blockName renderer childParser)

        Value renderer childParser ->
            Field name (Value renderer childParser)


fieldParser : Field value -> ( String, Parser Context Problem ( String, Description ) )
fieldParser (Field name myBlock) =
    ( name
    , withFieldName
        name
        (getParser myBlock)
    )


fieldName : Field v -> String
fieldName (Field name _) =
    name


{-| -}
type Field value
    = Field String (Block value)


{-| -}
startWith : (Range Position -> start -> rest -> result) -> Block start -> Block rest -> Block result
startWith fn startBlock endBlock =
    Value
        (\desc ->
            case desc of
                StartsWith range startDescription endDescription ->
                    case render startBlock startDescription of
                        Err err ->
                            Err err

                        Ok startChild ->
                            case render endBlock endDescription of
                                Err err ->
                                    Err err

                                Ok endChild ->
                                    Ok
                                        (fn range startChild endChild)

                _ ->
                    Err InvalidAst
        )
        (Parser.succeed
            (\( range, ( begin, end ) ) ->
                StartsWith range begin end
            )
            |= withRange
                (Parser.succeed Tuple.pair
                    |= getParser startBlock
                    |= getParser endBlock
                )
        )


{-| Many blocks that are all at the same indentation level.
-}
manyOf : List (Block a) -> Block (List a)
manyOf blocks =
    let
        gatherParsers myBlock ( blks, vals ) =
            case myBlock of
                Block name _ blkParser ->
                    ( blkParser :: blks, vals )

                Value _ valueParser ->
                    ( blks, valueParser :: vals )

        ( childBlocks, childValues ) =
            List.foldl gatherParsers ( [], [] ) blocks

        blockParser =
            Parser.succeed identity
                |. Parser.token (Parser.Token "|" BlockStart)
                |. Parser.oneOf
                    [ Parser.chompIf (\c -> c == ' ') Space
                    , Parser.succeed ()
                    ]
                |= Parser.oneOf (List.reverse childBlocks)
    in
    Value
        (\desc ->
            let
                applyDesc description blck found =
                    case found of
                        Nothing ->
                            case render blck description of
                                Err err ->
                                    found

                                Ok rendered ->
                                    Just rendered

                        _ ->
                            found

                getRendered found existingResult =
                    case existingResult of
                        Err err ->
                            Err err

                        Ok existing ->
                            case List.foldl (applyDesc found) Nothing blocks of
                                Nothing ->
                                    Err InvalidAst

                                Just result ->
                                    Ok (result :: existing)
            in
            case desc of
                ManyOf range options found ->
                    List.foldl getRendered (Ok []) found
                        |> Result.map List.reverse

                _ ->
                    Err InvalidAst
        )
        (Parser.succeed
            (\( range, found ) ->
                -- TODO: the "posibilities" are probably a different type
                -- because we don't have actual data
                -- , only a description of what we'd expect.
                ManyOf range [] found
            )
            |= withRange
                (Parser.getIndent
                    |> Parser.andThen
                        (\indent ->
                            Parser.loop ( False, [] )
                                (blocksOrNewlines (Parser.oneOf (blockParser :: List.reverse childValues)) indent)
                        )
                )
        )


{-| -}
blocksOrNewlines : Parser Context Problem Description -> Int -> ( Bool, List Description ) -> Parser Context Problem (Parser.Step ( Bool, List Description ) (List Description))
blocksOrNewlines myParser indent ( parsedSomething, existing ) =
    Parser.oneOf
        [ Parser.end End
            |> Parser.map
                (\_ ->
                    Parser.Done (List.reverse existing)
                )

        -- Whitespace Line
        , skipBlankLinesWith (Parser.Loop ( True, existing ))
        , if not parsedSomething then
            -- First thing already has indentation accounted for.
            myParser
                |> Parser.map
                    (\foundBlock ->
                        Parser.Loop ( True, foundBlock :: existing )
                    )

          else
            Parser.oneOf
                [ Parser.succeed
                    (\foundBlock ->
                        Parser.Loop ( True, foundBlock :: existing )
                    )
                    |. Parser.token (Parser.Token (String.repeat indent " ") (ExpectingIndent indent))
                    |= myParser

                -- We reach here because the indentation parsing was not successful,
                -- meaning the indentation has been lowered and the block is done
                , Parser.succeed (Parser.Done (List.reverse existing))
                ]
        ]


{-| -}
oneOf : List (Block a) -> Block a
oneOf blocks =
    let
        gatherParsers myBlock ( blks, vals ) =
            case myBlock of
                Block name _ blkParser ->
                    ( blkParser :: blks, vals )

                Value _ valueParser ->
                    ( blks, valueParser :: vals )

        ( childBlocks, childValues ) =
            List.foldl gatherParsers ( [], [] ) blocks

        blockParser =
            Parser.succeed identity
                |. Parser.token (Parser.Token "|" BlockStart)
                |. Parser.oneOf
                    [ Parser.chompIf (\c -> c == ' ') Space
                    , Parser.succeed ()
                    ]
                |= Parser.oneOf (List.reverse childBlocks)

        applyDesc description blck found =
            case found of
                Nothing ->
                    case render blck description of
                        Err err ->
                            found

                        Ok rendered ->
                            Just rendered

                _ ->
                    found
    in
    Value
        (\desc ->
            case desc of
                OneOf range options found ->
                    case List.foldl (applyDesc found) Nothing blocks of
                        Nothing ->
                            Err InvalidAst

                        Just result ->
                            Ok result

                _ ->
                    Err InvalidAst
        )
        (Parser.succeed
            (\( range, found ) ->
                -- TODO: the "posibilities" are probably a different type because we don't have actual data
                -- , only a description of what we'd expect.
                OneOf range [] found
            )
            |= withRange
                (Parser.oneOf
                    (blockParser :: List.reverse childValues)
                )
        )


integer : Parser Context Problem Int
integer =
    Parser.oneOf
        [ Parser.succeed (\num -> negate num)
            |. Parser.token (Parser.Token "-" (Expecting "-"))
            |= Parser.int Integer InvalidNumber
        , Parser.int Integer InvalidNumber
        ]


floating : Parser Context Problem Float
floating =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.token (Parser.Token "-" (Expecting "-"))
            |= Parser.float FloatingPoint InvalidNumber
        , Parser.float FloatingPoint InvalidNumber
        ]


{-| -}
intBetween : Int -> Int -> Block Int
intBetween one two =
    let
        top =
            max one two

        bottom =
            min one two
    in
    Value
        (\desc ->
            case desc of
                DescribeIntBetween low high range i ->
                    Ok i

                _ ->
                    Err InvalidAst
        )
        (Parser.map
            (\( range, i ) ->
                if i >= bottom && i <= top then
                    DescribeIntBetween bottom top range i

                else
                    -- TODO: out of range, what do we do?
                    DescribeIntBetween bottom top range i
            )
            (withRange integer)
        )


{-| -}
floatBetween : Float -> Float -> Block Float
floatBetween one two =
    let
        top =
            max one two

        bottom =
            min one two
    in
    Value
        (\desc ->
            case desc of
                DescribeFloatBetween low high range i ->
                    Ok i

                _ ->
                    Err InvalidAst
        )
        (Parser.map
            (\( range, i ) ->
                if i >= bottom && i <= top then
                    DescribeFloatBetween bottom top range i

                else
                    -- TODO: out of range, what do we do?
                    DescribeFloatBetween bottom top range i
            )
            (withRange floating)
        )


{-| Parse either `True` or `False`.
-}
bool : Block Bool
bool =
    Value
        (\desc ->
            case desc of
                DescribeBoolean range i ->
                    Ok i

                _ ->
                    Err InvalidAst
        )
        (Parser.map
            (\( range, i ) ->
                DescribeBoolean range i
            )
            (withRange
                (Parser.oneOf
                    [ Parser.token (Parser.Token "True" (Expecting "True"))
                        |> Parser.map (always True)
                    , Parser.token (Parser.Token "False" (Expecting "False"))
                        |> Parser.map (always False)
                    ]
                )
            )
        )


int : Block Int
int =
    Value
        (\desc ->
            case desc of
                DescribeInteger range i ->
                    Ok i

                _ ->
                    Err InvalidAst
        )
        (Parser.succeed
            (\start val end ->
                DescribeInteger { start = start, end = end } val
            )
            |= getPosition
            |= integer
            |= getPosition
        )


float : Block Float
float =
    Value
        (\desc ->
            case desc of
                DescribeFloat range i ->
                    Ok i

                _ ->
                    Err InvalidAst
        )
        (Parser.succeed
            (\start val end ->
                DescribeFloat { start = start, end = end } val
            )
            |= getPosition
            |= floating
            |= getPosition
        )


{-| Parse multiple lines at the current indentation level.

For example:

    Mark.block "Poem"
        (\str -> str)
        Mark.multiline

Will parse the following:

    | Poem
        Whose woods these are I think I know.
        His house is in the village though;
        He will not see me stopping here
        To watch his woods fill up with snow.

Where `str` in the above function will be

    """Whose woods these are I think I know.
    His house is in the village though;
    He will not see me stopping here
    To watch his woods fill up with snow."""

-}
multiline : Block String
multiline =
    Value
        (\desc ->
            case desc of
                DescribeMultiline range i ->
                    Ok i

                _ ->
                    Err InvalidAst
        )
        (Parser.map
            (\( pos, str ) ->
                DescribeMultiline pos str
            )
            (withRange
                (Parser.getIndent
                    |> Parser.andThen
                        (\indent ->
                            Parser.loop "" (indentedString indent)
                        )
                )
            )
        )


indentedString : Int -> String -> Parser Context Problem (Parser.Step String String)
indentedString indent found =
    Parser.oneOf
        -- First line, indentation is already handled by the block constructor.
        [ if found == "" then
            Parser.succeed (\str -> Parser.Loop (found ++ str))
                |= Parser.getChompedString
                    (Parser.chompWhile
                        (\c -> c /= '\n')
                    )

          else
            Parser.succeed (\str -> Parser.Loop (found ++ str))
                |. Parser.token (Parser.Token (String.repeat indent " ") (ExpectingIndent indent))
                |= Parser.getChompedString
                    (Parser.chompWhile
                        (\c -> c /= '\n')
                    )
        , Parser.succeed
            (\extra ->
                Parser.Loop <|
                    if extra then
                        found ++ "\n\n"

                    else
                        found ++ "\n"
            )
            |. Parser.token (Parser.Token "\n" Newline)
            |= Parser.oneOf
                [ Parser.succeed True
                    |. Parser.backtrackable (Parser.chompWhile (\c -> c == ' '))
                    |. Parser.backtrackable (Parser.token (Parser.Token "\n" Newline))
                , Parser.succeed False
                ]
        , Parser.succeed (Parser.Done found)
        ]


string : Block String
string =
    Value
        (\desc ->
            case desc of
                DescribeString range i ->
                    Ok i

                _ ->
                    Err InvalidAst
        )
        (Parser.succeed
            (\start val end ->
                DescribeString { start = start, end = end } val
            )
            |= getPosition
            |= Parser.getChompedString
                (Parser.chompWhile
                    (\c -> c /= '\n')
                )
            |= getPosition
        )


exactly : String -> value -> Block value
exactly key value =
    Value
        (\desc ->
            case desc of
                DescribeStringExactly range existingKey ->
                    if key == existingKey then
                        Ok value

                    else
                        Err InvalidAst

                _ ->
                    Err InvalidAst
        )
        (Parser.succeed
            (\start _ end ->
                DescribeStringExactly { start = start, end = end } key
            )
            |= getPosition
            |= Parser.token (Parser.Token key (Expecting key))
            |= getPosition
        )


{-| Parse an ISO-8601 date string.

Format: `YYYY-MM-DDTHH:mm:ss.SSSZ`

Though you don't need to specify all segments, so `YYYY-MM-DD` works as well.

Results in a `Posix` integer, which works well with [elm/time](https://package.elm-lang.org/packages/elm/time/latest/).

-}
date : Block Time.Posix
date =
    Value
        (\desc ->
            case desc of
                DescribeDate range i ->
                    Ok i

                _ ->
                    Err InvalidAst
        )
        (Parser.map
            (\( pos, parsedPosix ) ->
                case parsedPosix of
                    Err str ->
                        DescribeBadDate pos str

                    Ok posix ->
                        DescribeDate pos posix
            )
            (withRange
                (Parser.getChompedString
                    (Parser.chompWhile
                        (\c -> c /= '\n')
                    )
                    |> Parser.andThen
                        (\str ->
                            case Iso8601.toTime str of
                                Err err ->
                                    Parser.succeed
                                        (Err str)

                                Ok parsedPosix ->
                                    Parser.succeed (Ok parsedPosix)
                        )
                )
            )
        )


type AstError
    = InvalidAst


{-| Replace a string with another string. This can be useful to have shortcuts to unicode characters.

For example, in `Mark.Default`, this is used to replace `...` with the unicode ellipses character: `…`.

-}
replacement : String -> String -> Replacement
replacement =
    Replacement


{-| A balanced replacement. This is used in `Mark.Default` to do auto-curly quotes.

    Mark.balanced
        { start = ( "\"", "“" )
        , end = ( "\"", "”" )
        }

-}
balanced :
    { start : ( String, String )
    , end : ( String, String )
    }
    -> Replacement
balanced =
    Balanced


empty : Text
empty =
    Text [] ""


{-| -}
emptyText : TextAccumulator rendered
emptyText =
    TextAccumulator
        { text = empty
        , rendered = []
        , balancedReplacements = []
        }



{- Nested Lists -}


{-| = indentLevel icon space content
| indentLevel content

Where the second variation can only occur if the indentation is larger than the previous one.

A list item started with a list icon.

    If indent stays the same
    -> add to items at the current stack

    if ident increases
    -> create a new level in the stack

    if ident decreases
    -> close previous group
    ->

    <list>
        <*item>
            <txt> -> add to head sections
            <txt> -> add to head sections
            <item> -> add to head sections
            <item> -> add to head sections
                <txt> -> add to content
                <txt> -> add to content
                <item> -> add to content
                <item> -> add to content
            <item> -> add to content

        <*item>
        <*item>

    Section
        [ IconSection
            { icon = *
            , sections =
                [ Text
                , Text
                , IconSection Text
                , IconSection
                    [ Text
                    , Text
                    , item
                    , item
                    ]
                ]
            }
        , Icon -> Content
        , Icon -> Content
        ]

-}
type TreeBuilder item
    = TreeBuilder
        { previousIndent : Int
        , levels :
            -- (mostRecent :: remaining)
            List (Level item)
        }


{-| -}
type Level item
    = Level (List (Nested item))


{-| -}
type Nested item
    = Nested
        { content : item
        , children :
            List (Nested item)
        }


emptyTreeBuilder : TreeBuilder item
emptyTreeBuilder =
    TreeBuilder
        { previousIndent = 0
        , levels = []
        }


{-| It can be useful to parse a tree structure. For example, here's a nested list.

    | List
        - item one
        - item two
            - nested item two
            additional text for nested item two
        - item three
            - nested item three

In order to parse the above, you could define a block as

    Mark.block "List"
        (\(Nested nested) ->
            -- Do something with nested.content and nested.children
        )
        (Mark.nested
            { item = text
            , start = Mark.exactly "-" ()
            }
        )

**Note** the indentation is always a multiple of 4.
**Another Note** `text` in the above code is defined elsewhere.

-}
nested :
    { item : Block item
    , start : Block icon
    }
    -> Block (List (Nested ( icon, List item )))
nested config =
    Value
        (\description ->
            case Debug.log "mapping" description of
                DescribeTree range nestedDescriptors ->
                    reduceRender (renderTreeNodeSmall config) nestedDescriptors

                _ ->
                    Err InvalidAst
        )
        (Parser.getIndent
            |> Parser.andThen
                (\baseIndent ->
                    Parser.map
                        (\( pos, result ) ->
                            DescribeTree pos <| buildTree baseIndent result
                        )
                        (withRange
                            (Parser.loop
                                ( { base = baseIndent
                                  , prev = baseIndent
                                  }
                                , []
                                )
                                (indentedBlocksOrNewlines config.start config.item)
                            )
                        )
                )
        )


reduceRender : (thing -> Result AstError other) -> List thing -> Result AstError (List other)
reduceRender fn list =
    List.foldl
        (\x gathered ->
            case gathered of
                Err _ ->
                    gathered

                Ok remain ->
                    case fn x of
                        Err err ->
                            Err err

                        Ok success ->
                            Ok (success :: remain)
        )
        (Ok [])
        list
        |> Result.map List.reverse


renderTreeNodeSmall :
    { item : Block item
    , start : Block icon
    }
    -> Nested ( Description, List Description )
    -> Result AstError (Nested ( icon, List item ))
renderTreeNodeSmall config (Nested cursor) =
    let
        renderedChildren =
            reduceRender (renderTreeNodeSmall config) cursor.children

        renderedContent =
            case cursor.content of
                ( icon, content ) ->
                    ( render config.start icon
                    , reduceRender (render config.item) content
                    )
    in
    case renderedContent of
        ( Ok icon, Ok content ) ->
            case renderedChildren of
                Err err ->
                    Err err

                Ok successfullyRenderedChildren ->
                    Ok
                        (Nested
                            { content = ( icon, content )
                            , children = successfullyRenderedChildren
                            }
                        )

        ( Err err, _ ) ->
            Err err

        ( _, Err err ) ->
            Err err


buildTree baseIndent items =
    let
        gather ( indent, icon, item ) (TreeBuilder builder) =
            addItem (indent - baseIndent) ( icon, item ) (TreeBuilder builder)

        groupByIcon ( indent, maybeIcon, item ) maybeCursor =
            case maybeCursor of
                Nothing ->
                    case maybeIcon of
                        Just icon ->
                            Just
                                { indent = indent
                                , icon = icon
                                , items = [ item ]
                                , accumulated = []
                                }

                        Nothing ->
                            -- Because of how the code runs, we have a tenuous guarantee that this branch won't execute.
                            -- Not entirely sure how to make the types work to eliminate this.
                            Nothing

                Just cursor ->
                    Just <|
                        case maybeIcon of
                            Nothing ->
                                { indent = cursor.indent
                                , icon = cursor.icon
                                , items = item :: cursor.items
                                , accumulated = cursor.accumulated
                                }

                            Just icon ->
                                { indent = indent
                                , icon = icon
                                , items = [ item ]
                                , accumulated =
                                    ( cursor.indent, cursor.icon, cursor.items )
                                        :: cursor.accumulated
                                }

        finalizeGrouping maybeCursor =
            case maybeCursor of
                Nothing ->
                    []

                Just cursor ->
                    case cursor.items of
                        [] ->
                            cursor.accumulated

                        _ ->
                            ( cursor.indent, cursor.icon, cursor.items )
                                :: cursor.accumulated

        tree =
            items
                |> List.foldl groupByIcon Nothing
                |> finalizeGrouping
                |> List.reverse
                |> List.foldl gather emptyTreeBuilder
    in
    case tree of
        TreeBuilder builder ->
            renderLevels builder.levels


type alias NestedIndex =
    { base : Int
    , prev : Int
    }


{-| Results in a flattened version of the parsed list.

    ( 0, (), [ "item one" ] )

    ( 0, (), [ "item two" ] )

    ( 4, (), [ "nested item two", "additional text for nested item two" ] )

    ( 0, (), [ "item three" ] )

    ( 4, (), [ "nested item three" ] )

-}
indentedBlocksOrNewlines :
    Block icon
    -> Block thing
    -> ( NestedIndex, List ( Int, Maybe Description, Description ) )
    -> Parser Context Problem (Parser.Step ( NestedIndex, List ( Int, Maybe Description, Description ) ) (List ( Int, Maybe Description, Description )))
indentedBlocksOrNewlines icon item ( indent, existing ) =
    let
        _ =
            Debug.log "indented blocks/nl" ( indent, existing )
    in
    Parser.oneOf
        [ Parser.end End
            |> Parser.map
                (\_ ->
                    Parser.Done (List.reverse existing)
                )

        -- Whitespace Line
        , skipBlankLinesWith (Parser.Loop ( indent, existing ))
        , case existing of
            [] ->
                -- Indent is already parsed by the block constructor for first element, skip it
                Parser.succeed
                    (\foundIcon foundBlock ->
                        let
                            _ =
                                Debug.log "parse first" ( foundIcon, foundBlock )

                            newIndex =
                                { prev = indent.base
                                , base = indent.base
                                }
                        in
                        Parser.Loop ( newIndex, ( indent.base, Just foundIcon, foundBlock ) :: existing )
                    )
                    |= getParser icon
                    |= getParser item

            _ ->
                Parser.oneOf
                    [ -- block with required indent
                      expectIndentation indent.base indent.prev
                        |> Parser.andThen
                            (\newIndent ->
                                -- If the indent has changed, then the delimiter is required
                                Parser.withIndent newIndent <|
                                    Parser.oneOf
                                        ((Parser.succeed
                                            (\iconResult itemResult ->
                                                let
                                                    newIndex =
                                                        { prev = newIndent
                                                        , base = indent.base
                                                        }
                                                in
                                                Parser.Loop
                                                    ( newIndex
                                                    , ( newIndent, Just iconResult, itemResult ) :: existing
                                                    )
                                            )
                                            |= getParser icon
                                            |= getParser item
                                         )
                                            :: (if newIndent - 4 == indent.prev then
                                                    [ getParser item
                                                        |> Parser.map
                                                            (\foundBlock ->
                                                                let
                                                                    newIndex =
                                                                        { prev = indent.prev
                                                                        , base = indent.base
                                                                        }
                                                                in
                                                                Parser.Loop
                                                                    ( newIndex
                                                                    , ( indent.prev, Nothing, foundBlock ) :: existing
                                                                    )
                                                            )
                                                    ]

                                                else
                                                    []
                                               )
                                        )
                            )

                    -- We reach here because the indentation parsing was not successful,
                    -- This means any issues are handled by whatever parser comes next.
                    , Parser.succeed (Parser.Done (List.reverse existing))
                    ]
        ]


{-| We only expect nearby indentations.

We can't go below the `base` indentation.

Based on the previous indentation:

  - previous - 4
  - previous
  - previous + 4

If we don't match the above rules, we might want to count the mismatched number.

-}
expectIndentation : Int -> Int -> Parser Context Problem Int
expectIndentation base previous =
    Parser.succeed Tuple.pair
        |= Parser.oneOf
            ([ Parser.succeed (previous + 4)
                |. Parser.token (Parser.Token (String.repeat (previous + 4) " ") (ExpectingIndent (previous + 4)))
             , Parser.succeed previous
                |. Parser.token (Parser.Token (String.repeat previous " ") (ExpectingIndent previous))
             ]
                ++ descending base previous
            )
        |= Parser.getChompedString (Parser.chompWhile (\c -> c == ' '))
        |> Parser.andThen
            (\( indentLevel, extraSpaces ) ->
                if extraSpaces == "" then
                    Parser.succeed indentLevel

                else
                    Parser.problem
                        (ExpectingIndent (base + indentLevel))
            )


{-| Parse all indentation levels between `prev` and `base` in increments of 4.
-}
descending : Int -> Int -> List (Parser Context Problem Int)
descending base prev =
    if prev <= base then
        []

    else
        List.reverse
            (List.map
                (\x ->
                    let
                        level =
                            base + (x * 4)
                    in
                    Parser.succeed level
                        |. Parser.token (Parser.Token (String.repeat level " ") (ExpectingIndent level))
                )
                (List.range 0 (((prev - 4) - base) // 4))
            )


{-| A list item started with a list icon.

If indent stays the same
-> add to items at the current stack

if ident increases
-> create a new level in the stack

if ident decreases
-> close previous group
->

    1 Icon
        1.1 Content
        1.2 Icon
        1.3 Icon
           1.3.1 Icon

        1.4

    2 Icon

    Steps =
    []

    [ Level [ Item 1. [] ]
    ]

    [ Level [ Item 1.1 ]
    , Level [ Item 1. [] ]
    ]

    [ Level [ Item 1.2, Item 1.1 ]
    , Level [ Item 1. [] ]
    ]

    [ Level [ Item 1.3, Item 1.2, Item 1.1 ]
    , Level [ Item 1. [] ]
    ]

    [ Level [ Item 1.3.1 ]
    , Level [ Item 1.3, Item 1.2, Item 1.1 ]
    , Level [ Item 1. [] ]
    ]


    [ Level [ Item 1.4, Item 1.3([ Item 1.3.1 ]), Item 1.2, Item 1.1 ]
    , Level [ Item 1. [] ]
    ]

    [ Level [ Item 2., Item 1. (Level [ Item 1.4, Item 1.3([ Item 1.3.1 ]), Item 1.2, Item 1.1 ]) ]
    ]

-}
addItem :
    Int
    -> node
    -> TreeBuilder node
    -> TreeBuilder node
addItem indent content (TreeBuilder builder) =
    let
        newItem =
            Nested
                { children = []
                , content = content
                }

        deltaLevel =
            indent
                - builder.previousIndent

        addToLevel brandNewItem levels =
            case levels of
                [] ->
                    [ Level
                        [ brandNewItem ]
                    ]

                (Level lvl) :: remaining ->
                    Level (newItem :: lvl)
                        :: remaining
    in
    case builder.levels of
        [] ->
            TreeBuilder
                { previousIndent = indent
                , levels =
                    [ Level
                        [ newItem ]
                    ]
                }

        (Level lvl) :: remaining ->
            if deltaLevel == 0 then
                -- add to current level
                TreeBuilder
                    { previousIndent = indent
                    , levels =
                        Level (newItem :: lvl)
                            :: remaining
                    }

            else if deltaLevel > 0 then
                -- add new level
                TreeBuilder
                    { previousIndent = indent
                    , levels =
                        Level [ newItem ]
                            :: Level lvl
                            :: remaining
                    }

            else
                -- We've dedented, so we need to first collapse the current level
                -- into the one below, then add an item to that level
                TreeBuilder
                    { previousIndent = indent
                    , levels =
                        collapseLevel (abs deltaLevel // 4) builder.levels
                            |> addToLevel newItem
                    }


{-|

    1.
        1.1
    2.


    Steps =
    []

    [ Level [ Item 1. [] ]
    ]

    [ Level [ Item 1.1 ]
    , Level [ Item 1. [] ]
    ]

    -- collapse into lower level
    [ Level [ Item 1. [ Item 1.1 ] ]
    ]

    -- add new item
    [ Level [ Item 2, Item 1. [ Item 1.1 ] ]
    ]

-}
collapseLevel : Int -> List (Level item) -> List (Level item)
collapseLevel num levels =
    if num == 0 then
        levels

    else
        case levels of
            [] ->
                levels

            (Level topLevel) :: (Level ((Nested lowerItem) :: lower)) :: remaining ->
                collapseLevel (num - 1) <|
                    Level
                        (Nested
                            { lowerItem
                                | children = topLevel ++ lowerItem.children
                            }
                            :: lower
                        )
                        :: remaining

            _ ->
                levels


renderLevels levels =
    case levels of
        [] ->
            []

        _ ->
            case collapseLevel (List.length levels - 1) levels of
                [] ->
                    []

                (Level top) :: ignore ->
                    -- We just collapsed everything down to the top level.
                    List.foldl rev [] top


reverseTree (Nested nest) =
    Nested
        { content = Tuple.mapSecond List.reverse nest.content
        , children =
            List.foldl rev [] nest.children
        }


rev nest found =
    reverseTree nest :: found



{- Text Parsing -}


{-| Handling formatted text is a little more involved than may be initially apparent.

Text styling can be overlapped such as

    /My italicized sentence can have *bold*/

In order to render this, the above sentence is chopped up into `Text` fragments that can have multiple styles active.

  - `view` is the function to render an individual fragment.
  - `inlines` are custom inline blocks. These are how links are implemented in `Mark.Default`!
  - `replacements` will replace characters before rendering. For example, we can replace `...` with the real ellipses unicode character, `…`.

**Note** check out `Mark.Default.text` to see an example.

-}
text :
    { view : Range Position -> Text -> rendered
    , inlines : List (Inline rendered)
    , replacements : List Replacement
    }
    -> Block (List rendered)
text options =
    Value
        (renderText options)
        (getPosition
            |> Parser.andThen
                (\pos ->
                    styledTextParser
                        { inlines = List.map getPotentialInline options.inlines
                        , replacements = options.replacements
                        }
                        pos
                        []
                        []
                )
        )


getPotentialInline (Inline name renderer potential) =
    -- TODO: allow styles to be passed through the custom inline barrier
    potential []


renderText :
    { view : Range Position -> Text -> rendered
    , inlines : List (Inline rendered)
    , replacements : List Replacement
    }
    -> Description
    -> Result AstError (List rendered)
renderText options description =
    case description of
        DescribeText range textNodes ->
            -- TODO: Capture individual Failures
            List.foldl (renderTextComponent options) (Ok []) textNodes
                |> Result.map List.reverse

        _ ->
            Err InvalidAst


renderTextComponent options comp existing =
    case existing of
        Err _ ->
            existing

        Ok found ->
            case comp of
                Styled range textEl ->
                    Ok (options.view range textEl :: found)

                DescribeInline name range foundInline ->
                    case List.foldl (renderInline name range (List.reverse foundInline)) (Err InvalidAst) options.inlines of
                        Err err ->
                            Err err

                        Ok list ->
                            Ok (list ++ found)


renderInline name range pieces (Inline inlineName inlineRenderer inlinePotential) found =
    case found of
        Ok _ ->
            found

        Err error ->
            if name == inlineName then
                -- inlineRenderer
                inlineRenderer pieces

            else
                found


{-| -}
type TextCursor
    = TextCursor
        { current : Text
        , start : Position
        , found : List TextDescription
        , balancedReplacements : List String
        }


styledTextParser :
    { inlines : List PotentialInline
    , replacements : List Replacement
    }
    -> Position
    -> List Style
    -> List Char
    -> Parser Context Problem Description
styledTextParser options startingPos inheritedStyles until =
    let
        vacantText =
            TextCursor
                { current = Text inheritedStyles ""
                , found = []
                , start = startingPos
                , balancedReplacements = []
                }

        untilStrings =
            List.map String.fromChar until

        meaningful =
            '\\' :: '\n' :: until ++ stylingChars ++ replacementStartingChars options.replacements
    in
    Parser.oneOf
        [ -- Parser.chompIf (\c -> c == ' ') CantStartTextWithSpace
          -- -- TODO: return error description
          -- |> Parser.andThen
          --     (\_ ->
          --         Parser.problem CantStartTextWithSpace
          --     )
          Parser.map
            (\( pos, textNodes ) ->
                DescribeText pos textNodes
            )
          <|
            withRange
                (Parser.loop vacantText
                    (styledTextParserLoop options meaningful untilStrings)
                )
        ]


{-| -}
styledTextParserLoop :
    { inlines : List PotentialInline
    , replacements : List Replacement
    }
    -> List Char
    -> List String
    -> TextCursor
    -> Parser Context Problem (Parser.Step TextCursor (List TextDescription))
styledTextParserLoop options meaningful untilStrings found =
    Parser.oneOf
        [ Parser.oneOf (replace options.replacements found)
            |> Parser.map Parser.Loop

        -- If a char matches the first character of a replacement,
        -- but didn't match the full replacement captured above,
        -- then stash that char.
        , Parser.oneOf (almostReplacement options.replacements found)
            |> Parser.map Parser.Loop

        -- Capture style command characters
        , Parser.succeed
            (Parser.Loop << changeStyle options found)
            |= Parser.oneOf
                [ Parser.map (always (Just Italic)) (Parser.token (Parser.Token "/" (Expecting "/")))
                , Parser.map (always (Just Strike)) (Parser.token (Parser.Token "~" (Expecting "~")))
                , Parser.map (always (Just Bold)) (Parser.token (Parser.Token "*" (Expecting "*")))
                ]

        -- Custom inline block
        , Parser.succeed
            (\start rendered end ->
                let
                    current =
                        case changeStyle options found Nothing of
                            TextCursor accum ->
                                accum
                in
                Parser.Loop
                    (TextCursor
                        { found = rendered :: current.found
                        , start = end

                        -- TODO: This should inherit formatting from the inline parser
                        , current = empty
                        , balancedReplacements = current.balancedReplacements
                        }
                    )
            )
            |= getPosition
            |. Parser.token
                (Parser.Token "{" InlineStart)
            |= Parser.oneOf
                (List.map
                    (\(PotentialInline inlineName inlineComponents) ->
                        Parser.inContext
                            (InInline inlineName)
                            (parseInline inlineName inlineComponents)
                    )
                    options.inlines
                )
            |. Parser.token (Parser.Token "}" InlineEnd)
            |= getPosition
        , -- chomp until a meaningful character
          Parser.chompWhile
            (\c ->
                not (List.member c meaningful)
            )
            |> Parser.getChompedString
            |> Parser.andThen
                (\new ->
                    if new == "" || new == "\n" then
                        case changeStyle options found Nothing of
                            TextCursor txt ->
                                let
                                    styling =
                                        case txt.current of
                                            Text s _ ->
                                                s
                                in
                                -- TODO: What to do on unclosed styling?
                                -- if List.isEmpty styling then
                                Parser.succeed (Parser.Done (List.reverse txt.found))
                        -- else
                        -- Parser.problem (UnclosedStyles styling)

                    else
                        Parser.succeed (Parser.Loop (addText new found))
                )
        ]


parseInline : String -> List PotentialInlineVal -> Parser Context Problem TextDescription
parseInline name components =
    case components of
        [] ->
            Parser.succeed (\( range, _ ) -> DescribeInline name range [])
                |= withRange (Parser.keyword (Parser.Token name (ExpectingInlineName name)))

        _ ->
            Parser.succeed (\( range, foundComponents ) -> DescribeInline name range foundComponents)
                |= withRange
                    (Parser.succeed identity
                        |. Parser.keyword (Parser.Token name (ExpectingInlineName name))
                        |. Parser.chompWhile (\c -> c == ' ')
                        |= Parser.loop ( components, [] ) parseInlineComponents
                    )


parseInlineComponents ( components, found ) =
    case components of
        [] ->
            Parser.succeed (Parser.Done (List.reverse found))

        current :: remaining ->
            case current of
                PotentialInlineString inlineName ->
                    Parser.succeed
                        (\start str end ->
                            Parser.Loop
                                ( remaining
                                , DescribeInlineString
                                    inlineName
                                    { start = start
                                    , end = end
                                    }
                                    str
                                    :: found
                                )
                        )
                        |. Parser.chompIf (\c -> c == '|') (Expecting "|")
                        |. Parser.chompWhile (\c -> c == ' ')
                        |= getPosition
                        |. Parser.keyword
                            (Parser.Token
                                inlineName
                                (ExpectingFieldName inlineName)
                            )
                        |. Parser.chompWhile (\c -> c == ' ')
                        |. Parser.chompIf (\c -> c == '=') (Expecting "=")
                        |. Parser.chompWhile (\c -> c == ' ')
                        |= Parser.getChompedString
                            (Parser.chompWhile (\c -> c /= '|' && c /= '}'))
                        |= getPosition

                PotentialInlineText ->
                    Parser.succeed
                        (\start str end ->
                            Parser.Loop
                                ( remaining
                                , DescribeInlineText
                                    { start = start
                                    , end = end
                                    }
                                    str
                                    :: found
                                )
                        )
                        |. Parser.chompIf (\c -> c == '|') (Expecting "|")
                        |. Parser.chompWhile (\c -> c == ' ')
                        |= getPosition
                        |= Parser.loop [] (parseInlineText { replacements = [] })
                        |= getPosition


{-| -}
parseInlineText :
    { replacements : List Replacement
    }
    -> List Text
    -> Parser Context Problem (Parser.Step (List Text) (List Text))
parseInlineText options found =
    Parser.oneOf
        [ --     Parser.oneOf (replace options.replacements found)
          --     |> Parser.map Parser.Loop
          -- -- If a char matches the first character of a replacement,
          -- -- but didn't match the full replacement captured above,
          -- -- then stash that char.
          -- , Parser.oneOf (almostReplacement options.replacements found)
          --     |> Parser.map Parser.Loop
          -- Capture style command characters
          Parser.succeed
            (Parser.Loop << changeStyleOnText found)
            |= Parser.oneOf
                [ Parser.map (always Italic) (Parser.token (Parser.Token "/" (Expecting "/")))

                -- , Parser.map (always rline)) (Parser.token (Parser.Token "_" (Expecting "_")))
                , Parser.map (always Strike) (Parser.token (Parser.Token "~" (Expecting "~")))
                , Parser.map (always Bold) (Parser.token (Parser.Token "*" (Expecting "*")))

                -- , Parser.map (always (Just Code)) (Parser.token (Parser.Token "`" (Expecting "`")))
                ]
        , -- chomp until a meaningful character
          Parser.chompWhile
            (\c ->
                not (List.member c [ '}', '/', '|', '*', '~' ])
            )
            |> Parser.getChompedString
            |> Parser.andThen
                (\new ->
                    if new == "" || new == "\n" then
                        -- TODO: Warning about unclosed styles
                        Parser.succeed (Parser.Done (List.reverse found))

                    else
                        Parser.succeed (Parser.Loop (addTextToText new found))
                )
        ]


currentStyles (TextAccumulator formatted) =
    case formatted.text of
        Text s _ ->
            s


{-| -}
almostReplacement : List Replacement -> TextCursor -> List (Parser Context Problem TextCursor)
almostReplacement replacements existing =
    let
        captureChar char =
            Parser.succeed
                (\c ->
                    addText c existing
                )
                |= Parser.getChompedString
                    (Parser.chompIf (\c -> c == char && char /= '{' && char /= '*' && char /= '/') EscapedChar)

        first repl =
            case repl of
                Replacement x y ->
                    firstChar x

                Balanced range ->
                    firstChar (Tuple.first range.start)

        allFirstChars =
            List.filterMap first replacements
    in
    List.map captureChar allFirstChars


{-| **Reclaimed typography**

This function will replace certain characters with improved typographical ones.
Escaping a character will skip the replacement.

    -> "<>" -> a non-breaking space.
        - This can be used to glue words together so that they don't break
        - It also avoids being used for spacing like `&nbsp;` because multiple instances will collapse down to one.
    -> "--" -> "en-dash"
    -> "---" -> "em-dash".
    -> Quotation marks will be replaced with curly quotes.
    -> "..." -> ellipses

-}
replace : List Replacement -> TextCursor -> List (Parser Context Problem TextCursor)
replace replacements existing =
    let
        -- Escaped characters are captured as-is
        escaped =
            Parser.succeed
                (\esc ->
                    existing
                        |> addText esc
                )
                |. Parser.token
                    (Parser.Token "\\" Escape)
                |= Parser.getChompedString
                    (Parser.chompIf (always True) EscapedChar)

        replaceWith repl =
            case repl of
                Replacement x y ->
                    Parser.succeed
                        (\_ ->
                            addText y existing
                        )
                        |. Parser.token (Parser.Token x (Expecting x))
                        |= Parser.succeed ()

                Balanced range ->
                    let
                        balanceCache =
                            case existing of
                                TextCursor cursor ->
                                    cursor.balancedReplacements

                        id =
                            balanceId range
                    in
                    -- TODO: implement range replacement
                    if List.member id balanceCache then
                        case range.end of
                            ( x, y ) ->
                                Parser.succeed
                                    (addText y existing
                                        |> removeBalance id
                                    )
                                    |. Parser.token (Parser.Token x (Expecting x))

                    else
                        case range.start of
                            ( x, y ) ->
                                Parser.succeed
                                    (addText y existing
                                        |> addBalance id
                                    )
                                    |. Parser.token (Parser.Token x (Expecting x))
    in
    escaped :: List.map replaceWith replacements


balanceId balance =
    let
        join ( x, y ) =
            x ++ y
    in
    join balance.start ++ join balance.end


stylingChars =
    [ '~'

    -- , '_'
    , '/'
    , '*'
    , '\n'
    , '{'

    -- , '`'
    ]


firstChar str =
    case String.uncons str of
        Nothing ->
            Nothing

        Just ( fst, _ ) ->
            Just fst


replacementStartingChars replacements =
    let
        first repl =
            case repl of
                Replacement x y ->
                    firstChar x

                Balanced range ->
                    firstChar (Tuple.first range.start)
    in
    List.filterMap first replacements


addBalance id (TextCursor cursor) =
    TextCursor <|
        { cursor | balancedReplacements = id :: cursor.balancedReplacements }


removeBalance id (TextCursor cursor) =
    TextCursor <|
        { cursor | balancedReplacements = List.filter ((/=) id) cursor.balancedReplacements }


addTextToText newString textNodes =
    case textNodes of
        [] ->
            [ Text [] newString ]

        (Text styles txt) :: remaining ->
            Text styles (txt ++ newString) :: remaining


addText newTxt (TextCursor cursor) =
    case cursor.current of
        Text styles txt ->
            TextCursor { cursor | current = Text styles (txt ++ newTxt) }


changeStyle options (TextCursor cursor) maybeStyleToken =
    let
        cursorText =
            case cursor.current of
                Text _ txt ->
                    txt

        newText =
            case maybeStyleToken of
                Nothing ->
                    cursor.current

                Just sty ->
                    case sty of
                        Bold ->
                            flipStyle Bold cursor.current

                        Italic ->
                            flipStyle Italic cursor.current

                        Strike ->
                            flipStyle Strike cursor.current
    in
    if cursorText == "" then
        TextCursor
            { found = cursor.found
            , current = newText
            , start = cursor.start
            , balancedReplacements = cursor.balancedReplacements
            }

    else
        let
            end =
                measure cursor.start cursorText
        in
        TextCursor
            { found =
                Styled
                    { start = cursor.start
                    , end = end
                    }
                    cursor.current
                    :: cursor.found
            , start = end
            , current = newText
            , balancedReplacements = cursor.balancedReplacements
            }


changeStyleOnText textNodes styleToken =
    case textNodes of
        [] ->
            case styleToken of
                Bold ->
                    [ Text [ Bold ] "" ]

                Italic ->
                    [ Text [ Italic ] "" ]

                Strike ->
                    [ Text [ Strike ] "" ]

        current :: remaining ->
            let
                newText =
                    case styleToken of
                        Bold ->
                            flipStyle Bold current

                        Italic ->
                            flipStyle Italic current

                        Strike ->
                            flipStyle Strike current
            in
            newText :: current :: remaining


measure start textStr =
    let
        len =
            String.length textStr
    in
    { start
        | offset = start.offset + len
        , column = start.column + len
    }


flipStyle newStyle textStyle =
    case textStyle of
        Text styles str ->
            if List.member newStyle styles then
                Text (List.filter ((/=) newStyle) styles) ""

            else
                Text (newStyle :: styles) ""



{- PARSER HELPERS -}


withRange : Parser Context Problem thing -> Parser Context Problem ( Range Position, thing )
withRange parser =
    Parser.succeed
        (\start val end ->
            ( { start = start, end = end }, val )
        )
        |= getPosition
        |= parser
        |= getPosition


peek : String -> Parser c p thing -> Parser c p thing
peek name parser =
    Parser.succeed
        (\start val end src ->
            let
                highlight =
                    String.repeat (start.column - 1) " " ++ String.repeat (max 0 (end.column - start.column)) "^"

                fullLine =
                    String.slice (max 0 (start.offset - start.column)) end.offset src

                _ =
                    Debug.log name
                        fullLine

                _ =
                    Debug.log name
                        highlight
            in
            val
        )
        |= getPosition
        |= parser
        |= getPosition
        |= Parser.getSource


getPosition : Parser c p Position
getPosition =
    Parser.succeed
        (\offset ( row, col ) ->
            { offset = offset
            , line = row
            , column = col
            }
        )
        |= Parser.getOffset
        |= Parser.getPosition



{- ERROR MESSAGES -}


{-| -}
errorToString : ErrorMessage -> String
errorToString msg =
    formatErrorString msg


formatErrorString error =
    String.toUpper error.title
        ++ "\n\n"
        ++ String.join "" (List.map .text error.message)


type Theme
    = Dark
    | Light


{-| -}
errorToHtml : Theme -> ErrorMessage -> Html.Html msg
errorToHtml theme error =
    formatErrorHtml theme error


formatErrorHtml theme error =
    Html.div []
        (Html.span [ Html.Attributes.style "color" (foregroundClr theme) ]
            [ Html.text
                (String.toUpper error.title
                    ++ "\n\n"
                )
            ]
            :: List.map (renderMessageHtml theme) error.message
        )


redClr theme =
    case theme of
        Dark ->
            "#ef2929"

        Light ->
            "#cc0000"


yellowClr theme =
    case theme of
        Dark ->
            "#edd400"

        Light ->
            "#c4a000"


foregroundClr theme =
    case theme of
        Dark ->
            "#eeeeec"

        Light ->
            "rgba(16,16,16, 0.9)"


renderMessageHtml theme message =
    Html.span
        (List.filterMap identity
            [ if message.bold then
                Just (Html.Attributes.style "font-weight" "bold")

              else
                Nothing
            , if message.underline then
                Just (Html.Attributes.style "text-decoration" "underline")

              else
                Nothing
            , case message.color of
                Nothing ->
                    Just <| Html.Attributes.style "color" (foregroundClr theme)

                Just "red" ->
                    Just <| Html.Attributes.style "color" (redClr theme)

                Just "yellow" ->
                    Just <| Html.Attributes.style "color" (yellowClr theme)

                _ ->
                    Nothing
            ]
        )
        [ Html.text message.text ]


type alias Message =
    { row : Int
    , col : Int
    , problem : ProblemMessage
    }


type alias ErrorMessage =
    { message : List Format.Text
    , region : { start : Position, end : Position }
    , title : String
    }


{-| -}
type ProblemMessage
    = MsgUnknownBlock (List String)
    | MsgUnknownInline (List String)
    | MsgNonMatchingFields
        { expecting : List String
        , found : List String
        }
    | MsgUnexpectedField
        { found : String
        , options : List String
        , recordName : String
        }
    | MsgExpectingIndent Int
    | MsgCantStartTextWithSpace
    | MsgUnclosedStyle (List Style)
    | MsgBadDate String
    | MsgIntOutOfRange
        { found : Int
        , min : Int
        , max : Int
        }
    | MsgFloatOutOfRange
        { found : Float
        , min : Float
        , max : Float
        }


type alias Similarity =
    Int


similarity : String -> String -> Similarity
similarity source target =
    let
        length =
            if String.length source == String.length target then
                1

            else
                0

        first str =
            Maybe.map (String.fromChar << Tuple.first) (String.uncons str)
                |> Maybe.withDefault ""

        last str =
            Maybe.map (String.fromChar << Tuple.first) (String.uncons (String.reverse str))
                |> Maybe.withDefault

        fstChar =
            if first source == first target then
                1

            else
                0

        lastChar =
            if first source == first target then
                1

            else
                0

        addCompared ( x, y ) total =
            if x == y then
                total + 1

            else
                total
    in
    -- List.foldl (+) 0 [ length, firstChar, lastChar ]
    List.foldl addCompared 0 (List.map2 Tuple.pair (String.toList source) (String.toList target))


getRemap context found =
    case found of
        Nothing ->
            case context.context of
                InRemapped remapped ->
                    Just remapped

                _ ->
                    found

        _ ->
            found


getErrorPosition current =
    case List.foldl getRemap Nothing current.contextStack of
        Nothing ->
            ( current.row, current.col )

        Just re ->
            ( re.line, re.column )


mergeErrors : Parser.DeadEnd Context Problem -> List Message -> List Message
mergeErrors current merged =
    let
        ( row, col ) =
            getErrorPosition current
    in
    case merged of
        [] ->
            case current.problem of
                ExpectingBlockName name ->
                    [ { row = row
                      , col = col
                      , problem =
                            MsgUnknownBlock [ name ]
                      }
                    ]

                ExpectingInlineName name ->
                    [ { row = row
                      , col = col
                      , problem =
                            MsgUnknownInline [ name ]
                      }
                    ]

                NonMatchingFields fields ->
                    [ { row = row
                      , col = col
                      , problem =
                            MsgNonMatchingFields fields
                      }
                    ]

                UnexpectedField fields ->
                    [ { row = row
                      , col = col
                      , problem =
                            MsgUnexpectedField fields
                      }
                    ]

                ExpectingIndent indentation ->
                    [ { row = row
                      , col = col
                      , problem =
                            MsgExpectingIndent indentation
                      }
                    ]

                CantStartTextWithSpace ->
                    [ { row = row
                      , col = col
                      , problem =
                            MsgCantStartTextWithSpace
                      }
                    ]

                UnclosedStyles styles ->
                    [ { row = row
                      , col = col
                      , problem =
                            MsgUnclosedStyle styles
                      }
                    ]

                BadDate str ->
                    [ { row = row
                      , col = col
                      , problem =
                            MsgBadDate str
                      }
                    ]

                IntOutOfRange found ->
                    [ { row = row
                      , col = col
                      , problem =
                            MsgIntOutOfRange found
                      }
                    ]

                FloatOutOfRange found ->
                    [ { row = row
                      , col = col
                      , problem =
                            MsgFloatOutOfRange found
                      }
                    ]

                _ ->
                    []

        last :: remaining ->
            if last.col == col && last.row == row then
                case current.problem of
                    ExpectingBlockName blk ->
                        case last.problem of
                            MsgUnknownBlock blocks ->
                                { row = row
                                , col = col
                                , problem =
                                    MsgUnknownBlock (blk :: blocks)
                                }
                                    :: remaining

                            _ ->
                                remaining

                    ExpectingInlineName blk ->
                        case last.problem of
                            MsgUnknownInline blocks ->
                                { row = row
                                , col = col
                                , problem =
                                    MsgUnknownInline (blk :: blocks)
                                }
                                    :: remaining

                            _ ->
                                remaining

                    ExpectingIndent indentation ->
                        [ { row = row
                          , col = col
                          , problem =
                                MsgExpectingIndent indentation
                          }
                        ]

                    _ ->
                        merged

            else
                case current.problem of
                    ExpectingBlockName blk ->
                        { row = row
                        , col = col
                        , problem =
                            MsgUnknownBlock [ blk ]
                        }
                            :: merged

                    _ ->
                        merged


renderErrors : List String -> Message -> ErrorMessage
renderErrors lines current =
    case current.problem of
        MsgUnknownBlock expecting ->
            let
                line =
                    getLine current.row lines

                word =
                    getWord current line
            in
            { title = "UNKNOWN BLOCK"
            , region =
                focusWord current line
            , message =
                [ Format.text "I don't recognize this block name.\n\n"
                , singleLine current.row (line ++ "\n")
                , highlightWord current line
                , Format.text "Do you mean one of these instead?\n\n"
                , expecting
                    |> List.sortBy (\exp -> 0 - similarity word exp)
                    |> List.map (addIndent 4)
                    |> String.join "\n"
                    |> Format.text
                    |> Format.yellow
                ]
            }

        MsgUnknownInline expecting ->
            let
                line =
                    getLine current.row lines
            in
            { title = "UNKNOWN INLINE"
            , region =
                focusWord current line
            , message =
                [ Format.text "I ran into an unexpected inline name.\n\n"
                , singleLine current.row (line ++ "\n")
                , highlightUntil '|' { current | col = current.col + 1 } line
                , Format.text "But I was expecting one of these instead:\n\n"
                , expecting
                    |> List.sortBy (\exp -> 0 - similarity line exp)
                    |> List.map (addIndent 4)
                    |> String.join "\n"
                    |> Format.text
                    |> Format.yellow
                ]
            }

        MsgExpectingIndent indentation ->
            let
                line =
                    getLine current.row lines
            in
            { title = "MISMATCHED INDENTATION"
            , region = focusSpace current line
            , message =
                [ Format.text ("I was expecting " ++ String.fromInt indentation ++ " spaces of indentation.\n\n")
                , singleLine current.row (line ++ "\n")
                , highlightSpace current.col line
                ]
                    ++ hint "All indentation in `elm-markup` is a multiple of 4."
            }

        MsgCantStartTextWithSpace ->
            let
                line =
                    getLine current.row lines
            in
            { title = "TOO MUCH SPACE"
            , region = focusSpace current line
            , message =
                [ Format.text "This line of text starts with extra space.\n\n"
                , singleLine current.row (line ++ "\n")
                , highlightSpace (current.col - 1) line
                , Format.text "Beyond the required indentation, text should start with non-whitespace characters."
                ]
            }

        MsgUnclosedStyle styles ->
            let
                line =
                    getLine current.row lines
            in
            { title = "UNCLOSED STYLE"
            , region = focusSpace current line
            , message =
                [ Format.text (styleNames styles ++ " still open.  Add " ++ String.join " and " (List.map styleChars styles) ++ " to close it.\n\n")
                , singleLine current.row (line ++ "\n")
                , Format.text (String.join "" (List.map styleChars styles) ++ "\n")
                    |> Format.red
                , highlightSpace current.col line
                ]
                    ++ hint "`*` is used for bold and `/` is used for italic."
            }

        MsgUnexpectedField unexpectedField ->
            let
                line =
                    getLine current.row lines

                word =
                    getPrevWord current line
            in
            { title = "UNKNOWN FIELD"
            , region =
                focusPrevWord current line
            , message =
                [ Format.text "I ran into an unexpected field name for a "
                , Format.text unexpectedField.recordName
                    |> Format.yellow
                , Format.text " record\n\n"
                , singleLine current.row (line ++ "\n")
                , highlightPreviousWord current line
                , Format.text "Do you mean one of these instead?\n\n"
                , unexpectedField.options
                    |> List.sortBy (\exp -> 0 - similarity word exp)
                    |> List.map (addIndent 4)
                    |> String.join "\n"
                    |> Format.text
                    |> Format.yellow
                ]
            }

        MsgBadDate found ->
            let
                line =
                    getLine current.row lines
            in
            { title = "BAD DATE"
            , region =
                focusWord current line
            , message =
                [ Format.text "I was trying to parse a date, but this format looks off.\n\n"
                , singleLine current.row (line ++ "\n")
                , highlightWord current line
                , Format.text "Dates should be in ISO 8601 format:\n\n"
                , Format.text (addIndent 4 "YYYY-MM-DDTHH:mm:ss.SSSZ")
                    |> Format.yellow
                ]
            }

        MsgIntOutOfRange found ->
            let
                line =
                    getLine current.row lines
            in
            { title = "INTEGER OUT OF RANGE"
            , region =
                focusWord current line
            , message =
                [ Format.text "I was expecting an "
                , Format.yellow (Format.text "Int")
                , Format.text " between "
                , Format.text (String.fromInt found.min)
                    |> Format.yellow
                , Format.text " and "
                , Format.text (String.fromInt found.max)
                    |> Format.yellow
                , Format.text ", but found:\n\n"
                , singleLine current.row (line ++ "\n")
                , highlightWord current line
                ]
            }

        MsgFloatOutOfRange found ->
            let
                line =
                    getLine current.row lines
            in
            { title = "FLOAT OUT OF RANGE"
            , region =
                focusWord current line
            , message =
                [ Format.text "I was expecting a "
                , Format.yellow (Format.text "Float")
                , Format.text " between "
                , Format.text (String.fromFloat found.min)
                    |> Format.yellow
                , Format.text " and "
                , Format.text (String.fromFloat found.max)
                    |> Format.yellow
                , Format.text ", but found:\n\n"
                , singleLine current.row (line ++ "\n")
                , highlightWord current line
                ]
            }

        MsgNonMatchingFields fields ->
            let
                line =
                    getLine current.row lines

                remaining =
                    List.filter
                        (\f -> not <| List.member f fields.found)
                        fields.expecting
            in
            { title = "MISSING FIELD"
            , region = focusSpace current line
            , message =
                -- TODO: Highlight entire record section
                -- TODO: mention record name
                case remaining of
                    [] ->
                        -- TODO: This should never happen actually.
                        --  Maybe error should be a nonempty list?
                        [ Format.text "It looks like a field is missing." ]

                    [ single ] ->
                        [ Format.text "It looks like a field is missing.\n\n"
                        , Format.text "You need to add the "
                        , Format.yellow (Format.text single)
                        , Format.text " field."
                        ]

                    multiple ->
                        [ Format.text "It looks like a field is missing.\n\n"
                        , Format.text "You still need to add:\n"
                        , remaining
                            |> List.sortBy (\exp -> 0 - similarity line exp)
                            |> List.map (addIndent 4)
                            |> String.join "\n"
                            |> Format.text
                            |> Format.yellow
                        ]
            }


styleChars style =
    case style of
        Bold ->
            "*"

        Italic ->
            "/"

        Strike ->
            "~"


styleNames styles =
    let
        italic =
            List.any ((==) Italic) styles

        isBold =
            List.any ((==) Bold) styles

        strike =
            List.any ((==) Strike) styles
    in
    case ( italic, isBold, strike ) of
        ( False, False, False ) ->
            "Some formatting is"

        ( True, True, False ) ->
            "Italic and bold formatting are"

        ( True, True, True ) ->
            "Italic, strike, and bold formatting are"

        ( True, False, True ) ->
            "Italic and strike formatting are"

        ( False, True, True ) ->
            "Strike, and bold formatting are"

        ( True, False, False ) ->
            "Italic formatting is"

        ( False, True, False ) ->
            "Bold formatting is"

        ( False, False, True ) ->
            "Strike formatting is"


focusWord cursor line =
    let
        highlightLength =
            line
                |> String.dropLeft cursor.col
                |> String.words
                |> List.head
                |> Maybe.map String.length
                |> Maybe.withDefault 1
    in
    { start =
        { column = cursor.col
        , line = cursor.row
        , offset = 0
        }
    , end =
        { column = cursor.col + highlightLength
        , line = cursor.row
        , offset = 0
        }
    }


focusSpace cursor line =
    let
        start =
            String.dropLeft (cursor.col - 1) line

        trimmed =
            String.trimLeft start

        highlightLength =
            String.length start
                - String.length trimmed
                |> max 1
    in
    { start =
        { column = cursor.col
        , line = cursor.row
        , offset = 0
        }
    , end =
        { column = cursor.col + highlightLength
        , line = cursor.row
        , offset = 0
        }
    }


highlightSpace col line =
    let
        start =
            String.dropLeft (col - 1) line

        trimmed =
            String.trimLeft start

        highlightLength =
            String.length start
                - String.length trimmed
                |> max 1
    in
    Format.red <| Format.text (" " ++ String.repeat col " " ++ String.repeat highlightLength "^" ++ "\n")


highlightWord cursor line =
    let
        rowNumLength =
            String.length (String.fromInt cursor.row)

        highlightLength =
            line
                |> String.dropLeft (cursor.col - rowNumLength)
                |> String.words
                |> List.head
                |> Maybe.map String.length
                |> Maybe.withDefault 1
    in
    Format.red <| Format.text (String.repeat (rowNumLength + cursor.col - 1) " " ++ String.repeat highlightLength "^" ++ "\n")


highlightPreviousWord cursor line =
    let
        rowNumLength =
            String.length (String.fromInt cursor.row)

        start =
            String.length line - String.length (String.trimLeft line)

        highlightLength =
            line
                |> String.dropRight (String.length line - (cursor.col - 2))
                |> String.trimLeft
                |> String.length
    in
    Format.red <| Format.text (String.repeat (rowNumLength + start + 1) " " ++ String.repeat highlightLength "^" ++ "\n")


focusPrevWord cursor line =
    let
        start =
            String.length line - String.length (String.trimLeft line)

        highlightLength =
            line
                |> String.dropRight (String.length line - (cursor.col - 2))
                |> String.trimLeft
                |> String.length
    in
    { start =
        { column = start
        , line = cursor.row
        , offset = 0
        }
    , end =
        { column = start + highlightLength
        , line = cursor.row
        , offset = 0
        }
    }


highlightUntil end cursor line =
    let
        rowNumLength =
            String.length (String.fromInt cursor.row)

        highlightLength =
            line
                |> String.dropLeft (cursor.col - rowNumLength)
                |> String.split (String.fromChar end)
                |> List.head
                |> Maybe.map (\str -> String.length str + 1)
                |> Maybe.withDefault 1
    in
    Format.red <| Format.text (String.repeat (rowNumLength + cursor.col - 1) " " ++ String.repeat highlightLength "^" ++ "\n")


newline =
    { text = "\n"
    , color = Nothing
    , underline = False
    , bold = False
    }


addIndent x str =
    String.repeat x " " ++ str


singleLine row line =
    Format.text <|
        String.fromInt row
            ++ (if String.startsWith "|" line then
                    ""

                else
                    "|"
               )
            ++ line


hint str =
    [ Format.text "Hint"
        |> Format.underline
    , Format.text (": " ++ str)
    ]


getLine row lines =
    case List.head (List.drop (row - 1) lines) of
        Nothing ->
            "Empty"

        Just l ->
            l


getWord cursor line =
    let
        rowNumLength =
            String.length (String.fromInt cursor.row)

        highlightLength =
            line
                |> String.dropLeft (cursor.col - rowNumLength)
                |> String.words
                |> List.head
                |> Maybe.map String.length
                |> Maybe.withDefault 1

        end =
            cursor.col + highlightLength
    in
    String.slice (cursor.col - 1) end line


getPrevWord cursor line =
    let
        start =
            String.length line - String.length (String.trimLeft line)

        highlightLength =
            line
                |> String.dropRight (String.length line - (cursor.col - 2))
                |> String.trimLeft
                |> String.length
    in
    String.slice start (start + highlightLength) line
