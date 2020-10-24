module Mark.Internal.Description exposing
    ( render, compile
    , Icon(..)
    , Description(..), TextDescription(..), Text(..), Style(..)
    , Expectation(..), InlineExpectation(..)
    , Parsed(..), descriptionToString, toString, mergeWith
    , create, createInline
    , Styling, emptyStyles
    , inlineExample, blockName, uncertain, humanReadableExpectations
    , Uncertain(..), mapSuccessAndRecovered, renderBlock, getBlockExpectation, getParser, getParserNoBar
    , Block(..), BlockKind(..), Document(..)
    , boldStyle, italicStyle, strikeStyle
    , getId, sizeFromRange
    , Record(..), Range, recordName, ParseContext(..), blockKindToContext, blockKindToSelection, length, match, matchExpected
    , BlockOutcome, InlineSelection(..), New(..), NewInline(..), emptyRange, findMatch, lookup, matchBlock, mergeListWithAttrs, mergeWithAttrs, minusPosition, valid
    )

{-|

@docs render, compile

@docs Icon

@docs Description, TextDescription, Text, Style

@docs Expectation, InlineExpectation

@docs Parsed, descriptionToString, toString, mergeWith

@docs create, createInline

@docs Styling, emptyStyles

@docs inlineExample, blockName, uncertain, humanReadableExpectations

@docs Uncertain, mapSuccessAndRecovered, renderBlock, getBlockExpectation, getParser, getParserNoBar

@docs Block, BlockKind, Document

@docs boldStyle, italicStyle, strikeStyle

@docs resultToDescription, getId, sizeFromRange, foundRange

@docs Record, Range, recordName, ParseContext, blockKindToContext, blockKindToSelection, length, match, matchExpected

-}

import Mark.Internal.Error as Error
import Mark.Internal.Format exposing (text)
import Mark.Internal.Id as Id exposing (..)
import Mark.Internal.Outcome exposing (..)
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


{-| -}
type Document meta data
    = Document
        { expect : Expectation
        , metadata : Parser Error.Context Error.Problem (Result Error.UnexpectedDetails meta)
        , parser : Parser Error.Context Error.Problem Parsed
        , converter : Parsed -> BlockOutcome (List data)
        }


type alias WithAttr data =
    { data : data
    , attrs : List Attribute
    }


{-| -}
type Parsed
    = Parsed
        { errors : List Error.Rendered
        , found : Description
        , expected : Expectation
        , initialSeed : Id.Seed
        , currentSeed : Id.Seed
        , attributes : List Attribute
        }


type alias Attribute =
    { name : String
    , value : String
    , block : Id.Id
    }


{-| -}
type Description
    = DescribeBlock
        { id : Id
        , name : String
        , found : Description
        }
    | Record
        { id : Id
        , name : String
        , found : List ( String, Description )
        }
    | Group
        { id : Id
        , children : List Description
        }
    | StartsWith
        { id : Id
        , first : Description
        , second : Description
        }
      -- This is an item within a tree
    | DescribeItem
        { id : Id
        , icon : Icon
        , content : List Description
        , children : List Description
        }
      -- Primitives
    | DescribeBoolean
        { id : Id
        , found : Bool
        }
    | DescribeInteger
        { id : Id
        , found : Int
        }
    | DescribeFloat
        { id : Id
        , found : ( String, Float )
        }
    | DescribeText
        { id : Id
        , text : List TextDescription
        }
    | DescribeString Id String
    | DescribeNothing Id
    | DescribeUnexpected Id Error.UnexpectedDetails


type InlineSelection
    = EmptyAnnotation
    | SelectText (List Text)
    | SelectString String


{-| -}
type TextDescription
    = Styled Text
    | InlineBlock
        { kind : InlineSelection
        , record : Description
        }


{-| A text fragment with some styling.
-}
type Text
    = Text Styling String


{-| -}
type alias Position =
    { offset : Int
    , line : Int
    , column : Int
    }


{-| -}
type alias Range =
    { start : Position
    , end : Position
    }


{-| -}
type Icon
    = Bullet
    | AutoNumber Int


{-|

    A `Block data` is just a parser that results in `data`.

You'll be building up your `Document` in terms of the `Blocks`.

A block starts with `|` and has a name(already built into the parser)

A value is just a raw parser.

--- Error Propagation

    Failure
        -> The document has had a mismatch
        -> This can only be recovered by `oneOf` or `manyOf`
    Almost
        -> An `Error` has been "raised".  It's propogated until it's resolved by `onError`
        ->

    Success
        -> Well, this is the easy one.

    Ids are only available on the element they apply to.

    Recovery is done at the level above the errored block.

    So, if there is a record with a string that is constrained

    If the constraint fails, the record can say "Here's the reset"

-}
type Block data
    = Block
        { kind : BlockKind
        , converter : Description -> BlockOutcome data
        , expect : Expectation
        , parser : ParseContext -> Id.Seed -> ( Id.Seed, Parser Error.Context Error.Problem Description )
        }


type alias BlockOutcome data =
    Outcome Error.AstError (Uncertain (WithAttr data)) (WithAttr data)


{-| With this type, we're not quite sure if we're going to be able to render or not.

Scenarios:

  - A field value of a record has multiple errors with it.
  - It's caught and the error statue is rendered immediately.
  - The document can be rendered, but we still have multiple errors to keep track of.

-}
type Uncertain data
    = Uncertain ( Error.UnexpectedDetails, List Error.UnexpectedDetails )
    | Recovered ( Error.UnexpectedDetails, List Error.UnexpectedDetails ) data


type ParseContext
    = ParseBlock
    | ParseInline
    | ParseInTree


type BlockKind
    = Value
    | Named String
    | VerbatimNamed String
    | AnnotationNamed String


blockKindToContext : BlockKind -> ParseContext
blockKindToContext kind =
    case kind of
        Value ->
            ParseBlock

        Named name ->
            ParseBlock

        VerbatimNamed name ->
            ParseInline

        AnnotationNamed name ->
            ParseInline


blockKindToSelection : BlockKind -> InlineSelection
blockKindToSelection kind =
    case kind of
        Value ->
            EmptyAnnotation

        Named name ->
            EmptyAnnotation

        VerbatimNamed name ->
            SelectString ""

        AnnotationNamed name ->
            SelectText []


recordName : Description -> Maybe String
recordName desc =
    case desc of
        Record details ->
            Just details.name

        _ ->
            Nothing


{-| -}
length : TextDescription -> Int
length inlineEl =
    case inlineEl of
        Styled txt ->
            textLength txt

        InlineBlock details ->
            case details.kind of
                EmptyAnnotation ->
                    0

                SelectString str ->
                    String.length str

                SelectText txts ->
                    List.sum (List.map textLength txts)


textLength : Text -> Int
textLength (Text _ str) =
    String.length str


type alias Styling =
    { bold : Bool
    , italic : Bool
    , strike : Bool
    }



{- EXPECTATIONS -}


{-| -}
type Style
    = Bold
    | Italic
    | Strike


{-| -}
type Expectation
    = ExpectBlock String Expectation
    | ExpectRecord String (List ( String, Expectation ))
    | ExpectOneOf (List Expectation)
    | ExpectManyOf (List Expectation)
    | ExpectStartsWith Expectation Expectation
    | ExpectBoolean Bool
    | ExpectInteger Int
    | ExpectFloat Float
    | ExpectTextBlock (List InlineExpectation)
    | ExpectString String
    | ExpectTree Expectation
    | ExpectNothing


{-| -}
type InlineExpectation
    = ExpectText Text
    | ExpectInlineBlock
        { name : String
        , kind : InlineSelection
        , fields : List ( String, Expectation )
        }


{-| -}
type New
    = NewBlock String New
    | NewRecord String (List ( String, New ))
    | NewGroup (List New)
    | NewStartsWith New New
    | NewBoolean Bool
    | NewInteger Int
    | NewFloat Float
    | NewTextBlock (List NewInline)
    | NewString String
    | NewItem Icon (List New)


{-| -}
type NewInline
    = NewText Text
    | NewInlineBlock
        { name : String
        , kind : InlineSelection
        , fields : List ( String, New )
        }


{-| -}
uncertain : Error.UnexpectedDetails -> Outcome Error.AstError (Uncertain data) data
uncertain err =
    Almost (Uncertain ( err, [] ))


mapSuccessAndRecovered :
    (success -> otherSuccess)
    -> BlockOutcome success
    -> BlockOutcome otherSuccess
mapSuccessAndRecovered fn outcome =
    case outcome of
        Success s ->
            Success
                { data = fn s.data
                , attrs = s.attrs
                }

        Almost (Uncertain u) ->
            Almost (Uncertain u)

        Almost (Recovered e a) ->
            Almost
                (Recovered e
                    { data = fn a.data
                    , attrs = a.attrs
                    }
                )

        Failure f ->
            Failure f


{-| -}
type Record data
    = ProtoRecord
        { name : String
        , blockKind : BlockKind
        , expectations : List ( String, Expectation )
        , fieldConverter :
            Description
            -> InlineSelection
            -> BlockOutcome (FieldConverter data)
        , fields : List FieldParser
        }


type alias FieldConverter data =
    ( List ( String, Description ), data )


type alias FieldParser =
    ParseContext
    -> Id.Seed
    -> ( Id.Seed, ( String, Parser Error.Context Error.Problem ( String, Description ) ) )


emptyRange =
    { start =
        { offset = 0
        , line = 1
        , column = 1
        }
    , end =
        { offset = 0
        , line = 1
        , column = 1
        }
    }


getId : Description -> Id
getId description =
    case description of
        DescribeBlock details ->
            details.id

        Record details ->
            details.id

        Group details ->
            details.id

        StartsWith details ->
            details.id

        DescribeItem details ->
            details.id

        DescribeBoolean details ->
            details.id

        DescribeInteger details ->
            details.id

        DescribeFloat details ->
            details.id

        DescribeText details ->
            details.id

        DescribeString id _ ->
            id

        DescribeNothing id ->
            id

        DescribeUnexpected id details ->
            id


sizeFromRange range =
    { offset = range.end.offset - range.start.offset
    , line = range.end.line - range.start.line
    }


renderBlock : Block data -> Description -> BlockOutcome data
renderBlock fromBlock description =
    case description of
        DescribeUnexpected id unexpected ->
            uncertain unexpected

        _ ->
            case fromBlock of
                Block { converter } ->
                    converter description


getBlockExpectation fromBlock =
    case fromBlock of
        Block { expect } ->
            expect


blockName : Block data -> Maybe String
blockName (Block details) =
    case details.kind of
        Named name ->
            Just name

        Value ->
            Nothing

        VerbatimNamed name ->
            Just name

        AnnotationNamed name ->
            Just name


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


getParser : ParseContext -> Id.Seed -> Block data -> ( Id.Seed, Parser Error.Context Error.Problem Description )
getParser context seed (Block details) =
    case details.kind of
        Named name ->
            case context of
                ParseInline ->
                    details.parser context seed

                _ ->
                    let
                        ( newSeed, blockParser ) =
                            details.parser context seed
                    in
                    ( newSeed
                    , Parser.succeed identity
                        |. Parser.token (Parser.Token "|>" (Error.ExpectingBlockName name))
                        |. Parser.chompWhile (\c -> c == ' ')
                        |= blockParser
                    )

        Value ->
            details.parser context seed

        VerbatimNamed name ->
            details.parser context seed

        AnnotationNamed name ->
            details.parser context seed


getParserNoBar : ParseContext -> Id.Seed -> Block data -> ( Id.Seed, Parser Error.Context Error.Problem Description )
getParserNoBar context seed (Block details) =
    case details.kind of
        Named name ->
            details.parser context seed

        Value ->
            details.parser context seed

        VerbatimNamed name ->
            details.parser context seed

        AnnotationNamed name ->
            details.parser context seed


emptyStyles =
    { bold = False
    , italic = False
    , strike = False
    }


boldStyle =
    { bold = True
    , italic = False
    , strike = False
    }


italicStyle =
    { bold = False
    , italic = True
    , strike = False
    }


strikeStyle =
    { bold = False
    , italic = False
    , strike = True
    }


inlineExample : InlineSelection -> Block a -> Maybe String
inlineExample selection (Block details) =
    let
        selectionText =
            selectionContentToString selection
    in
    case details.kind of
        VerbatimNamed name ->
            Just
                ("`" ++ selectionText ++ "`" ++ inlineAttributeExample details.expect)

        AnnotationNamed name ->
            Just
                ("[" ++ selectionText ++ "]" ++ inlineAttributeExample details.expect)

        _ ->
            Just selectionText


inlineAttributeExample : Expectation -> String
inlineAttributeExample exp =
    case exp of
        ExpectRecord name attrs ->
            "{"
                ++ name
                ++ inlineFieldExample attrs
                ++ "}"

        _ ->
            ""


inlineFieldExample attrs =
    case attrs of
        [] ->
            ""

        _ ->
            "| "
                ++ (List.map
                        (\( fieldName, valueExpected ) ->
                            fieldName ++ " = " ++ humanReadableExpectations valueExpected
                        )
                        attrs
                        |> String.join ", "
                        |> (\x -> x ++ " ")
                   )


selectionContentToString : InlineSelection -> String
selectionContentToString selection =
    case selection of
        EmptyAnnotation ->
            ""

        SelectText texts ->
            let
                ( newStyles, renderedText ) =
                    textListToString texts emptyStyles
            in
            renderedText

        SelectString str ->
            str


inlineExampleHelper : InlineSelection -> Block a -> String
inlineExampleHelper kind (Block block) =
    let
        containerAsString =
            case block.expect of
                ExpectRecord name [] ->
                    "{" ++ name ++ "}"

                ExpectRecord name fields ->
                    "{"
                        ++ name
                        ++ "| "
                        ++ String.join ", " (List.map renderField fields)
                        ++ " }"

                _ ->
                    ""

        renderField ( name, contentBlock ) =
            name ++ " = " ++ "value"

        selection =
            case kind of
                EmptyAnnotation ->
                    ""

                SelectText txts ->
                    let
                        ( newStyles, renderedText ) =
                            txts
                                |> List.foldl gatherText ( emptyStyles, "" )
                                |> gatherText (Text emptyStyles "")
                    in
                    renderedText

                SelectString str ->
                    str
    in
    case block.kind of
        Named name ->
            containerAsString

        Value ->
            containerAsString

        VerbatimNamed str ->
            "`" ++ selection ++ "`" ++ containerAsString

        AnnotationNamed name ->
            "[" ++ selection ++ "]" ++ containerAsString


findMatch description blcks =
    case blcks of
        [] ->
            Failure Error.NoMatch

        blck :: remain ->
            if matchBlock description blck then
                renderBlock blck description

            else
                findMatch description remain


matchBlock : Description -> Block a -> Bool
matchBlock desc (Block details) =
    match desc details.expect


valid : New -> Expectation -> Bool
valid new exp =
    case exp of
        ExpectOneOf choices ->
            List.any
                (valid new)
                choices

        _ ->
            case new of
                NewBlock name child ->
                    case exp of
                        ExpectBlock expectedName expectedChild ->
                            if expectedName == name then
                                valid child expectedChild

                            else
                                False

                        _ ->
                            False

                NewRecord name fields ->
                    case exp of
                        ExpectRecord expectedName expectedFields ->
                            if name == expectedName then
                                List.all (validFields expectedFields) fields

                            else
                                False

                        _ ->
                            False

                NewGroup children ->
                    case exp of
                        ExpectManyOf choices ->
                            List.all
                                (\child ->
                                    List.any (valid child) choices
                                )
                                children

                        _ ->
                            False

                NewStartsWith one two ->
                    case exp of
                        ExpectStartsWith startExp endExp ->
                            valid one startExp
                                && valid two endExp

                        _ ->
                            False

                NewItem icon children ->
                    List.all
                        (\content ->
                            valid content exp
                        )
                        children

                NewBoolean foundBoolean ->
                    case exp of
                        ExpectBoolean _ ->
                            True

                        _ ->
                            False

                NewInteger _ ->
                    case exp of
                        ExpectInteger _ ->
                            True

                        _ ->
                            False

                NewFloat _ ->
                    case exp of
                        ExpectFloat _ ->
                            True

                        _ ->
                            False

                NewTextBlock inlines ->
                    case exp of
                        ExpectTextBlock expectedInlines ->
                            List.all (validInline expectedInlines) inlines

                        _ ->
                            False

                NewString _ ->
                    case exp of
                        ExpectString _ ->
                            True

                        _ ->
                            False


validInline : List InlineExpectation -> NewInline -> Bool
validInline expectations new =
    case new of
        NewText _ ->
            True

        NewInlineBlock details ->
            List.any
                (\exp ->
                    case exp of
                        ExpectInlineBlock expected ->
                            if
                                (details.name == expected.name)
                                    && validKind expected.kind details.kind
                            then
                                List.all (validFields expected.fields) details.fields

                            else
                                False

                        _ ->
                            False
                )
                expectations


validKind : InlineSelection -> InlineSelection -> Bool
validKind one two =
    case one of
        EmptyAnnotation ->
            case two of
                EmptyAnnotation ->
                    True

                _ ->
                    False

        SelectText _ ->
            case two of
                SelectText _ ->
                    True

                _ ->
                    False

        SelectString _ ->
            case two of
                SelectString _ ->
                    True

                _ ->
                    False


validFields : List ( String, Expectation ) -> ( String, New ) -> Bool
validFields expectedFields ( targetFieldName, new ) =
    let
        innerMatch ( validFieldName, validExpectation ) =
            (validFieldName == targetFieldName)
                && valid new validExpectation
    in
    List.any innerMatch expectedFields


match : Description -> Expectation -> Bool
match description exp =
    case exp of
        ExpectOneOf choices ->
            case description of
                DescribeUnexpected _ details ->
                    -- This is sorta weird but correct
                    -- If we're in a manyOf or oneOf, the error we want to report is this specific one
                    -- not the general "didn't match" error
                    True

                _ ->
                    List.any
                        (match description)
                        choices

        _ ->
            case description of
                DescribeNothing _ ->
                    case exp of
                        ExpectNothing ->
                            True

                        _ ->
                            False

                DescribeUnexpected _ details ->
                    -- Not totally sure if this is right :/
                    True

                DescribeBlock details ->
                    case exp of
                        ExpectBlock expectedName expectedChild ->
                            if expectedName == details.name then
                                match details.found expectedChild

                            else
                                False

                        _ ->
                            False

                Record record ->
                    case exp of
                        ExpectRecord name fields ->
                            if record.name == name then
                                List.all (matchExpectedFields fields) record.found

                            else
                                False

                        _ ->
                            False

                Group many ->
                    case exp of
                        ExpectManyOf choices ->
                            List.all
                                (\child ->
                                    List.any (match child) choices
                                )
                                many.children

                        ExpectTree itemExpectation ->
                            List.all
                                (\child ->
                                    match child itemExpectation
                                )
                                many.children

                        _ ->
                            False

                StartsWith details ->
                    case exp of
                        ExpectStartsWith startExp endExp ->
                            match details.first startExp
                                && match details.second endExp

                        _ ->
                            False

                DescribeItem item ->
                    List.all
                        (\content ->
                            match content exp
                        )
                        item.content
                        && List.all
                            (\content ->
                                match content exp
                            )
                            item.children

                DescribeBoolean foundBoolean ->
                    case exp of
                        ExpectBoolean _ ->
                            True

                        _ ->
                            False

                DescribeInteger _ ->
                    case exp of
                        ExpectInteger _ ->
                            True

                        _ ->
                            False

                DescribeFloat _ ->
                    case exp of
                        ExpectFloat _ ->
                            True

                        _ ->
                            False

                DescribeText _ ->
                    case exp of
                        ExpectTextBlock _ ->
                            True

                        _ ->
                            False

                DescribeString _ _ ->
                    case exp of
                        ExpectString _ ->
                            True

                        _ ->
                            False


matchExpectedFields : List ( String, Expectation ) -> ( String, Description ) -> Bool
matchExpectedFields fields ( targetFieldName, description ) =
    let
        innerMatch ( validFieldName, validExpectation ) =
            (validFieldName == targetFieldName)
                && match description validExpectation
    in
    List.any innerMatch fields


{-| Is the first expectation a subset of the second?
-}
matchExpected : Expectation -> Expectation -> Bool
matchExpected subExp expected =
    case ( subExp, expected ) of
        ( ExpectBlock oneName oneExp, ExpectBlock twoName twoExp ) ->
            oneName == twoName && matchExpected oneExp twoExp

        ( ExpectRecord one oneFields, ExpectRecord two twoFields ) ->
            one == two && List.all (matchFields twoFields) oneFields

        ( ExpectOneOf oneOptions, ExpectOneOf twoOptions ) ->
            List.all (matchExpectedOptions twoOptions) oneOptions

        ( ExpectManyOf oneOptions, ExpectManyOf twoOptions ) ->
            List.all (matchExpectedOptions twoOptions) oneOptions

        ( ExpectStartsWith oneStart oneRemain, ExpectStartsWith twoStart twoRemain ) ->
            matchExpected oneStart twoStart
                && matchExpected oneRemain twoRemain

        ( ExpectBoolean _, ExpectBoolean _ ) ->
            True

        ( ExpectInteger _, ExpectInteger _ ) ->
            True

        ( ExpectFloat _, ExpectFloat _ ) ->
            True

        ( ExpectTextBlock oneInline, ExpectTextBlock twoInline ) ->
            True

        ( ExpectString _, ExpectString _ ) ->
            True

        ( ExpectTree _, ExpectTree _ ) ->
            True

        _ ->
            False


matchExpectedOptions : List Expectation -> Expectation -> Bool
matchExpectedOptions opts target =
    List.any (matchExpected target) opts


matchFields : List ( String, Expectation ) -> ( String, Expectation ) -> Bool
matchFields expected ( targetFieldName, targetFieldExpectation ) =
    let
        innerMatch ( validFieldName, validExpectation ) =
            validFieldName
                == targetFieldName
                && matchExpected validExpectation targetFieldExpectation
    in
    List.any innerMatch expected


boolToString : Bool -> String
boolToString b =
    if b then
        "True"

    else
        "False"


moveColumn : Int -> Position -> Position
moveColumn num pos =
    { offset = pos.offset + num
    , column = pos.column + num
    , line = pos.line
    }


moveNewline : Position -> Position
moveNewline pos =
    { offset = pos.offset + 1
    , column = 1
    , line = pos.line + 1
    }


moveNewlines : Int -> Position -> Position
moveNewlines i pos =
    { offset = pos.offset + i
    , column = 1
    , line = pos.line + i
    }


minusPosition end start =
    { offset = end.offset - start.offset
    , line = end.line - start.line
    , column = end.column - start.column
    }



{- DESCRIPTION -> STRING -}


{-| -}
toString : Parsed -> String
toString (Parsed parsed) =
    writeDescription
        parsed.found
        { indent = 0
        , position = { line = 1, column = 1, offset = 0 }
        , printed = ""
        }
        |> .printed


descriptionToString : Description -> String
descriptionToString desc =
    writeDescription
        desc
        { indent = 0
        , position = { line = 1, column = 1, offset = 0 }
        , printed = ""
        }
        |> .printed


type alias PrintCursor =
    { indent : Int
    , position : Position
    , printed : String
    }


write : String -> PrintCursor -> PrintCursor
write str cursor =
    if str == "" then
        cursor

    else
        { cursor
            | printed = cursor.printed ++ str
            , position =
                (\pos ->
                    { pos
                        | offset = pos.offset + String.length str
                        , column = pos.column + String.length str
                    }
                )
                    cursor.position
        }


writeNewline : PrintCursor -> PrintCursor
writeNewline cursor =
    { cursor
        | printed = cursor.printed ++ "\n"
        , position =
            (\pos ->
                { pos
                    | offset = pos.offset + 1
                    , column = 1
                    , line = pos.line + 1
                }
            )
                cursor.position
    }


writeNewlines : Int -> PrintCursor -> PrintCursor
writeNewlines n cursor =
    { cursor
        | printed = cursor.printed ++ String.repeat n "\n"
        , position =
            (\pos ->
                { pos
                    | offset = pos.offset + n
                    , column = 1
                    , line = pos.line + n
                }
            )
                cursor.position
    }


{-| Add spaces and newlines in order to make up the discrepancy between cursor and target
-}
advanceTo : Range -> PrintCursor -> PrintCursor
advanceTo target cursor =
    let
        lineDiff =
            abs (target.start.line - cursor.position.line)
    in
    if target.start == cursor.position then
        cursor

    else if lineDiff == 0 then
        write (String.repeat (target.start.column - cursor.position.column) " ") cursor

    else
        cursor
            |> writeNewlines lineDiff
            |> write (String.repeat (target.start.column - 1) " ")


writeLine line cursor =
    cursor
        |> write line
        |> writeNewline


indent : PrintCursor -> PrintCursor
indent cursor =
    { cursor | indent = cursor.indent + 1 }


dedent : PrintCursor -> PrintCursor
dedent cursor =
    { cursor | indent = max 0 cursor.indent - 1 }


writeIndent : PrintCursor -> PrintCursor
writeIndent cursor =
    cursor
        |> write (String.repeat (cursor.indent * 4) " ")


writeIcon icon cursor =
    case icon of
        Bullet ->
            cursor
                |> write "--"

        AutoNumber i ->
            cursor
                |> write (String.fromInt i ++ ".")


writeGroup group cursor =
    case group of
        [] ->
            cursor

        top :: [] ->
            cursor
                |> writeIndent
                |> writeDescription top

        top :: remain ->
            let
                compact =
                    case top of
                        DescribeItem _ ->
                            True

                        _ ->
                            False

                writtenDescription =
                    writeDescription top
                        { indent = cursor.indent
                        , position = cursor.position
                        , printed = ""
                        }

                lines =
                    writtenDescription.printed
                        |> String.lines
                        |> List.length

                newCursor =
                    cursor
                        |> writeIndent
                        |> write writtenDescription.printed
                        |> write
                            (if lines > 1 then
                                "\n\n"

                             else if compact then
                                "\n"

                             else
                                "\n\n"
                            )
            in
            writeGroup remain newCursor


{-| -}
writeDescription : Description -> PrintCursor -> PrintCursor
writeDescription description cursor =
    case description of
        DescribeNothing _ ->
            cursor

        DescribeUnexpected _ details ->
            -- TODO: What do we expect here?
            cursor

        DescribeBlock details ->
            cursor
                |> write ("|> " ++ details.name ++ "\n")
                |> indent
                |> writeIndent
                |> writeDescription details.found
                |> dedent

        Record details ->
            cursor
                |> write ("|> " ++ details.name ++ "\n")
                |> indent
                |> (\c ->
                        List.foldr writeField
                            c
                            details.found
                   )
                |> dedent

        Group many ->
            writeGroup many.children cursor

        StartsWith details ->
            cursor
                |> writeDescription details.first
                |> write "\n"
                |> writeDescription details.second

        DescribeBoolean details ->
            writeWith boolToString details.found cursor

        DescribeInteger details ->
            writeWith String.fromInt details.found cursor

        DescribeFloat details ->
            writeWith Tuple.first details.found cursor

        DescribeText txt ->
            cursor
                |> (\c ->
                        txt.text
                            |> List.foldl
                                writeTextDescription
                                { cursor = c
                                , styles = emptyStyles
                                }
                            |> writeTextDescription
                                (Styled (Text emptyStyles ""))
                            |> .cursor
                   )

        DescribeString id str ->
            let
                indented =
                    String.lines str
                        |> List.indexedMap
                            (\i s ->
                                if s == "" || i == 0 then
                                    s

                                else
                                    String.repeat (cursor.indent * 4) " " ++ s
                            )

                numLines =
                    List.length indented
            in
            cursor
                |> (\curs ->
                        List.foldl
                            (\line ( i, advancedCurs ) ->
                                if i == numLines then
                                    ( i + 1, write line advancedCurs )

                                else
                                    ( i + 1, writeLine line advancedCurs )
                            )
                            ( 1, curs )
                            indented
                   )
                |> Tuple.second

        DescribeItem item ->
            cursor
                |> writeIcon item.icon
                |> write " "
                |> indent
                |> (\curs ->
                        case item.content of
                            [] ->
                                curs

                            first :: [] ->
                                curs
                                    |> writeDescription first

                            first :: remain ->
                                curs
                                    |> writeDescription first
                                    |> write "\n\n"
                                    |> writeGroup remain
                                    |> write "\n"
                   )
                |> (\curs ->
                        case item.children of
                            [] ->
                                curs

                            _ ->
                                curs
                                    |> write "\n"
                                    |> writeGroup item.children
                   )
                |> dedent


textDescriptionToString existingStyles txt =
    case txt of
        Styled t ->
            textToString existingStyles t

        InlineBlock details ->
            let
                renderField ( name, desc ) =
                    name
                        ++ " = "
                        ++ descriptionToString desc

                inlineRecord =
                    case details.record of
                        Record recordDetails ->
                            case recordDetails.found of
                                [] ->
                                    "{"
                                        ++ recordDetails.name
                                        ++ "}"

                                fields ->
                                    "{"
                                        ++ recordDetails.name
                                        ++ "|"
                                        ++ String.join ", " (List.map renderField fields)
                                        ++ "}"

                        _ ->
                            ""
            in
            case details.kind of
                EmptyAnnotation ->
                    Tuple.pair existingStyles <|
                        inlineRecord

                SelectText txts ->
                    let
                        ( newStyles, renderedText ) =
                            textListToString txts existingStyles
                    in
                    ( newStyles
                    , "[" ++ renderedText ++ "]" ++ inlineRecord
                    )

                SelectString str ->
                    Tuple.pair existingStyles
                        ("`" ++ str ++ "`" ++ inlineRecord)


textListToString txts existingStyles =
    txts
        |> List.foldl gatherText ( existingStyles, "" )
        |> gatherText (Text emptyStyles "")


writeTextDescription desc cursorAndStyles =
    let
        ( newStyles, newStr ) =
            textDescriptionToString cursorAndStyles.styles desc
    in
    { cursor =
        if cursorAndStyles.cursor.indent > 0 then
            write
                (String.replace "\n"
                    ("\n" ++ String.repeat (4 * cursorAndStyles.cursor.indent) " ")
                    newStr
                )
                cursorAndStyles.cursor

        else
            write newStr cursorAndStyles.cursor
    , styles = newStyles
    }


textToString : Styling -> Text -> ( Styling, String )
textToString existingStyles (Text styles txt) =
    ( styles, startingCharacters existingStyles styles ++ txt )


gatherText : Text -> ( Styling, String ) -> ( Styling, String )
gatherText (Text styles txt) ( existingStyles, existingStr ) =
    ( styles, existingStr ++ startingCharacters existingStyles styles ++ txt )


{-| Here we can standardize the order of control characters.
-}
startingCharacters one two =
    let
        boldClosing =
            if one.bold && not two.bold then
                "*"

            else
                ""

        boldOpening =
            if not one.bold && two.bold then
                "*"

            else
                ""

        italicClosing =
            if one.italic && not two.italic then
                "/"

            else
                ""

        italicOpening =
            if not one.italic && two.italic then
                "/"

            else
                ""

        strikeClosing =
            if one.strike && not two.strike then
                "~"

            else
                ""

        strikeOpening =
            if not one.strike && two.strike then
                "~"

            else
                ""
    in
    boldClosing
        ++ italicClosing
        ++ strikeClosing
        ++ strikeOpening
        ++ italicOpening
        ++ boldOpening


writeWith toStr a cursor =
    write (toStr a) cursor


writeField : ( String, Description ) -> PrintCursor -> PrintCursor
writeField ( name, description ) cursor =
    let
        writtenDescription =
            { indent = cursor.indent
            , position = cursor.position
            , printed = ""
            }
                |> indent
                |> writeDescription description

        lines =
            List.length (String.lines writtenDescription.printed)
    in
    cursor
        |> writeIndent
        |> write
            (name ++ " =")
        |> indent
        |> indentIfMultiline (lines > 1)
        |> write writtenDescription.printed
        |> (\c ->
                if String.endsWith "\n" c.printed then
                    c

                else
                    c |> write "\n"
           )
        |> dedent


indentIfMultiline isMultiline cursor =
    if isMultiline then
        cursor
            |> write "\n"
            |> writeIndent

    else
        cursor
            |> write " "



{- CREATION -}


createInline :
    List NewInline
    -> List TextDescription
createInline current =
    List.foldl newInlineToText
        { text = []
        , styling = emptyStyles
        }
        current
        |> .text
        |> List.reverse


numberStyleChanges one two =
    let
        boldNum =
            if one.bold /= two.bold then
                1

            else
                0

        italicNum =
            if one.italic /= two.italic then
                1

            else
                0

        strikeNum =
            if one.strike /= two.strike then
                1

            else
                0
    in
    boldNum + italicNum + strikeNum


moveText (Text styling str) existingStyling cursor =
    let
        numberLines =
            List.length (String.lines str) - 1
    in
    cursor
        |> moveColumn
            ((String.length str
                + numberStyleChanges styling existingStyling
             )
                - numberLines
            )
        |> moveNewlines numberLines


newInlineToText new cursor =
    case new of
        NewText ((Text newStyling str) as txt) ->
            { text =
                Styled txt :: cursor.text
            , styling = newStyling
            }

        NewInlineBlock details ->
            { styling = cursor.styling
            , text =
                InlineBlock
                    { kind = details.kind
                    , record =
                        DescribeNothing (Tuple.first (Id.step (Id.initialSeed "none")))

                    -- TODO: MAKE CONVERSION TO DESCRIPTION!
                    -- List.map expectationToAttr attrs
                    }
                    :: cursor.text
            }


{-| This function does no validation, it only creates a new Description.

    `

-}
create :
    Id.Seed
    -> New
    ->
        { desc : Description
        , seed : Id.Seed
        }
create seed new =
    case new of
        NewBlock name child ->
            let
                ( blockId, newSeed ) =
                    Id.step seed

                created =
                    create newSeed child
            in
            { desc =
                DescribeBlock
                    { name = name
                    , id = blockId
                    , found =
                        created.desc
                    }
            , seed = newSeed
            }

        NewRecord name fields ->
            let
                ( blockId, newSeed ) =
                    Id.step seed

                created =
                    List.foldl
                        createField
                        { fields = []
                        , seed = newSeed
                        }
                        fields
            in
            { desc =
                Record
                    { name = name
                    , id = blockId
                    , found =
                        created.fields
                    }
            , seed = created.seed
            }

        NewItem icon children ->
            let
                ( parentId, startSeed ) =
                    Id.step seed

                childSeed =
                    Id.indent startSeed

                ( finalSeed, childrenDescriptions ) =
                    List.foldl
                        (\item ( currentSeed, result ) ->
                            let
                                created =
                                    create currentSeed item
                            in
                            ( created.seed
                            , created.desc
                                :: result
                            )
                        )
                        ( childSeed, [] )
                        children
            in
            { desc =
                DescribeItem
                    { id = parentId
                    , icon = icon
                    , content = []
                    , children =
                        childrenDescriptions
                    }
            , seed = finalSeed
            }

        NewGroup items ->
            let
                ( parentId, startSeed ) =
                    Id.step seed

                childSeed =
                    Id.indent startSeed

                ( finalSeed, children ) =
                    List.foldl
                        (\item ( currentSeed, result ) ->
                            let
                                created =
                                    create currentSeed item
                            in
                            ( created.seed
                            , created.desc
                                :: result
                            )
                        )
                        ( childSeed, [] )
                        items
            in
            { seed = finalSeed
            , desc =
                Group
                    { id = parentId
                    , children = List.reverse children
                    }
            }

        NewStartsWith one two ->
            let
                ( parentId, newSeed ) =
                    Id.step seed

                first =
                    create newSeed one

                second =
                    create first.seed two
            in
            { desc =
                StartsWith
                    { id = parentId
                    , first = first.desc
                    , second = second.desc
                    }
            , seed = second.seed
            }

        -- Primitives
        NewBoolean b ->
            let
                ( newId, newSeed ) =
                    Id.step seed
            in
            { desc =
                DescribeBoolean
                    { id = newId
                    , found =
                        b
                    }
            , seed = newSeed
            }

        NewInteger i ->
            let
                ( newId, newSeed ) =
                    Id.step seed
            in
            { desc =
                DescribeInteger
                    { id = newId
                    , found = i
                    }
            , seed = newSeed
            }

        NewFloat f ->
            let
                ( newId, newSeed ) =
                    Id.step seed
            in
            { desc =
                DescribeFloat
                    { id = newId
                    , found = ( String.fromFloat f, f )
                    }
            , seed = newSeed
            }

        NewString str ->
            let
                ( newId, newSeed ) =
                    Id.step seed
            in
            { desc = DescribeString newId str
            , seed = newSeed
            }

        NewTextBlock nodes ->
            let
                newText =
                    createInline nodes

                ( newId, newSeed ) =
                    Id.step seed
            in
            { desc =
                DescribeText
                    { id = newId
                    , text = newText
                    }
            , seed = newSeed
            }


type alias CreateFieldCursor =
    { fields : List ( String, Description )
    , seed : Id.Seed
    }


{-| -}
createField :
    ( String, New )
    -> CreateFieldCursor
    -> CreateFieldCursor
createField ( name, new ) current =
    let
        created =
            create current.seed new
    in
    { fields =
        ( name
        , created.desc
        )
            :: current.fields
    , seed = created.seed
    }


{-| -}
type alias Partial data =
    { errors : List Error.Rendered
    , result : data
    }


errorsToList ( fst, remain ) =
    fst :: remain


{-| -}
compile :
    Document metadata data
    -> String
    ->
        Result
            (Outcome (List Error.Rendered)
                { errors : List Error.Rendered
                , result : List data
                }
                (List data)
            )
            ( Parsed
            , Outcome (List Error.Rendered)
                { errors : List Error.Rendered
                , result : List data
                }
                (List data)
            )
compile (Document blocks) source =
    case Parser.run blocks.parser source of
        Ok ((Parsed parsedDetails) as parsed) ->
            case parsedDetails.errors of
                [] ->
                    case blocks.converter parsed of
                        Success rendered ->
                            Ok
                                ( Parsed { parsedDetails | attributes = rendered.attrs }
                                , Success rendered.data
                                )

                        Almost (Recovered errors rendered) ->
                            Ok
                                ( Parsed { parsedDetails | attributes = rendered.attrs }
                                , Almost
                                    { errors = List.map (Error.render source) (errorsToList errors)
                                    , result = rendered.data
                                    }
                                )

                        Almost (Uncertain errors) ->
                            -- now we're certain :/
                            Ok ( parsed, Failure (List.map (Error.render source) (errorsToList errors)) )

                        Failure Error.NoMatch ->
                            -- Invalid Ast.
                            -- This should never happen because
                            -- we definitely have the same document in both parsing and converting.
                            Ok
                                ( parsed
                                , Failure
                                    [ Error.documentMismatch ]
                                )

                _ ->
                    case blocks.converter parsed of
                        Success rendered ->
                            Ok
                                ( Parsed { parsedDetails | attributes = rendered.attrs }
                                , Almost
                                    { errors = parsedDetails.errors
                                    , result = rendered.data
                                    }
                                )

                        Almost (Uncertain ( err, remainError )) ->
                            Ok
                                ( parsed
                                , Failure (List.map (Error.render source) (err :: remainError))
                                )

                        Almost (Recovered ( err, remainError ) rendered) ->
                            Ok
                                ( Parsed { parsedDetails | attributes = rendered.attrs }
                                , Almost
                                    { errors =
                                        List.map (Error.render source) (err :: remainError)
                                    , result = rendered.data
                                    }
                                )

                        Failure noMatch ->
                            Ok
                                ( parsed
                                , Failure
                                    (Error.documentMismatch
                                        :: parsedDetails.errors
                                    )
                                )

        Err deadEnds ->
            Err <|
                Failure
                    [ Error.renderParsingErrors source deadEnds
                    ]


{-| In order for block lookups to work and for the block to be renderable, we need to retrieve a block, but keep the general structure.

So, if we have `ManyOf [ One,Two ]`, and our ID matches `Two`, we need to return `ManyOf [Two]`, not just `Two`.

Then our rendering function will still work.

-}
lookup : Id.Id -> Document meta block -> Parsed -> Outcome (List Error.Rendered) (Partial block) block
lookup id (Document doc) (Parsed parsed) =
    case find id parsed.found of
        Nothing ->
            Failure [ Error.compilerError ]

        Just found ->
            case doc.converter (Parsed { parsed | found = found }) of
                Success rendered ->
                    case rendered.data of
                        top :: _ ->
                            Success top

                        _ ->
                            Failure [ Error.compilerError ]

                Almost (Uncertain ( err, remainError )) ->
                    -- Failure (List.map (Error.render source) (err :: remainError))
                    Failure [ Error.compilerError ]

                Almost (Recovered ( err, remainError ) result) ->
                    -- Almost
                    --     { errors = List.map (Error.render source) (err :: remainError)
                    --     , result = result
                    --     }
                    Failure [ Error.compilerError ]

                Failure noMatch ->
                    Failure [ Error.compilerError ]


firstFound : Id.Id -> List Description -> Maybe Description
firstFound id options =
    case options of
        [] ->
            Nothing

        top :: remain ->
            case find id top of
                Nothing ->
                    firstFound id remain

                found ->
                    found


find : Id.Id -> Description -> Maybe Description
find targetId desc =
    case desc of
        DescribeUnexpected id details ->
            if id == targetId then
                Just desc

            else
                Nothing

        DescribeBlock details ->
            if details.id == targetId then
                Just desc

            else
                Nothing

        Record details ->
            if details.id == targetId then
                Just desc

            else
                Nothing

        Group details ->
            if details.id == targetId then
                Just desc

            else
                case firstFound targetId details.children of
                    Nothing ->
                        Nothing

                    Just block ->
                        Just
                            (Group
                                { details
                                    | children = [ block ]
                                }
                            )

        StartsWith details ->
            if details.id == targetId then
                Just desc

            else
                case find targetId details.first of
                    Nothing ->
                        find targetId details.second

                    found ->
                        found

        DescribeItem details ->
            if details.id == targetId then
                Just desc

            else
                case firstFound targetId details.content of
                    Nothing ->
                        case firstFound targetId details.children of
                            Nothing ->
                                Nothing

                            Just block ->
                                Just
                                    (DescribeItem
                                        { details
                                            | children = [ block ]
                                        }
                                    )

                    Just block ->
                        Just
                            (DescribeItem
                                { details
                                    | children = [ block ]
                                }
                            )

        -- Primitives
        DescribeBoolean details ->
            if details.id == targetId then
                Just desc

            else
                Nothing

        DescribeInteger details ->
            if details.id == targetId then
                Just desc

            else
                Nothing

        DescribeFloat details ->
            if details.id == targetId then
                Just desc

            else
                Nothing

        DescribeText details ->
            if details.id == targetId then
                Just desc

            else
                Nothing

        DescribeString id string ->
            if id == targetId then
                Just desc

            else
                Nothing

        DescribeNothing id ->
            if id == targetId then
                Just desc

            else
                Nothing


{-| Render is a little odd.

We should always expect `Parsed` to either already have our error messages, or to succeed.

Render can't add additional errors.

-}
render :
    Document meta data
    -> Parsed
    -> Outcome (List Error.Rendered) { errors : List Error.Rendered, result : List data } (List data)
render (Document blocks) ((Parsed parsedDetails) as parsed) =
    case parsedDetails.errors of
        [] ->
            case blocks.converter parsed of
                Success rendered ->
                    Success rendered.data

                Almost (Uncertain ( err, remainError )) ->
                    -- Failure (List.map (Error.render source) (err :: remainError))
                    Failure [ Error.compilerError ]

                Almost (Recovered ( err, remainError ) result) ->
                    -- Almost
                    --     { errors = List.map (Error.render source) (err :: remainError)
                    --     , result = result
                    --     }
                    Failure [ Error.compilerError ]

                Failure noMatch ->
                    Failure [ Error.compilerError ]

        _ ->
            case blocks.converter parsed of
                Success rendered ->
                    Almost
                        { errors = parsedDetails.errors
                        , result = rendered.data
                        }

                Almost (Uncertain _) ->
                    Failure parsedDetails.errors

                Almost (Recovered _ result) ->
                    Almost
                        { errors = parsedDetails.errors
                        , result = result.data
                        }

                Failure noMatch ->
                    Failure
                        (Error.documentMismatch
                            :: parsedDetails.errors
                        )


humanReadableExpectations expect =
    case expect of
        ExpectNothing ->
            ""

        ExpectBlock name exp ->
            "|> " ++ name

        ExpectRecord name fields ->
            "|> " ++ name

        ExpectOneOf expectations ->
            "One of: " ++ String.join ", " (List.map humanReadableExpectations expectations)

        ExpectManyOf expectations ->
            "Many of: " ++ String.join ", " (List.map humanReadableExpectations expectations)

        ExpectStartsWith start remain ->
            humanReadableExpectations start ++ " and then " ++ humanReadableExpectations remain

        ExpectBoolean _ ->
            "A Boolean"

        ExpectInteger _ ->
            "An Int"

        ExpectFloat _ ->
            "A Float"

        ExpectTextBlock inlines ->
            "Styled Text"

        ExpectString _ ->
            "A String"

        ExpectTree _ ->
            "A tree"


mergeWith fn one two =
    case one of
        Success renderedOne ->
            case two of
                Success renderedTwo ->
                    Success (fn renderedOne renderedTwo)

                Almost (Recovered errors second) ->
                    Almost
                        (Recovered
                            errors
                            (fn renderedOne second)
                        )

                _ ->
                    Failure Error.NoMatch

        Almost (Recovered firstErrs fst) ->
            case two of
                Almost (Recovered secondErrs snd) ->
                    Almost
                        (Recovered
                            (mergeErrors firstErrs secondErrs)
                            (fn fst snd)
                        )

                _ ->
                    Failure Error.NoMatch

        Almost (Uncertain unexpected) ->
            Almost (Uncertain unexpected)

        _ ->
            case two of
                Almost (Uncertain unexpected) ->
                    Almost (Uncertain unexpected)

                _ ->
                    Failure Error.NoMatch


mergeWithAttrs : (one -> two -> final) -> BlockOutcome one -> BlockOutcome two -> BlockOutcome final
mergeWithAttrs fn one two =
    case one of
        Success first ->
            case two of
                Success second ->
                    Success
                        { data = fn first.data second.data
                        , attrs = first.attrs ++ second.attrs
                        }

                Almost (Recovered errors second) ->
                    Almost
                        (Recovered
                            errors
                            { data = fn first.data second.data
                            , attrs = first.attrs ++ second.attrs
                            }
                        )

                Almost (Uncertain unexpected) ->
                    Almost (Uncertain unexpected)

                Failure err ->
                    Failure err

        Almost (Recovered firstErrs first) ->
            case two of
                Success second ->
                    Almost
                        (Recovered firstErrs
                            { data = fn first.data second.data
                            , attrs = first.attrs ++ second.attrs
                            }
                        )

                Almost (Recovered secondErrs second) ->
                    Almost
                        (Recovered
                            (mergeErrors firstErrs secondErrs)
                            { data = fn first.data second.data
                            , attrs = first.attrs ++ second.attrs
                            }
                        )

                Almost (Uncertain unexpected) ->
                    Almost (Uncertain unexpected)

                Failure twoErr ->
                    Failure twoErr

        Almost (Uncertain unexpected) ->
            Almost (Uncertain unexpected)

        Failure err ->
            Failure err


type alias ListOutcome data =
    Outcome Error.AstError (Uncertain (List (WithAttr data))) (List (WithAttr data))


mergeListWithAttrs : (List one -> List two -> final) -> ListOutcome one -> ListOutcome two -> BlockOutcome final
mergeListWithAttrs fn one two =
    case one of
        Success first ->
            case two of
                Success second ->
                    Success
                        { data = fn (List.map .data first) (List.map .data second)
                        , attrs = List.concatMap .attrs first ++ List.concatMap .attrs second
                        }

                Almost (Recovered errors second) ->
                    Almost
                        (Recovered
                            errors
                            { data = fn (List.map .data first) (List.map .data second)
                            , attrs = List.concatMap .attrs first ++ List.concatMap .attrs second
                            }
                        )

                _ ->
                    Failure Error.NoMatch

        Almost (Recovered firstErrs first) ->
            case two of
                Success second ->
                    Almost
                        (Recovered firstErrs
                            { data = fn (List.map .data first) (List.map .data second)
                            , attrs = List.concatMap .attrs first ++ List.concatMap .attrs second
                            }
                        )

                Almost (Recovered secondErrs second) ->
                    Almost
                        (Recovered
                            (mergeErrors firstErrs secondErrs)
                            { data = fn (List.map .data first) (List.map .data second)
                            , attrs = List.concatMap .attrs first ++ List.concatMap .attrs second
                            }
                        )

                _ ->
                    Failure Error.NoMatch

        Almost (Uncertain unexpected) ->
            Almost (Uncertain unexpected)

        _ ->
            case two of
                Almost (Uncertain unexpected) ->
                    Almost (Uncertain unexpected)

                _ ->
                    Failure Error.NoMatch


mergeErrors ( h1, r1 ) ( h2, r2 ) =
    ( h1, r1 ++ h2 :: r2 )
