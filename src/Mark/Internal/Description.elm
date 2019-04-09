module Mark.Internal.Description exposing
    ( render, compile
    , Found(..), Nested(..), Icon(..)
    , Description(..), TextDescription(..), InlineAttribute(..), Text(..), Style(..)
    , Expectation(..), InlineExpectation(..), AttrExpectation(..)
    , Parsed(..), startingPoint, descriptionToString, toString
    , create
    , Styling, emptyStyles
    , inlineExample, blockName, uncertain, humanReadableExpectations
    , Uncertain(..), mapSuccessAndRecovered, renderBlock, getBlockExpectation, getParser, getParserNoBar, noInlineAttributes
    , Block(..), Document(..)
    )

{-|

@docs render, compile

@docs Found, Nested, Icon

@docs Description, TextDescription, InlineAttribute, Text, Style

@docs Expectation, InlineExpectation, AttrExpectation

@docs Parsed, startingPoint, descriptionToString, toString

@docs create

@docs Styling, emptyStyles

@docs inlineExample, blockName, uncertain, humanReadableExpectations

@docs Uncertain, mapSuccessAndRecovered, renderBlock, getBlockExpectation, getParser, getParserNoBar, noInlineAttributes

@docs Block, Document

-}

import Mark.Format as Format
import Mark.Internal.Error as Error
import Mark.Internal.Id as Id exposing (..)
import Mark.Internal.Outcome exposing (..)
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


{-| -}
type Document data
    = Document
        { converter : Parsed -> Outcome Error.AstError (Uncertain data) data
        , initialSeed : Id.Seed
        , currentSeed : Id.Seed
        , expect : Expectation
        , parser : Parser Error.Context Error.Problem Parsed
        }


{-| -}
type Parsed
    = Parsed
        { errors : List Error.Rendered
        , found : Found Description
        , expected : Expectation
        , initialSeed : Id.Seed
        , currentSeed : Id.Seed
        }


{-| -}
type Found item
    = Found Range item
    | Unexpected Error.UnexpectedDetails


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
    | AutoNumber


{-| -}
type Nested item
    = Nested
        { index : List Int
        , icon : Icon
        , content : List item
        , children :
            List (Nested item)
        }


{-| With this type, we're not quite sure if we're going to be able to render or not.

Scenarios:

  - A field value of a record has multiple errors with it.
  - It's caught and the error statue is rendered immediately.
  - The document can be rendered, but we still have multiple errors to keep track of.

-}
type Uncertain data
    = Uncertain ( Error.UnexpectedDetails, List Error.UnexpectedDetails )
    | Recovered ( Error.UnexpectedDetails, List Error.UnexpectedDetails ) data


{-| -}
type
    Block data
    {-
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
    = Block
        String
        { converter : Description -> Outcome Error.AstError (Uncertain data) data
        , expect : Expectation
        , parser : Id.Seed -> ( Id.Seed, Parser Error.Context Error.Problem Description )
        }
    | Value
        { converter : Description -> Outcome Error.AstError (Uncertain data) data
        , expect : Expectation
        , parser : Id.Seed -> ( Id.Seed, Parser Error.Context Error.Problem Description )
        }


{-| -}
type Description
    = DescribeBlock
        { name : String
        , found : Found Description
        , expected : Expectation
        }
    | Record
        { name : String
        , found : Found (List ( String, Found Description ))
        , expected : Expectation
        }
    | OneOf
        { id : Id Options
        , choices : List (Choice (Id Options) Expectation)
        , child : Found Description
        }
    | ManyOf
        { id : Id ManyOptions
        , range : Range
        , choices : List (Choice (Id ManyOptions) Expectation)
        , children : List (Found Description)
        }
    | StartsWith
        Range
        { found : Description
        , expected : Expectation
        }
        { found : Description
        , expected : Expectation
        }
    | DescribeTree
        { found : ( Range, List (Nested Description) )
        , expected : Expectation
        }
      -- Primitives
    | DescribeStub String (Found String)
    | DescribeBoolean
        { id : Id Bool
        , found : Found Bool
        }
    | DescribeInteger
        { id : Id Int
        , found : Found Int
        }
    | DescribeIntBetween
        { max : Int
        , min : Int
        , found : Found Int
        , id : Id Int
        }
    | DescribeFloat
        { id : Id Float
        , found : Found ( String, Float )
        }
    | DescribeFloatBetween
        { max : Float
        , min : Float
        , found : Found ( String, Float )
        , id : Id Float
        }
    | DescribeText
        { id : Id Text
        , range : Range
        , text : List TextDescription
        }
    | DescribeString (Id String) Range String
    | DescribeMultiline (Id String) Range String
    | DescribeStringExactly Range String
    | DescribeNothing


{-| -}
type Proved
    = Proved (Id ManyOptions) (List (Found Description))


{-| -}
type TextDescription
    = Styled Range Text
    | InlineAnnotation
        { name : String
        , range : Range
        , text : List Text
        , attributes : List InlineAttribute
        }
    | InlineToken
        { name : String
        , range : Range
        , attributes : List InlineAttribute
        }
    | InlineVerbatim
        { name : Maybe String
        , range : Range
        , text : Text
        , attributes : List InlineAttribute
        }
    | UnexpectedInline Error.UnexpectedDetails


{-| -}
type InlineAttribute
    = AttrString
        { name : String
        , range : Range
        , value : String
        }



-- | DescribeInlineText Range (List Text)


{-| -}
uncertain : Error.UnexpectedDetails -> Outcome Error.AstError (Uncertain data) data
uncertain err =
    Almost (Uncertain ( err, [] ))


mapSuccessAndRecovered :
    (success -> otherSuccess)
    -> Outcome f (Uncertain success) success
    -> Outcome f (Uncertain otherSuccess) otherSuccess
mapSuccessAndRecovered fn outcome =
    case outcome of
        Success s ->
            Success (fn s)

        Almost (Uncertain u) ->
            Almost (Uncertain u)

        Almost (Recovered e a) ->
            Almost (Recovered e (fn a))

        Failure f ->
            Failure f


renderBlock : Block data -> Description -> Outcome Error.AstError (Uncertain data) data
renderBlock fromBlock =
    case fromBlock of
        Block name { converter } ->
            converter

        Value { converter } ->
            converter


getBlockExpectation fromBlock =
    case fromBlock of
        Block name { expect } ->
            expect

        Value { expect } ->
            expect


blockName : Block data -> Maybe String
blockName fromBlock =
    case fromBlock of
        Block name _ ->
            Just name

        Value _ ->
            Nothing


getParser : Id.Seed -> Block data -> ( Id.Seed, Parser Error.Context Error.Problem Description )
getParser seed fromBlock =
    case fromBlock of
        Block name { parser } ->
            let
                ( newSeed, blockParser ) =
                    parser seed
            in
            ( newSeed
            , Parser.succeed identity
                |. Parser.token (Parser.Token "|>" (Error.ExpectingBlockName name))
                |. Parser.chompWhile (\c -> c == ' ')
                |= blockParser
            )

        Value { parser } ->
            parser seed


getParserNoBar seed fromBlock =
    case fromBlock of
        Block name { parser } ->
            let
                ( newSeed, blockParser ) =
                    parser seed
            in
            ( newSeed
            , blockParser
            )

        Value { parser } ->
            parser seed


{-| -}
type Style
    = Bold
    | Italic
    | Strike


{-| A text fragment with some styling.
-}
type Text
    = Text Styling String


type alias Styling =
    { bold : Bool
    , italic : Bool
    , strike : Bool
    }


emptyStyles =
    { bold = False
    , italic = False
    , strike = False
    }


{-| -}
type Expectation
    = ExpectBlock String Expectation
    | ExpectStub String
    | ExpectRecord String (List ( String, Expectation ))
    | ExpectOneOf (List Expectation)
    | ExpectManyOf (List Expectation)
    | ExpectStartsWith Expectation Expectation
    | ExpectBoolean Bool
    | ExpectInteger Int
    | ExpectFloat Float
    | ExpectFloatBetween
        { min : Float
        , max : Float
        , default : Float
        }
    | ExpectIntBetween
        { min : Int
        , max : Int
        , default : Int
        }
    | ExpectText (List InlineExpectation)
    | ExpectString String
    | ExpectMultiline String
    | ExpectStringExactly String
    | ExpectTree Expectation
    | ExpectNothing


{-| -}
type InlineExpectation
    = ExpectAnnotation String (List AttrExpectation)
    | ExpectToken String (List AttrExpectation)
    | ExpectVerbatim String (List AttrExpectation)


noInlineAttributes expect =
    case expect of
        ExpectAnnotation _ attrs ->
            List.isEmpty attrs

        ExpectToken _ attrs ->
            List.isEmpty attrs

        ExpectVerbatim _ attrs ->
            List.isEmpty attrs


{-| -}
type AttrExpectation
    = ExpectAttrString String



-- | ExpectAttr
-- | ExpectInlineText


inlineExample : InlineExpectation -> String
inlineExample inline =
    let
        inlineAttrExamples attrs =
            attrs
                |> List.map renderAttr
                |> String.join ", "

        renderAttr attr =
            case attr of
                ExpectAttrString name ->
                    name ++ " = A String"
    in
    case inline of
        ExpectAnnotation name attrs ->
            if List.isEmpty attrs then
                "[some styled text]{" ++ name ++ "}"

            else
                "[some styled text]{" ++ name ++ "|" ++ inlineAttrExamples attrs ++ "}"

        ExpectToken name attrs ->
            if List.isEmpty attrs then
                "{" ++ name ++ "}"

            else
                "{" ++ name ++ "|" ++ inlineAttrExamples attrs ++ "}"

        ExpectVerbatim name attrs ->
            if List.isEmpty attrs then
                "`some styled text`"

            else
                "`some styled text`{" ++ name ++ "|" ++ inlineAttrExamples attrs ++ "}"


choiceExpectation (Choice id exp) =
    exp


match description exp =
    case description of
        DescribeNothing ->
            case exp of
                ExpectNothing ->
                    True

                _ ->
                    False

        DescribeBlock details ->
            case exp of
                ExpectBlock expectedName expectedChild ->
                    if expectedName == details.name then
                        matchExpected details.expected expectedChild

                    else
                        False

                _ ->
                    False

        DescribeStub name found ->
            case exp of
                ExpectStub expectedName ->
                    name == expectedName

                _ ->
                    False

        Record details ->
            matchExpected details.expected exp

        OneOf one ->
            matchExpected (ExpectOneOf (List.map choiceExpectation one.choices)) exp

        ManyOf many ->
            matchExpected (ExpectManyOf (List.map choiceExpectation many.choices)) exp

        StartsWith range start end ->
            case exp of
                ExpectStartsWith startExp endExp ->
                    match start.found startExp && match end.found endExp

                _ ->
                    False

        DescribeTree tree ->
            matchExpected tree.expected exp

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

        DescribeFloatBetween _ ->
            case exp of
                ExpectFloatBetween _ ->
                    True

                _ ->
                    False

        DescribeIntBetween _ ->
            case exp of
                ExpectIntBetween _ ->
                    True

                _ ->
                    False

        DescribeText _ ->
            case exp of
                ExpectText _ ->
                    True

                _ ->
                    False

        DescribeString _ _ _ ->
            case exp of
                ExpectString _ ->
                    True

                _ ->
                    False

        DescribeMultiline _ _ _ ->
            case exp of
                ExpectMultiline _ ->
                    True

                _ ->
                    False

        DescribeStringExactly _ _ ->
            case exp of
                ExpectStringExactly _ ->
                    True

                _ ->
                    False


{-| Is the first expectation a subset of the second?
-}
matchExpected : Expectation -> Expectation -> Bool
matchExpected subExp expected =
    case ( subExp, expected ) of
        ( ExpectBlock oneName oneExp, ExpectBlock twoName twoExp ) ->
            oneName == twoName && matchExpected oneExp twoExp

        ( ExpectStub one, ExpectStub two ) ->
            one == two

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

        ( ExpectFloatBetween oneDetails, ExpectFloatBetween twoDetails ) ->
            oneDetails.max == twoDetails.max && oneDetails.min == twoDetails.min

        ( ExpectIntBetween oneDetails, ExpectIntBetween twoDetails ) ->
            oneDetails.max == twoDetails.max && oneDetails.min == twoDetails.min

        ( ExpectText oneInline, ExpectText twoInline ) ->
            True

        ( ExpectString _, ExpectString _ ) ->
            True

        ( ExpectMultiline _, ExpectMultiline _ ) ->
            True

        ( ExpectStringExactly oneName, ExpectStringExactly twoName ) ->
            oneName == twoName

        ( ExpectTree oneContent, ExpectTree twoContent ) ->
            True

        _ ->
            False


matchExpectedOptions : List Expectation -> Expectation -> Bool
matchExpectedOptions opts target =
    List.any (matchExpected target) opts


matchFields : List ( String, Expectation ) -> ( String, Expectation ) -> Bool
matchFields valid ( targetFieldName, targetFieldExpectation ) =
    let
        innerMatch ( validFieldName, validExpectation ) =
            validFieldName
                == targetFieldName
                && matchExpected validExpectation targetFieldExpectation
    in
    List.any innerMatch valid


within rangeOne rangeTwo =
    withinOffsetRange { start = rangeOne.start.offset, end = rangeOne.end.offset } rangeTwo


withinOffsetRange offset range =
    range.start.offset <= offset.start && range.end.offset >= offset.end


{-| Given an expectation and a list of choices, verify that the expectation is a valid choice.
-}
make : Expectation -> List (Choice id Expectation) -> Maybe (Choice id Expectation)
make expected options =
    List.filterMap
        (\(Choice id exp) ->
            if matchExpected expected exp then
                Just (Choice id expected)

            else
                Nothing
        )
        options
        |> List.head


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


startingPoint =
    { offset = 0
    , line = 1
    , column = 1
    }



-- push maybePush found =
--     case maybePush of
--         Nothing ->
--             found
--         Just to ->
--             case found of
--                 Found range item ->
--                     Found (pushRange to range) (pushDescription to item)
--                 Unexpected unexpected ->
--                     Unexpected { unexpected | range = pushRange to unexpected.range }
-- pushDescription to desc =
--     case desc of
--         DescribeString id str ->
--             DescribeString (pushId to id) str
--         _ ->
--             desc
-- pushId to (Id range) =
--     Id (pushRange to range)
-- pushRange to range =
--     { start = addPositions to range.start
--     , end = addPositions to range.end
--     }
-- addPositions to pos =
--     { offset = pos.offset + to.offset
--     , line = pos.line + to.line
--     , column = pos.column + to.column
--     }
-- pushFromRange { start, end } =
--     { offset = end.offset - start.offset
--     , line = end.line - start.line
--     , column = end.column - start.column
--     }


minusPosition end start =
    { offset = end.offset - start.offset
    , line = end.line - start.line
    , column = end.column - start.column
    }



-- sizeToRange start delta =
--     { start = start
--     , end =
--         addPositions start delta
--     }


{-| -}
getDescription : Parsed -> Found Description
getDescription (Parsed parsed) =
    parsed.found


{-| -}
getDesc : { start : Int, end : Int } -> Parsed -> List Description
getDesc offset (Parsed parsed) =
    getWithinFound offset parsed.found


{-| -}
getWithinFound : { start : Int, end : Int } -> Found Description -> List Description
getWithinFound offset found =
    case found of
        Found range item ->
            if withinOffsetRange offset range then
                if isPrimitive item then
                    [ item ]

                else
                    [ item ]
                        ++ getContainingDescriptions item offset

            else
                []

        Unexpected unexpected ->
            []


withinFoundLeaf offset found =
    case found of
        Found range item ->
            withinOffsetRange offset range

        Unexpected unexpected ->
            withinOffsetRange offset unexpected.range


isPrimitive : Description -> Bool
isPrimitive description =
    case description of
        DescribeBlock _ ->
            False

        Record _ ->
            False

        OneOf _ ->
            False

        ManyOf _ ->
            False

        StartsWith _ fst snd ->
            False

        DescribeTree details ->
            False

        -- Primitives
        DescribeStub name found ->
            True

        DescribeBoolean found ->
            True

        DescribeInteger found ->
            True

        DescribeFloat found ->
            True

        DescribeFloatBetween _ ->
            True

        DescribeIntBetween _ ->
            True

        DescribeText _ ->
            True

        DescribeString _ _ _ ->
            True

        DescribeMultiline _ _ _ ->
            True

        DescribeStringExactly rng str ->
            True

        DescribeNothing ->
            False


{-| -}
getContainingDescriptions : Description -> { start : Int, end : Int } -> List Description
getContainingDescriptions description offset =
    case description of
        DescribeBlock details ->
            getWithinFound offset details.found

        Record details ->
            case details.found of
                Found range fields ->
                    if withinOffsetRange offset range then
                        List.concatMap (getWithinFound offset << Tuple.second) fields

                    else
                        []

                Unexpected unexpected ->
                    if withinOffsetRange offset unexpected.range then
                        []

                    else
                        []

        OneOf one ->
            getWithinFound offset one.child

        ManyOf many ->
            List.concatMap (getWithinFound offset) many.children

        StartsWith range fst snd ->
            if withinOffsetRange offset range then
                getContainingDescriptions fst.found offset ++ getContainingDescriptions snd.found offset

            else
                []

        DescribeTree details ->
            case details.found of
                ( range, items ) ->
                    if withinOffsetRange offset range then
                        List.concatMap (getWithinNested offset) items

                    else
                        []

        -- Primitives
        DescribeStub name found ->
            if withinFoundLeaf offset found then
                [ description ]

            else
                []

        DescribeBoolean details ->
            if withinFoundLeaf offset details.found then
                [ description ]

            else
                []

        DescribeInteger details ->
            if withinFoundLeaf offset details.found then
                [ description ]

            else
                []

        DescribeFloat details ->
            if withinFoundLeaf offset details.found then
                [ description ]

            else
                []

        DescribeFloatBetween details ->
            if withinFoundLeaf offset details.found then
                [ description ]

            else
                []

        DescribeIntBetween details ->
            if withinFoundLeaf offset details.found then
                [ description ]

            else
                []

        DescribeText txt ->
            if withinOffsetRange offset txt.range then
                [ description ]

            else
                []

        DescribeString id range str ->
            if withinOffsetRange offset range then
                [ description ]

            else
                []

        DescribeMultiline id range str ->
            if withinOffsetRange offset range then
                [ description ]

            else
                []

        DescribeStringExactly rng str ->
            if withinOffsetRange offset rng then
                [ description ]

            else
                []

        DescribeNothing ->
            []


getWithinNested offset (Nested nest) =
    -- case nest.content of
    --     ( desc, items ) ->
    --         getContainingDescriptions desc offset
    --             ++
    List.concatMap
        (\item ->
            getContainingDescriptions item offset
        )
        nest.content



{- DESCRIPTION -> STRING -}


{-| -}
toString : Parsed -> String
toString (Parsed parsed) =
    writeFound writeDescription
        parsed.found
        { indent = 0
        , position = { line = 1, column = 1, offset = 0 }
        , printed = ""
        }
        |> .printed


descriptionToString desc =
    writeDescription
        desc
        { indent = 0
        , position = { line = 1, column = 1, offset = 0 }
        , printed = ""
        }
        |> .printed


getSize desc =
    writeDescription desc
        { indent = 0
        , position = { line = 1, column = 1, offset = 0 }
        , printed = ""
        }
        |> .position
        |> (\pos ->
                { column = pos.column - 1
                , line = pos.line - 1
                , offset = pos.offset
                }
           )


type alias PrintCursor =
    { indent : Int
    , position : Position
    , printed : String
    }


write : String -> PrintCursor -> PrintCursor
write str cursor =
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


writeIndent : PrintCursor -> PrintCursor
writeIndent cursor =
    write (String.repeat (cursor.indent * 4) " ") cursor


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


writeIcon icon cursor =
    case icon of
        Bullet ->
            cursor
                |> write "-"

        AutoNumber ->
            cursor
                |> write "#."


{-| -}
writeDescription : Description -> PrintCursor -> PrintCursor
writeDescription description cursor =
    case description of
        DescribeNothing ->
            cursor

        DescribeBlock details ->
            cursor
                |> write ("|> " ++ details.name)
                |> indent
                |> writeFound writeDescription details.found
                |> dedent

        DescribeStub name found ->
            cursor
                |> write "|> "
                |> writeFound (writeWith identity) found

        Record details ->
            cursor
                |> writeFound
                    (\fields curs ->
                        curs
                            -- TODO: This seems to be necessary for the recordOfRecord test, but
                            -- makes things parsed normally fail...sooo
                            -- |> writeIndent
                            |> write ("|> " ++ details.name)
                            |> indent
                            |> (\c ->
                                    List.foldr writeField c fields
                               )
                    )
                    details.found
                |> dedent

        OneOf one ->
            cursor
                |> writeFound writeDescription one.child

        ManyOf many ->
            List.foldl
                (writeFound writeDescription)
                cursor
                many.children

        StartsWith range start end ->
            cursor
                |> writeDescription start.found
                |> writeDescription end.found

        DescribeBoolean details ->
            writeFound (writeWith boolToString) details.found cursor

        DescribeInteger details ->
            writeFound (writeWith String.fromInt) details.found cursor

        DescribeFloat details ->
            writeFound (writeWith Tuple.first) details.found cursor

        DescribeFloatBetween details ->
            writeFound (writeWith Tuple.first) details.found cursor

        DescribeIntBetween details ->
            writeFound (writeWith String.fromInt) details.found cursor

        DescribeText txt ->
            cursor
                |> advanceTo txt.range
                |> (\c -> List.foldl writeTextDescription c txt.text)

        DescribeString id range str ->
            cursor
                |> advanceTo range
                |> write str

        DescribeMultiline id range str ->
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
                |> advanceTo range
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

        DescribeStringExactly range str ->
            cursor
                |> advanceTo range
                |> write str

        DescribeTree tree ->
            case tree.found of
                ( range, nestedItems ) ->
                    cursor
                        |> advanceTo range
                        |> (\curs -> List.foldl writeNested curs nestedItems)


writeNested (Nested node) cursor =
    cursor
        |> writeIcon node.icon
        |> (\curs -> List.foldl writeDescription curs node.content)
        |> indent
        |> (\curs -> List.foldl writeNested curs node.children)
        |> dedent


textDescriptionToString txt =
    case txt of
        Styled range t ->
            textToString t

        InlineToken details ->
            case details.attributes of
                [] ->
                    "{" ++ details.name ++ "}"

                _ ->
                    "{"
                        ++ details.name
                        ++ " |"
                        ++ String.join ", " (List.map inlineDescToString details.attributes)
                        ++ "}"

        InlineAnnotation details ->
            "["
                ++ String.join "" (List.map textToString details.text)
                ++ "]{"
                ++ String.join ", " (List.map inlineDescToString details.attributes)
                ++ "}"

        InlineVerbatim details ->
            case details.text of
                Text _ str ->
                    if List.isEmpty details.attributes then
                        "`" ++ str ++ "`"

                    else
                        "`"
                            ++ str
                            ++ "`{"
                            ++ String.join ", " (List.map inlineDescToString details.attributes)
                            ++ "}"

        UnexpectedInline unexpected ->
            ""


inlineDescToString : InlineAttribute -> String
inlineDescToString inlineDesc =
    case inlineDesc of
        AttrString { name, range, value } ->
            name ++ " = " ++ value



-- DescribeInlineText range txts ->
--     String.join "" (List.map textToString txts)


writeTextDescription desc curs =
    write (textDescriptionToString desc) curs


writeTextNode node curs =
    write (textToString node) curs


textToString : Text -> String
textToString (Text styles txt) =
    txt


writeWith toStr a cursor =
    write (toStr a) cursor


writeFound : (a -> PrintCursor -> PrintCursor) -> Found a -> PrintCursor -> PrintCursor
writeFound fn found cursor =
    case found of
        Found range fnd ->
            cursor
                |> advanceTo range
                |> fn fnd

        Unexpected unexpected ->
            cursor
                |> advanceTo unexpected.range


writeField : ( String, Found Description ) -> PrintCursor -> PrintCursor
writeField ( name, foundVal ) cursor =
    case foundVal of
        Found rng fnd ->
            cursor
                |> advanceTo rng
                |> write (name ++ " =")
                |> writeDescription fnd

        Unexpected unexpected ->
            cursor
                |> advanceTo unexpected.range



{- CREATION -}


{-|

    `Position` is the starting position for a block.

    The same rules for indentation as they apply everywhere.

        - Primitives do not handle their indentation.
        - Block, Record, and Tree elements handle the indentation of their children.

-}
create :
    { seed : Id.Seed
    , indent : Int
    , base : Position
    , expectation : Expectation
    }
    ->
        { pos : Position
        , desc : Description
        , seed : Id.Seed
        }
create current =
    case current.expectation of
        ExpectBlock name childExpectation ->
            let
                new =
                    create
                        { indent = current.indent + 1
                        , base =
                            current.base
                                |> moveNewline
                                |> moveColumn ((current.indent + 1) * 4)
                        , expectation = childExpectation
                        , seed = current.seed
                        }
            in
            { pos = new.pos
            , desc =
                DescribeBlock
                    { name = name
                    , found =
                        Found
                            { start = current.base
                            , end = new.pos
                            }
                            new.desc
                    , expected = current.expectation
                    }
            , seed = new.seed
            }

        ExpectStub name ->
            let
                end =
                    moveColumn (String.length name) current.base
            in
            { pos = end
            , desc =
                DescribeStub name
                    (Found
                        { start = current.base
                        , end = end
                        }
                        name
                    )
            , seed = current.seed
            }

        ExpectRecord name fields ->
            let
                new =
                    List.foldl
                        (createField (current.indent + 1))
                        { position = moveNewline current.base
                        , fields = []
                        , seed = current.seed
                        }
                        fields
            in
            { pos = new.position
            , desc =
                Record
                    { name = name
                    , found =
                        Found
                            { start = current.base
                            , end = moveNewline new.position
                            }
                            new.fields
                    , expected = current.expectation
                    }
            , seed = new.seed
            }

        ExpectTree content ->
            let
                range =
                    { start = current.base
                    , end = current.base
                    }

                items =
                    []
            in
            { pos = moveNewline current.base
            , desc =
                DescribeTree
                    { found = ( range, items )
                    , expected = current.expectation
                    }
            , seed = current.seed
            }

        ExpectOneOf choices ->
            let
                ( newId, newSeed ) =
                    Id.step current.seed

                -- TODO: handle case of empty OneOf
                new =
                    create
                        { indent = current.indent + 1
                        , base = current.base
                        , expectation = Maybe.withDefault (ExpectStub "Unknown") (List.head choices)
                        , seed = newSeed
                        }
            in
            { pos = new.pos
            , desc =
                OneOf
                    { id = newId
                    , choices = List.map (Choice newId) choices
                    , child =
                        Found
                            { start = current.base
                            , end = new.pos
                            }
                            new.desc
                    }
            , seed = new.seed
            }

        ExpectManyOf choices ->
            let
                ( parentId, newSeed ) =
                    Id.step current.seed

                ( _, childStart ) =
                    Id.step newSeed

                reseeded =
                    Id.reseed childStart

                ( finalSeed, lastPos, children ) =
                    List.foldl
                        (\choice ( seed, newBase, result ) ->
                            let
                                new =
                                    create
                                        { indent = current.indent
                                        , base = newBase
                                        , expectation = choice
                                        , seed = seed
                                        }
                            in
                            ( new.seed
                            , new.pos
                                |> moveNewline
                                |> moveColumn (current.indent * 4)
                            , Found
                                { start = newBase
                                , end = new.pos
                                }
                                new.desc
                                :: result
                            )
                        )
                        ( newSeed, current.base, [] )
                        choices
                        |> (\( s, p, c ) -> ( s, p, List.reverse c ))
            in
            { pos = moveNewline lastPos
            , seed = finalSeed
            , desc =
                ManyOf
                    { id = parentId
                    , range =
                        { start = current.base
                        , end = lastPos
                        }
                    , choices = List.map (Choice parentId) choices
                    , children = children
                    }
            }

        ExpectStartsWith start remaining ->
            let
                first =
                    create
                        { indent = current.indent
                        , base = current.base
                        , expectation = start
                        , seed = current.seed
                        }

                second =
                    create
                        { indent = current.indent
                        , base = moveNewline first.pos
                        , expectation = remaining
                        , seed = first.seed
                        }
            in
            { pos = second.pos
            , desc =
                StartsWith
                    { start = current.base
                    , end = second.pos
                    }
                    { found = first.desc
                    , expected = start
                    }
                    { found = second.desc
                    , expected = remaining
                    }
            , seed = second.seed
            }

        -- Primitives
        ExpectBoolean b ->
            let
                boolString =
                    boolToString b

                end =
                    moveColumn (String.length boolString) current.base

                range =
                    { start = current.base
                    , end = end
                    }

                ( newId, newSeed ) =
                    Id.step current.seed
            in
            { pos = end
            , desc =
                DescribeBoolean
                    { id = newId
                    , found =
                        Found range b
                    }
            , seed = newSeed
            }

        ExpectInteger i ->
            let
                end =
                    moveColumn
                        (String.length (String.fromInt i))
                        current.base

                pos =
                    { start = current.base
                    , end = end
                    }

                ( newId, newSeed ) =
                    Id.step current.seed
            in
            { pos = end
            , desc =
                DescribeInteger
                    { id = newId
                    , found = Found pos i
                    }
            , seed = newSeed
            }

        ExpectFloat f ->
            let
                end =
                    moveColumn
                        (String.length (String.fromFloat f))
                        current.base

                pos =
                    { start = current.base
                    , end = end
                    }

                ( newId, newSeed ) =
                    Id.step current.seed
            in
            { pos = end
            , desc =
                DescribeFloat
                    { id = newId
                    , found = Found pos ( String.fromFloat f, f )
                    }
            , seed = newSeed
            }

        ExpectFloatBetween details ->
            let
                end =
                    moveColumn
                        (String.length (String.fromFloat details.default))
                        current.base

                pos =
                    { start = current.base
                    , end =
                        end
                    }

                ( newId, newSeed ) =
                    Id.step current.seed
            in
            { pos = end
            , desc =
                DescribeFloatBetween
                    { id = newId
                    , min = details.min
                    , max = details.max
                    , found =
                        Found pos
                            ( String.fromFloat details.default
                            , details.default
                            )
                    }
            , seed = newSeed
            }

        ExpectIntBetween details ->
            let
                end =
                    moveColumn
                        (String.length (String.fromInt details.default))
                        current.base

                pos =
                    { start = current.base
                    , end = end
                    }

                ( newId, newSeed ) =
                    Id.step current.seed
            in
            { pos = end
            , desc =
                DescribeIntBetween
                    { id = newId
                    , min = details.min
                    , max = details.max
                    , found = Found pos details.default
                    }
            , seed = newSeed
            }

        ExpectString str ->
            let
                end =
                    moveColumn (String.length str) current.base

                pos =
                    { start = current.base
                    , end = end
                    }

                ( newId, newSeed ) =
                    Id.step current.seed
            in
            { pos = end
            , desc = DescribeString newId pos str
            , seed = newSeed
            }

        ExpectMultiline str ->
            let
                end =
                    moveColumn (String.length str) current.base

                -- TODO: This position is not correct!
                -- Account for newlines
                pos =
                    { start = current.base
                    , end = end
                    }

                ( newId, newSeed ) =
                    Id.step current.seed
            in
            { pos = end
            , desc = DescribeMultiline newId pos str
            , seed = newSeed
            }

        ExpectStringExactly str ->
            let
                end =
                    moveColumn (String.length str) current.base

                pos =
                    { start = current.base
                    , end = end
                    }
            in
            { pos = end
            , desc = DescribeStringExactly pos str
            , seed = current.seed
            }

        -- ExpectDate ->
        _ ->
            let
                end =
                    moveColumn (String.length "True") current.base

                range =
                    { start = current.base
                    , end = end
                    }

                ( newId, newSeed ) =
                    Id.step current.seed
            in
            { pos = end
            , desc =
                DescribeBoolean
                    { id = newId
                    , found =
                        Found
                            range
                            True
                    }
            , seed = newSeed
            }


type alias CreateFieldCursor =
    { position : Position

    -- We've already created these
    , fields : List ( String, Found Description )
    , seed : Id.Seed
    }


{-| -}
createField :
    Int
    -> ( String, Expectation )
    -> CreateFieldCursor
    -> CreateFieldCursor
createField currentIndent ( name, exp ) current =
    let
        -- This is the beginning of the field
        --    field = x
        --   ^ right there
        fieldValueStart =
            current.position
                |> moveColumn (currentIndent * 4)

        -- If the field value is more than a line
        new =
            create
                { indent = currentIndent + 1
                , base =
                    fieldValueStart
                        -- Add a few characters to account for `field = `.
                        |> moveColumn (String.length name + 3)
                , expectation = exp
                , seed = current.seed
                }

        height =
            new.pos.line - current.position.line

        fieldEnd =
            moveNewline new.pos
    in
    if height == 0 then
        { position = fieldEnd
        , fields =
            ( name
            , Found
                { start =
                    fieldValueStart
                , end =
                    fieldEnd
                }
                new.desc
            )
                :: current.fields
        , seed = new.seed
        }

    else
        let
            -- If the field value is more than a line
            multiline =
                create
                    { indent = currentIndent + 1
                    , base =
                        fieldValueStart
                            -- account for just `fieldname =`
                            |> moveColumn (String.length name + 2)
                            |> moveNewline
                            -- Indent one level further
                            |> moveColumn ((currentIndent + 1) * 4)
                    , expectation = exp
                    , seed = current.seed
                    }

            finalEnd =
                multiline.pos
                    |> moveNewline
        in
        { position = finalEnd
        , fields =
            ( name
            , Found
                { start =
                    fieldValueStart
                , end =
                    finalEnd
                }
                multiline.desc
            )
                :: current.fields
        , seed = multiline.seed
        }



-- {-| -}
-- parse : Document data -> String -> Result (List (Parser.DeadEnd Error.Context Error.Problem)) Parsed
-- parse (Document blocks) source =
--     Parser.run blocks.parser source


{-| -}
type alias Partial data =
    { errors : List Error.Rendered
    , result : data
    }


{-| -}
startDocRange : Range
startDocRange =
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


errorsToList ( fst, remain ) =
    fst :: remain


{-| -}
compile :
    Document data
    -> String
    ->
        Result
            (Outcome (List Error.Rendered)
                { errors : List Error.Rendered
                , result : data
                }
                data
            )
            ( Parsed
            , Outcome (List Error.Rendered)
                { errors : List Error.Rendered
                , result : data
                }
                data
            )
compile (Document blocks) source =
    case Parser.run blocks.parser source of
        Ok ((Parsed parsedDetails) as parsed) ->
            (Ok << Tuple.pair parsed) <|
                case parsedDetails.errors of
                    [] ->
                        case blocks.converter parsed of
                            Success rendered ->
                                Success rendered

                            Almost (Recovered errors rendered) ->
                                Almost
                                    { errors = List.map (Error.render source) (errorsToList errors)
                                    , result = rendered
                                    }

                            Almost (Uncertain errors) ->
                                -- now we're certain :/
                                Failure (List.map (Error.render source) (errorsToList errors))

                            Failure Error.NoMatch ->
                                -- Invalid Ast.
                                -- This should never happen because
                                -- we definitely have the same document in both parsing and converting.
                                Failure
                                    [ Error.documentMismatch ]

                    _ ->
                        case blocks.converter parsed of
                            Success rendered ->
                                Almost
                                    { errors = parsedDetails.errors
                                    , result = rendered
                                    }

                            Almost (Uncertain ( err, remainError )) ->
                                Failure (List.map (Error.render source) (err :: remainError))

                            Almost (Recovered ( err, remainError ) result) ->
                                Almost
                                    { errors =
                                        List.map (Error.render source) (err :: remainError)
                                    , result = result
                                    }

                            Failure noMatch ->
                                Failure
                                    (Error.documentMismatch
                                        :: parsedDetails.errors
                                    )

        Err deadEnds ->
            Err <|
                Failure
                    [ Error.renderParsingErrors source deadEnds
                    ]


errorsToMessages source outcome =
    case outcome of
        Success s ->
            Success s

        Almost { errors, result } ->
            Almost
                { errors = List.map (Error.render source) errors
                , result = result
                }

        Failure errors ->
            Failure (List.map (Error.render source) errors)


{-| Render is a little odd.

We should always expect `Parsed` to either already have our error messages, or to succeed.

Render can't add additional errors.

-}
render :
    Document data
    -> Parsed
    -> Outcome (List Error.Rendered) { errors : List Error.Rendered, result : data } data
render (Document blocks) ((Parsed parsedDetails) as parsed) =
    case parsedDetails.errors of
        [] ->
            case blocks.converter parsed of
                Success rendered ->
                    Success rendered

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
                        , result = rendered
                        }

                Almost (Uncertain _) ->
                    Failure parsedDetails.errors

                Almost (Recovered _ result) ->
                    Almost
                        { errors = parsedDetails.errors
                        , result = result
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
            "| " ++ name

        ExpectStub stubName ->
            "| " ++ stubName

        ExpectRecord name fields ->
            "| " ++ name

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

        ExpectFloatBetween bounds ->
            "A Float between " ++ String.fromFloat bounds.min ++ " and " ++ String.fromFloat bounds.max

        ExpectIntBetween bounds ->
            "An Int between " ++ String.fromInt bounds.min ++ " and " ++ String.fromInt bounds.max

        ExpectText inlines ->
            "Styled Text"

        ExpectString _ ->
            "A String"

        ExpectMultiline _ ->
            "A Multiline String"

        ExpectStringExactly exact ->
            exact

        ExpectTree content ->
            "A tree starting of "
                ++ humanReadableExpectations content
                ++ " content"
