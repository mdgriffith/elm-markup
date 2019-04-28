module Mark.Internal.Description exposing
    ( render, compile
    , Found(..), Nested(..), Icon(..)
    , Description(..), TextDescription(..), InlineAttribute(..), Text(..), Style(..)
    , Expectation(..), InlineExpectation(..), AttrExpectation(..), TreeExpectation(..)
    , Parsed(..), startingPoint, descriptionToString, toString, mergeWith
    , create, createInline
    , Styling, emptyStyles
    , inlineExample, blockName, uncertain, humanReadableExpectations
    , Uncertain(..), mapSuccessAndRecovered, renderBlock, getBlockExpectation, getParser, getParserNoBar, noInlineAttributes
    , Block(..), Document(..), Inline(..)
    , boldStyle, italicStyle, strikeStyle
    , resultToFound, getId, expectationToAttr, mapFound, mapNested, textDescriptionRange, getSize, sizeFromRange, minusSize, textSize
    )

{-|

@docs render, compile

@docs Found, Nested, Icon

@docs Description, TextDescription, InlineAttribute, Text, Style

@docs Expectation, InlineExpectation, AttrExpectation, TreeExpectation

@docs Parsed, startingPoint, descriptionToString, toString, mergeWith

@docs create, createInline

@docs Styling, emptyStyles

@docs inlineExample, blockName, uncertain, humanReadableExpectations

@docs Uncertain, mapSuccessAndRecovered, renderBlock, getBlockExpectation, getParser, getParserNoBar, noInlineAttributes

@docs Block, Document, Inline

@docs boldStyle, italicStyle, strikeStyle

@docs resultToFound, getId, expectationToAttr, mapFound, mapNested, textDescriptionRange, getSize, sizeFromRange, foundRange, minusSize, textSize

-}

import Mark.Internal.Error as Error
import Mark.Internal.Format as Format
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
    | AutoNumber Int


{-| -}
type Nested item
    = Nested
        { icon : Icon
        , content : List item
        , children :
            List (Nested item)
        }


mapNested : (a -> b) -> Nested a -> Nested b
mapNested fn (Nested nest) =
    Nested
        { icon = nest.icon
        , content = List.map fn nest.content
        , children =
            List.map (mapNested fn) nest.children
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
        { id : Id
        , name : String
        , found : Found Description
        , expected : Expectation
        }
    | Record
        { id : Id
        , name : String
        , found : Found (List ( String, Found Description ))
        , expected : Expectation
        }
    | OneOf
        { id : Id
        , choices : List Expectation
        , child : Found Description
        }
    | ManyOf
        { id : Id
        , range : Range
        , choices : List Expectation
        , children : List (Found Description)
        }
    | StartsWith
        { range : Range
        , id : Id
        , first :
            { found : Description
            , expected : Expectation
            }
        , second :
            { found : Description
            , expected : Expectation
            }
        }
    | DescribeTree
        { id : Id
        , range : Range
        , children : List (Nested Description)
        , expected : Expectation
        }
      -- Primitives
    | DescribeBoolean
        { id : Id
        , found : Found Bool
        }
    | DescribeInteger
        { id : Id
        , found : Found Int
        }
    | DescribeFloat
        { id : Id
        , found : Found ( String, Float )
        }
    | DescribeText
        { id : Id
        , range : Range
        , text : List TextDescription
        }
    | DescribeString Id Range String
    | DescribeMultiline Id Range String
    | DescribeNothing Id


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

        OneOf details ->
            details.id

        ManyOf details ->
            details.id

        StartsWith details ->
            details.id

        DescribeTree details ->
            details.id

        DescribeBoolean details ->
            details.id

        DescribeInteger details ->
            details.id

        DescribeFloat details ->
            details.id

        DescribeText details ->
            details.id

        DescribeString id _ _ ->
            id

        DescribeMultiline id _ _ ->
            id

        DescribeNothing id ->
            id


minusSize :
    { offset : Int, line : Int }
    -> { offset : Int, line : Int }
    -> { offset : Int, line : Int }
minusSize one two =
    { offset = one.offset - two.offset
    , line = one.line - two.line
    }


textSize : List TextDescription -> { offset : Int, line : Int }
textSize els =
    case els of
        [] ->
            { offset = 0, line = 0 }

        start :: remain ->
            case List.head (List.reverse remain) of
                Nothing ->
                    sizeFromRange (textDescriptionRange start)

                Just last ->
                    minusSize
                        (sizeFromRange (textDescriptionRange start))
                        (sizeFromRange (textDescriptionRange last))


getSize : Description -> { offset : Int, line : Int }
getSize description =
    case description of
        DescribeBlock details ->
            sizeFromRange (getFoundSize details.found)

        Record details ->
            sizeFromRange (getFoundSize details.found)

        OneOf details ->
            sizeFromRange (getFoundSize details.child)

        ManyOf details ->
            sizeFromRange details.range

        StartsWith details ->
            sizeFromRange details.range

        DescribeTree details ->
            sizeFromRange details.range

        DescribeBoolean details ->
            sizeFromRange (getFoundSize details.found)

        DescribeInteger details ->
            sizeFromRange (getFoundSize details.found)

        DescribeFloat details ->
            sizeFromRange (getFoundSize details.found)

        DescribeText details ->
            sizeFromRange details.range

        DescribeString _ range _ ->
            sizeFromRange range

        DescribeMultiline _ range _ ->
            sizeFromRange range

        DescribeNothing id ->
            { offset = 0
            , line = 0
            }


sizeFromRange range =
    { offset = range.end.offset - range.start.offset
    , line = range.end.line - range.start.line
    }


{-| -}
type Proved
    = Proved Id (List (Found Description))


textDescriptionRange : TextDescription -> Range
textDescriptionRange textDesc =
    case textDesc of
        Styled rng _ ->
            rng

        InlineAnnotation details ->
            details.range

        InlineToken details ->
            details.range

        InlineVerbatim details ->
            details.range

        UnexpectedInline details ->
            details.range


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


{-| A text fragment with some styling.
-}
type Text
    = Text Styling String


type Inline data
    = Inline
        { converter : List Text -> List InlineAttribute -> Outcome Error.AstError (Uncertain (List data)) (List data)
        , expect : InlineExpectation
        , name : String
        }


type alias Styling =
    { bold : Bool
    , italic : Bool
    , strike : Bool
    }


{-| -}
type InlineAttribute
    = AttrString
        { name : String
        , range : Range
        , value : String
        }
    | AttrInt
        { name : String
        , range : Range
        , value : Int
        }
    | AttrFloat
        { name : String
        , range : Range
        , value : ( String, Float )
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
    | ExpectMultiline String
    | ExpectTree Expectation (List TreeExpectation)
    | ExpectNothing


{-| -}
type TreeExpectation
    = TreeExpectation
        { icon : Icon
        , content : List Expectation
        , children : List TreeExpectation
        }


{-| -}
type InlineExpectation
    = ExpectText Text
    | ExpectAnnotation String (List AttrExpectation) (List Text)
      -- tokens have no placeholder
    | ExpectToken String (List AttrExpectation)
      -- name, attrs, placeholder content
    | ExpectVerbatim String (List AttrExpectation) String


noInlineAttributes expect =
    case expect of
        ExpectAnnotation _ attrs _ ->
            List.isEmpty attrs

        ExpectToken _ attrs ->
            List.isEmpty attrs

        ExpectVerbatim _ attrs _ ->
            List.isEmpty attrs

        ExpectText _ ->
            True


expectationToAttr : AttrExpectation -> InlineAttribute
expectationToAttr exp =
    case exp of
        ExpectAttrString name default ->
            AttrString
                { name = name
                , range = emptyRange
                , value = default
                }

        ExpectAttrFloat name default ->
            AttrFloat
                { name = name
                , range = emptyRange
                , value = default
                }

        ExpectAttrInt name default ->
            AttrInt
                { name = name
                , range = emptyRange
                , value = default
                }


{-| -}
type
    AttrExpectation
    --                 name   default
    = ExpectAttrString String String
    | ExpectAttrFloat String ( String, Float )
    | ExpectAttrInt String Int


{-| -}
mapFound : (a -> b) -> Found a -> Found b
mapFound fn found =
    case found of
        Found range item ->
            Found range (fn item)

        Unexpected unexp ->
            Unexpected unexp


getFoundSize : Found a -> Range
getFoundSize found =
    case found of
        Found range _ ->
            range

        Unexpected unexp ->
            unexp.range


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


inlineExample : InlineExpectation -> String
inlineExample inline =
    let
        inlineAttrExamples attrs =
            attrs
                |> List.map renderAttr
                |> String.join ", "

        renderAttr attr =
            case attr of
                ExpectAttrString name _ ->
                    name ++ " = A String"

                ExpectAttrFloat name _ ->
                    name ++ " = A Float"

                ExpectAttrInt name _ ->
                    name ++ " = An Int"
    in
    case inline of
        ExpectText text ->
            ""

        ExpectAnnotation name attrs placeholder ->
            if List.isEmpty attrs then
                "[some styled text]{" ++ name ++ "}"

            else
                "[some styled text]{"
                    ++ name
                    ++ "|"
                    ++ inlineAttrExamples attrs
                    ++ "}"

        ExpectToken name attrs ->
            if List.isEmpty attrs then
                "{" ++ name ++ "}"

            else
                "{" ++ name ++ "|" ++ inlineAttrExamples attrs ++ "}"

        ExpectVerbatim name attrs placeholder ->
            if List.isEmpty attrs then
                "`some styled text`"

            else
                "`some styled text`{"
                    ++ name
                    ++ "|"
                    ++ inlineAttrExamples attrs
                    ++ "}"


match description exp =
    case description of
        DescribeNothing _ ->
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

        Record details ->
            matchExpected details.expected exp

        OneOf one ->
            matchExpected (ExpectOneOf one.choices) exp

        ManyOf many ->
            matchExpected (ExpectManyOf many.choices) exp

        StartsWith details ->
            case exp of
                ExpectStartsWith startExp endExp ->
                    match details.first.found startExp
                        && match details.second.found endExp

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

        DescribeText _ ->
            case exp of
                ExpectTextBlock _ ->
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

        ( ExpectMultiline _, ExpectMultiline _ ) ->
            True

        ( ExpectTree oneContent _, ExpectTree twoContent _ ) ->
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
make : Expectation -> List Expectation -> Maybe Expectation
make expected options =
    List.filterMap
        (\exp ->
            if matchExpected expected exp then
                Just expected

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


moveNewlines : Int -> Position -> Position
moveNewlines i pos =
    { offset = pos.offset + i
    , column = 1
    , line = pos.line + i
    }


startingPoint =
    { offset = 0
    , line = 1
    , column = 1
    }


minusPosition end start =
    { offset = end.offset - start.offset
    , line = end.line - start.line
    , column = end.column - start.column
    }


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

        StartsWith _ ->
            False

        DescribeTree details ->
            False

        -- Primitives
        DescribeBoolean found ->
            True

        DescribeInteger found ->
            True

        DescribeFloat found ->
            True

        DescribeText _ ->
            True

        DescribeString _ _ _ ->
            True

        DescribeMultiline _ _ _ ->
            True

        DescribeNothing _ ->
            False



-- {-|-}
-- descriptionSize description =
--     case description of
--         DescribeBlock _ ->
--             False
--         Record _ ->
--             False
--         OneOf _ ->
--             False
--         ManyOf _ ->
--             False
--         StartsWith _ ->
--             False
--         DescribeTree details ->
--             False
--         -- Primitives
--         DescribeBoolean found ->
--             True
--         DescribeInteger found ->
--             True
--         DescribeFloat found ->
--             True
--         DescribeText _ ->
--             True
--         DescribeString _ _ _ ->
--             True
--         DescribeMultiline _ _ _ ->
--             True
--         DescribeNothing _ ->
--             False


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

        StartsWith details ->
            if withinOffsetRange offset details.range then
                getContainingDescriptions details.first.found offset
                    ++ getContainingDescriptions details.second.found offset

            else
                []

        DescribeTree details ->
            if withinOffsetRange offset details.range then
                List.concatMap (getWithinNested offset) details.children

            else
                []

        -- Primitives
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

        DescribeNothing _ ->
            []


getWithinNested offset (Nested nest) =
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


descriptionToString : Description -> String
descriptionToString desc =
    writeDescription
        desc
        { indent = 0
        , position = { line = 1, column = 1, offset = 0 }
        , printed = ""
        }
        |> .printed



-- getSize desc =
--     writeDescription desc
--         { indent = 0
--         , position = { line = 1, column = 1, offset = 0 }
--         , printed = ""
--         }
--         |> .position
--         |> (\pos ->
--                 { column = pos.column - 1
--                 , line = pos.line - 1
--                 , offset = pos.offset
--                 }
--            )


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

        AutoNumber i ->
            cursor
                |> write (String.fromInt i ++ ".")


{-| -}
writeDescription : Description -> PrintCursor -> PrintCursor
writeDescription description cursor =
    case description of
        DescribeNothing _ ->
            cursor

        DescribeBlock details ->
            cursor
                |> write ("|> " ++ details.name)
                |> indent
                |> writeFound writeDescription details.found
                |> dedent

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

        StartsWith details ->
            cursor
                |> writeDescription details.first.found
                |> writeDescription details.second.found

        DescribeBoolean details ->
            writeFound (writeWith boolToString) details.found cursor

        DescribeInteger details ->
            writeFound (writeWith String.fromInt) details.found cursor

        DescribeFloat details ->
            writeFound (writeWith Tuple.first) details.found cursor

        DescribeText txt ->
            cursor
                |> advanceTo txt.range
                |> (\c ->
                        txt.text
                            |> List.foldl
                                writeTextDescription
                                { cursor = c
                                , styles = emptyStyles
                                }
                            |> writeTextDescription
                                (Styled emptyRange (Text emptyStyles ""))
                            |> .cursor
                   )

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

        DescribeTree tree ->
            cursor
                |> advanceTo tree.range
                |> (\curs -> List.foldl writeNested curs tree.children)


writeNested (Nested node) cursor =
    cursor
        |> writeIcon node.icon
        |> (\curs -> List.foldl writeDescription curs node.content)
        |> indent
        |> (\curs -> List.foldl writeNested curs node.children)
        |> dedent


textDescriptionToString existingStyles txt =
    case txt of
        Styled range t ->
            textToString existingStyles t

        InlineToken details ->
            Tuple.pair existingStyles <|
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
            let
                ( newStyles, renderedText ) =
                    details.text
                        |> List.foldl gatherText ( existingStyles, "" )
                        |> gatherText (Text emptyStyles "")
            in
            ( newStyles
            , "["
                ++ renderedText
                ++ "]{"
                ++ String.join ", " (List.map inlineDescToString details.attributes)
                ++ "}"
            )

        InlineVerbatim details ->
            case details.text of
                Text _ str ->
                    Tuple.pair existingStyles <|
                        if List.isEmpty details.attributes then
                            "`" ++ str ++ "`"

                        else
                            "`"
                                ++ str
                                ++ "`{"
                                ++ String.join ", " (List.map inlineDescToString details.attributes)
                                ++ "}"

        UnexpectedInline unexpected ->
            ( existingStyles, "" )


inlineDescToString : InlineAttribute -> String
inlineDescToString inlineDesc =
    case inlineDesc of
        AttrString { name, range, value } ->
            name ++ " = " ++ value

        AttrFloat { name, range, value } ->
            name ++ " = " ++ Tuple.first value

        AttrInt { name, range, value } ->
            name ++ " = " ++ String.fromInt value



-- DescribeInlineText range txts ->
--     String.join "" (List.map textToString txts)


writeTextDescription desc cursorAndStyles =
    let
        ( newStyles, newStr ) =
            textDescriptionToString cursorAndStyles.styles desc
    in
    { cursor = write newStr cursorAndStyles.cursor
    , styles = newStyles
    }



-- writeTextNode node curs =
--     write (textToString node) curs


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


createInline :
    Position
    -> List InlineExpectation
    -> ( Position, List TextDescription )
createInline start current =
    List.foldl inlineExpectationToDesc
        { position = start
        , text = []
        , styling = emptyStyles
        }
        current
        |> (\cursor ->
                ( moveColumn
                    (numberStyleChanges emptyStyles cursor.styling)
                    cursor.position
                , List.reverse cursor.text
                )
           )


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


inlineExpectationToDesc exp cursor =
    case exp of
        ExpectText ((Text newStyling str) as txt) ->
            let
                end =
                    moveText txt cursor.styling cursor.position
            in
            { position = end
            , text =
                Styled
                    { start = cursor.position
                    , end = end
                    }
                    txt
                    :: cursor.text
            , styling = newStyling
            }

        ExpectAnnotation name attrs txts ->
            let
                end =
                    cursor.position
                        |> moveColumn (String.length name + 5)
                        |> moveColumn (attributesLength attrs)
            in
            { position = end
            , styling = cursor.styling
            , text =
                InlineAnnotation
                    { name = name
                    , range =
                        { start = cursor.position
                        , end = end
                        }
                    , text = txts
                    , attributes = List.map expectationToAttr attrs
                    }
                    :: cursor.text
            }

        ExpectToken name attrs ->
            let
                end =
                    cursor.position
                        -- add 5 which accounts for
                        -- two brackets, a bar, and two spaces
                        |> moveColumn (String.length name + 5)
                        |> moveColumn (attributesLength attrs)
            in
            { position = end
            , styling = cursor.styling
            , text =
                InlineToken
                    { name = name
                    , range =
                        { start = cursor.position
                        , end = end
                        }
                    , attributes = List.map expectationToAttr attrs
                    }
                    :: cursor.text
            }

        -- name, attrs, placeholder content
        ExpectVerbatim name attrs content ->
            let
                end =
                    cursor.position
                        |> moveColumn (String.length name + 5)
                        |> moveColumn (attributesLength attrs)
            in
            { position =
                end
            , styling = cursor.styling
            , text =
                InlineVerbatim
                    { name =
                        if name == "" && attrs == [] then
                            Nothing

                        else
                            Just name
                    , range =
                        { start = cursor.position
                        , end = end
                        }
                    , text = Text emptyStyles content
                    , attributes = List.map expectationToAttr attrs
                    }
                    :: cursor.text
            }


attributesLength : List AttrExpectation -> Int
attributesLength attrs =
    let
        sumLength attr count =
            case attr of
                ExpectAttrString name val ->
                    --       2 for a space and a comma
                    count + String.length name + String.length val + 2

                ExpectAttrFloat name ( flStr, _ ) ->
                    count + String.length name + String.length flStr + 2

                ExpectAttrInt name i ->
                    count + String.length name + String.length (String.fromInt i) + 2
    in
    case attrs of
        [] ->
            0

        _ ->
            List.foldl sumLength 0 attrs - 2


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

                ( newId, newSeed ) =
                    Id.step new.seed
            in
            { pos = new.pos
            , desc =
                DescribeBlock
                    { name = name
                    , id = newId
                    , found =
                        Found
                            { start = current.base
                            , end = new.pos
                            }
                            new.desc
                    , expected = current.expectation
                    }
            , seed = newSeed
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

                ( newId, newSeed ) =
                    Id.step new.seed
            in
            { pos = new.position
            , desc =
                Record
                    { name = name
                    , id = newId
                    , found =
                        Found
                            { start = current.base
                            , end = moveNewline new.position
                            }
                            new.fields
                    , expected = current.expectation
                    }
            , seed = newSeed
            }

        ExpectTree content _ ->
            let
                range =
                    { start = current.base
                    , end = current.base
                    }

                items =
                    []

                ( parentId, newSeed ) =
                    Id.step current.seed
            in
            { pos = moveNewline current.base
            , desc =
                DescribeTree
                    { children = items
                    , id = parentId
                    , range = range
                    , expected = current.expectation
                    }
            , seed = newSeed
            }

        ExpectOneOf choices ->
            let
                ( newId, newSeed ) =
                    Id.step current.seed

                -- QUESTION: what about an empty OneOf
                new =
                    create
                        { indent = current.indent + 1
                        , base = current.base
                        , expectation =
                            Maybe.withDefault
                                ExpectNothing
                                (List.head choices)
                        , seed = newSeed
                        }
            in
            { pos = new.pos
            , desc =
                OneOf
                    { id = newId
                    , choices = choices
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
                    , choices = choices
                    , children = children
                    }
            }

        ExpectStartsWith start remaining ->
            let
                ( parentId, newSeed ) =
                    Id.step current.seed

                first =
                    create
                        { indent = current.indent
                        , base = current.base
                        , expectation = start
                        , seed = newSeed
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
                    { id = parentId
                    , range =
                        { start = current.base
                        , end = second.pos
                        }
                    , first =
                        { found = first.desc
                        , expected = start
                        }
                    , second =
                        { found = second.desc
                        , expected = remaining
                        }
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

                walk line ( isStart, at ) =
                    at
                        |> (if isStart then
                                identity

                            else
                                moveColumn ((current.indent + 1) * 4)
                           )
                        |> moveColumn (String.length line)
                        |> moveNewline
                        |> Tuple.pair False

                lineDiff =
                    str
                        |> String.lines
                        |> List.foldl walk ( True, current.base )
                        |> Tuple.second

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

        ExpectTextBlock nodes ->
            let
                ( end, newText ) =
                    createInline current.base nodes

                ( newId, newSeed ) =
                    Id.step current.seed
            in
            { pos = end
            , desc =
                DescribeText
                    { id = newId
                    , range =
                        { start = current.base
                        , end = end
                        }
                    , text = newText
                    }
            , seed = newSeed
            }

        ExpectNothing ->
            let
                ( newId, newSeed ) =
                    Id.step current.seed
            in
            { pos = current.base
            , desc = DescribeNothing newId
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

        ExpectTextBlock inlines ->
            "Styled Text"

        ExpectString _ ->
            "A String"

        ExpectMultiline _ ->
            "A Multiline String"

        ExpectTree content _ ->
            "A tree starting of "
                ++ humanReadableExpectations content
                ++ " content"


mergeWith fn one two =
    case ( one, two ) of
        ( Success renderedOne, Success renderedTwo ) ->
            Success (fn renderedOne renderedTwo)

        ( Almost (Recovered firstErrs fst), Almost (Recovered secondErrs snd) ) ->
            Almost
                (Recovered
                    (mergeErrors firstErrs secondErrs)
                    (fn fst snd)
                )

        ( Almost (Uncertain unexpected), _ ) ->
            Almost (Uncertain unexpected)

        ( _, Almost (Uncertain unexpected) ) ->
            Almost (Uncertain unexpected)

        _ ->
            Failure Error.NoMatch


mergeErrors ( h1, r1 ) ( h2, r2 ) =
    ( h1, r1 ++ h2 :: r2 )


resultToFound result =
    case result of
        Ok ( range, desc ) ->
            Found range desc

        Err ( range, prob ) ->
            Unexpected
                { range = range
                , problem = prob
                }
