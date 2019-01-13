module Mark.Internal exposing
    ( parse, compile
    , Block, Found(..)
    , document
    , block
    , string, int, float, floatBetween, intBetween
    , oneOf, manyOf, startWith
    , getDesc, toString
    , record2, field
    , getContainingDescriptions, prettyDescription, prettyFound, prettyParsed
    )

{-|

@docs parse, compile

@docs Block, Found

@docs document

@docs block

@docs string, int, float, floatBetween, intBetween, bool

@docs oneOf, manyOf, startWith

@docs getDesc, toString

@docs record2, field

-}

import Html
import Html.Attributes
import Iso8601
import Mark.Format as Format
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)
import Time



{- INTERFACE -}
-- parse : Document data -> String -> Parsed
-- update : Edit -> Parsed -> Parsed
-- convert : Document data -> Parsed -> Result (Partial data) data
-- compile :  Document data -> String -> Result (Partial data) data


{-| -}
parse :
    Document data
    -> String
    -> Result (List (Parser.DeadEnd Context Problem)) Parsed
parse (Document blocks) source =
    Parser.run blocks.parser source


{-| -}
compile : Document data -> String -> Result AstError data
compile (Document blocks) source =
    case Parser.run blocks.parser source of
        Ok ast ->
            blocks.converter ast

        Err err ->
            Err InvalidAst


{-| -}
type Document data
    = Document
        { converter : Parsed -> Result AstError data
        , expect : Expectation
        , parser : Parser Context Problem Parsed
        }


type Parsed
    = Parsed (Found Description)


{-| A `Block data` is just a parser that results in `data`.

You'll be building up your `Document` in terms of the `Blocks`.

A block starts with `|` and has a name(already built into the parser)

A value is just a raw parser.

-}
type Block data
    = Block
        String
        { converter : Description -> Result AstError (Found data)
        , expect : Expectation
        , parser : Parser Context Problem Description
        }
    | Value
        { converter : Description -> Result AstError (Found data)
        , expect : Expectation
        , parser : Parser Context Problem Description
        }



{- These are `soft` parse errors.

   Only a few can happen.

       1. Block names don't match (If they do match and the content is wrong, that's an Unexpected)


-}


type ParseError
    = Unknown


getParser : Block data -> Parser Context Problem Description
getParser fromBlock =
    case fromBlock of
        Block name { parser } ->
            Parser.succeed identity
                |. Parser.token (Parser.Token "|" (ExpectingBlockName name))
                |. Parser.chompIf (\c -> c == ' ') Space
                |= parser

        Value { parser } ->
            parser


renderBlock : Block data -> Description -> Result AstError (Found data)
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


type AstError
    = InvalidAst
    | NoMatch


type Found item
    = Found (Range Position) item
    | Unexpected
        { range : Range Position

        -- , source : String
        , problem : ProblemMessage
        }


type alias UnexpectedDetails =
    { range : Range Position

    -- , source : String
    , problem : ProblemMessage
    }


prettyParsed (Parsed foundDescription) =
    prettyFound 0 prettyDescription foundDescription


prettyDescription indent desc =
    case desc of
        DescribeBlock name details ->
            name

        Record name details ->
            name

        OneOf expected foundDesciption ->
            "oneOf"

        ManyOf expected foundRange foundDesciptions ->
            "manyOf"

        StartsWith _ start end ->
            ""

        DescribeTree details ->
            ""

        -- Primitives
        DescribeStub name foundStub ->
            ""

        DescribeBoolean foundBool ->
            ""

        DescribeInteger foundInt ->
            ""

        DescribeFloat foundFloat ->
            ""

        DescribeFloatBetween bottom top foundFloat ->
            ""

        DescribeIntBetween bottom top foundInt ->
            ""

        DescribeText range textDescriptions ->
            ""

        DescribeString range str ->
            str

        DescribeMultiline range str ->
            str

        DescribeStringExactly range str ->
            str

        DescribeDate foundPosix ->
            prettyFound indent (always Iso8601.fromTime) foundPosix


prettyFound indent contentToString found =
    case found of
        Found range actual ->
            String.repeat (indent * 4) " "
                ++ "Found\n"
                ++ contentToString (indent + 1) actual

        Unexpected unexpected ->
            String.repeat (indent * 4) " " ++ "Unexpected"


type Description
    = DescribeBlock
        String
        { found : Found Description
        , expected : Expectation
        }
    | Record
        String
        { found : Found (List ( String, Found Description ))
        , expected : Expectation
        }
    | OneOf (List Expectation) (Found Description)
    | ManyOf (List Expectation) (Range Position) (List (Found Description))
    | StartsWith
        (Range Position)
        { found : Description
        , expected : Expectation
        }
        { found : Description
        , expected : Expectation
        }
    | DescribeTree
        { found : ( Range Position, List (Nested ( Description, List Description )) )
        , expected : Expectation
        }
      -- Primitives
    | DescribeStub String (Found String)
    | DescribeBoolean (Found Bool)
    | DescribeInteger (Found Int)
    | DescribeFloat (Found Float)
    | DescribeFloatBetween Float Float (Found Float)
    | DescribeIntBetween Int Int (Found Int)
    | DescribeText (Range Position) (List TextDescription)
    | DescribeString (Range Position) String
    | DescribeMultiline (Range Position) String
    | DescribeStringExactly (Range Position) String
    | DescribeDate (Found Time.Posix)


type TextDescription
    = Styled (Range Position) Text
    | DescribeInline String (Range Position) (List InlineDescription)


type InlineDescription
    = DescribeInlineString String (Range Position) String
    | DescribeInlineText (Range Position) (List Text)


{-| A text fragment with some styling.
-}
type Text
    = Text (List Style) String


type Expectation
    = ExpectBlock String Expectation
    | ExpectStub String
    | ExpectRecord String (List ( String, Expectation ))
    | ExpectOneOf (List Expectation)
    | ExpectManyOf (List Expectation)
    | ExpectStartsWith Expectation Expectation
    | ExpectBoolean
    | ExpectInteger
    | ExpectFloat
    | ExpectFloatBetween Float Float
    | ExpectIntBetween Int Int
    | ExpectText (List InlineExpectation)
    | ExpectString
    | ExpectMultiline
    | ExpectStringExactly String
    | ExpectDate
    | ExpectTree Expectation Expectation


type InlineExpectation
    = InlineExpectation String (List InlineValueExpectation)


type InlineValueExpectation
    = ExpectInlineString String
    | ExpectInlineText


getDesc : { start : Int, end : Int } -> Parsed -> List Description
getDesc offset (Parsed foundDesc) =
    getWithinFound offset foundDesc


withinOffsetRange range offset =
    range.start.offset <= offset.start && range.end.offset >= offset.end


getWithinFound : { start : Int, end : Int } -> Found Description -> List Description
getWithinFound offset found =
    case found of
        Found range item ->
            if withinOffsetRange range offset then
                if isPrimitive item then
                    [ item ]

                else
                    [ item ]
                        ++ getContainingDescriptions item offset

            else
                []

        Unexpected unexpected ->
            []


withinFoundLeaf found offset =
    case found of
        Found range item ->
            withinOffsetRange range offset

        Unexpected unexpected ->
            withinOffsetRange unexpected.range offset


isPrimitive : Description -> Bool
isPrimitive description =
    case description of
        DescribeBlock name details ->
            False

        Record name details ->
            False

        OneOf expected found ->
            False

        ManyOf expected rng foundList ->
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

        DescribeFloatBetween _ _ found ->
            True

        DescribeIntBetween _ _ found ->
            True

        DescribeText rng textNodes ->
            True

        DescribeString rng str ->
            True

        DescribeMultiline rng str ->
            True

        DescribeStringExactly rng str ->
            True

        DescribeDate found ->
            True


getContainingDescriptions : Description -> { start : Int, end : Int } -> List Description
getContainingDescriptions description offset =
    case description of
        DescribeBlock name details ->
            getWithinFound offset details.found

        Record name details ->
            []

        OneOf expected found ->
            []

        ManyOf expected rng foundList ->
            List.concatMap (getWithinFound offset) foundList

        StartsWith _ fst snd ->
            []

        DescribeTree details ->
            []

        -- Primitives
        DescribeStub name found ->
            []

        DescribeBoolean found ->
            []

        DescribeInteger found ->
            []

        DescribeFloat found ->
            []

        DescribeFloatBetween _ _ found ->
            []

        DescribeIntBetween _ _ found ->
            if withinFoundLeaf found offset then
                [ description ]

            else
                []

        DescribeText rng textNodes ->
            if withinOffsetRange rng offset then
                [ description ]

            else
                []

        DescribeString rng str ->
            if withinOffsetRange rng offset then
                [ description ]

            else
                []

        DescribeMultiline rng str ->
            if withinOffsetRange rng offset then
                [ description ]

            else
                []

        DescribeStringExactly rng str ->
            if withinOffsetRange rng offset then
                [ description ]

            else
                []

        DescribeDate found ->
            []


{-| -}
toString : Int -> Description -> String
toString indent description =
    case description of
        DescribeBlock name details ->
            String.repeat (indent * 4) " "
                ++ "| "
                ++ name
                ++ "\n"
                ++ foundToString (toString (indent + 1)) details.found

        DescribeStub name range ->
            String.repeat (indent * 4) " "
                ++ "| "
                ++ name
                ++ "\n"

        Record name details ->
            String.repeat (indent * 4) " "
                ++ "| "
                ++ name
                ++ "\n"

        -- ++ String.join "\n" (List.map (fieldToString (indent + 1)) fields)
        OneOf expected found ->
            ""

        -- toString indent found
        ManyOf expected range found ->
            found
                |> List.map (foundToString (toString indent))
                |> String.join "\n\n"

        StartsWith range start end ->
            ""

        -- toString indent start ++ toString indent end
        DescribeBoolean foundBoolean ->
            foundToString boolToString foundBoolean

        DescribeInteger found ->
            foundToString String.fromInt found

        DescribeFloat found ->
            foundToString String.fromFloat found

        DescribeFloatBetween low high found ->
            foundToString String.fromFloat found

        DescribeIntBetween low high found ->
            foundToString String.fromInt found

        DescribeText range textNodes ->
            ""

        DescribeString range str ->
            str

        DescribeMultiline range str ->
            str

        DescribeStringExactly range str ->
            str

        DescribeDate foundPosix ->
            ""

        DescribeTree tree ->
            ""


boolToString b =
    if b then
        "True"

    else
        "False"


foundToString innerToString found =
    case found of
        Found _ inner ->
            innerToString inner

        Unexpected unexpected ->
            ":/"


fieldToString indent ( name, val ) =
    String.repeat (indent * 4) " " ++ name ++ " = " ++ toString indent val


type alias ErrorMessage =
    { message : List Format.Text
    , region : { start : Position, end : Position }
    , title : String
    }


{-| -}
type ProblemMessage
    = MsgUnknownBlock (List String)
    | MsgUnknownInline (List String)
    | MsgMissingFields (List String)
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
    | MsgBadFloat String
    | MsgBadInt String
    | MsgBadBool String
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


type alias Position =
    { offset : Int
    , line : Int
    , column : Int
    }


type alias Range x =
    { start : x
    , end : x
    }


{-| -}
type Style
    = Bold
    | Italic
    | Strike


{-| -}
type Nested item
    = Nested
        { content : item
        , children :
            List (Nested item)
        }



{- BLOCKS -}


{-| -}
document :
    ({ found : Found child
     , expected : Expectation
     }
     -> result
    )
    -> Block child
    -> Document result
document renderer child =
    let
        expectation =
            getBlockExpectation child
    in
    Document
        { expect = expectation
        , converter =
            \(Parsed found) ->
                case found of
                    Found range childDesc ->
                        case renderBlock child childDesc of
                            Err err ->
                                Err err

                            Ok renderedChild ->
                                Ok
                                    (renderer
                                        { found = renderedChild
                                        , expected = expectation
                                        }
                                    )

                    Unexpected unexpected ->
                        Ok
                            (renderer
                                { expected = expectation
                                , found = Unexpected unexpected
                                }
                            )
        , parser =
            Parser.map
                (\( range, val ) ->
                    Parsed (Found range val)
                )
                (withRange
                    (Parser.withIndent 0 (getParser child))
                )
        }


{-| -}
block :
    String
    ->
        ({ found : Found child
         , expected : Expectation
         }
         -> result
        )
    -> Block child
    -> Block result
block name renderer child =
    Block name
        { expect = ExpectBlock name (getBlockExpectation child)
        , converter =
            \desc ->
                case desc of
                    DescribeBlock actualBlockName blockDetails ->
                        if actualBlockName == name then
                            case blockDetails.found of
                                Found range found ->
                                    case renderBlock child found of
                                        Err err ->
                                            -- TODO: This generally means that the child had a NoMatch
                                            -- However it's not obvious when this would occur compared to
                                            -- just being Unexpected.
                                            -- I guess if a block, had a containing block and the names didn't match.
                                            -- In Which case, what would the error state be?
                                            Err err

                                        Ok renderedChild ->
                                            Ok
                                                (Found range
                                                    (renderer
                                                        { found =
                                                            -- Found renderedChild
                                                            renderedChild
                                                        , expected = blockDetails.expected
                                                        }
                                                    )
                                                )

                                Unexpected unexpected ->
                                    Ok
                                        (Found unexpected.range
                                            (renderer
                                                { found =
                                                    Unexpected unexpected
                                                , expected = blockDetails.expected
                                                }
                                            )
                                        )

                        else
                            -- The NoMatch error, allows the parent to decide what to do.
                            -- oneOf would go to the next option
                            -- document could default to an error state
                            Err NoMatch

                    _ ->
                        Err NoMatch
        , parser =
            Parser.map
                (\( range, valueResult ) ->
                    case valueResult of
                        Ok value ->
                            DescribeBlock name
                                { found = Found range value
                                , expected = ExpectBlock name (getBlockExpectation child)
                                }

                        Err errorMessage ->
                            DescribeBlock name
                                { found =
                                    Unexpected
                                        { range = range
                                        , problem = errorMessage
                                        }
                                , expected = ExpectBlock name (getBlockExpectation child)
                                }
                )
            <|
                withRange
                    (Parser.getIndent
                        |> Parser.andThen
                            (\indent ->
                                Parser.succeed identity
                                    |. Parser.keyword (Parser.Token name (ExpectingBlockName name))
                                    |. Parser.chompWhile (\c -> c == ' ')
                                    |. skipBlankLineWith ()
                                    |= Parser.oneOf
                                        [ Parser.succeed Ok
                                            |. Parser.token (Parser.Token (String.repeat (indent + 4) " ") (ExpectingIndent (indent + 4)))
                                            |= Parser.withIndent (indent + 4) (Parser.inContext (InBlock name) (getParser child))

                                        -- If we're here, it's because the indentation failed.
                                        -- If the child parser failed in some way, it would
                                        -- take care of that itself by returning Unexpected
                                        , Parser.succeed (Err <| MsgExpectingIndent (indent + 4))
                                            |. Parser.loop "" (raggedIndentedStringAbove indent)
                                        ]
                            )
                    )
        }


blockNameParser name =
    Parser.succeed identity
        |. Parser.keyword (Parser.Token name (ExpectingBlockName name))
        |. Parser.chompWhile (\c -> c == ' ')
        |. Parser.chompIf (\c -> c == '\n') Newline


{-| -}
startWith : (Found ( start, rest ) -> result) -> Block start -> Block rest -> Block result
startWith fn startBlock endBlock =
    Value
        { expect = ExpectStartsWith (getBlockExpectation startBlock) (getBlockExpectation endBlock)
        , converter =
            \desc ->
                case desc of
                    StartsWith range start end ->
                        case ( renderBlock startBlock start.found, renderBlock endBlock end.found ) of
                            ( Ok (Found startRange renderedStart), Ok (Found endRange renderedEnd) ) ->
                                Ok <|
                                    Found
                                        range
                                        (fn (Found range ( renderedStart, renderedEnd )))

                            ( Ok (Unexpected unexpected), _ ) ->
                                Ok (Found range (fn (Unexpected unexpected)))

                            ( _, Ok (Unexpected unexpected) ) ->
                                Ok (Found range (fn (Unexpected unexpected)))

                            _ ->
                                Err InvalidAst

                    _ ->
                        Err InvalidAst
        , parser =
            Parser.succeed
                (\( range, ( begin, end ) ) ->
                    StartsWith range
                        { found = begin
                        , expected = getBlockExpectation startBlock
                        }
                        { found = end
                        , expected = getBlockExpectation endBlock
                        }
                )
                |= withRange
                    (Parser.succeed Tuple.pair
                        |= getParser startBlock
                        |. Parser.loop 0 manyBlankLines
                        |= getParser endBlock
                    )
        }


{-| Skip all blank lines.
-}
manyBlankLines lineCount =
    Parser.oneOf
        [ skipBlankLineWith (Parser.Loop (lineCount + 1))
        , Parser.succeed (Parser.Done ())
        ]


{-| -}
oneOf : (UnexpectedDetails -> a) -> List (Block a) -> Block a
oneOf renderUnexpected blocks =
    let
        gatherParsers myBlock ( names, blks, vals ) =
            case myBlock of
                Block name { parser } ->
                    ( name :: names, Parser.map Ok parser :: blks, vals )

                Value { parser } ->
                    ( names, blks, Parser.map Ok parser :: vals )

        ( blockNames, childBlocks, childValues ) =
            List.foldl gatherParsers ( [], [], [] ) blocks

        blockParser =
            Parser.succeed identity
                |. Parser.token (Parser.Token "|" BlockStart)
                |. Parser.oneOf
                    [ Parser.chompIf (\c -> c == ' ') Space
                    , Parser.succeed ()
                    ]
                |= Parser.oneOf
                    (List.reverse childBlocks
                        ++ [ Parser.succeed (Err (MsgUnknownBlock blockNames))

                           -- |. (Parser.getIndent
                           --         |> Parser.andThen
                           --             (\indent ->
                           --                 Parser.loop "" (indentedString (indent + 4))
                           --             )
                           --    )
                           ]
                    )

        applyDesc description blck found =
            case found of
                Nothing ->
                    case renderBlock blck description of
                        Err err ->
                            found

                        Ok rendered ->
                            Just rendered

                _ ->
                    found

        expectations =
            List.map getBlockExpectation blocks
    in
    Value
        { expect = ExpectOneOf expectations
        , converter =
            \desc ->
                case desc of
                    OneOf expected foundResult ->
                        case foundResult of
                            Found rng found ->
                                case List.foldl (applyDesc found) Nothing blocks of
                                    Nothing ->
                                        Err InvalidAst

                                    Just result ->
                                        Ok result

                            Unexpected unexpected ->
                                Ok (Found unexpected.range (renderUnexpected unexpected))

                    _ ->
                        Err InvalidAst
        , parser =
            Parser.succeed
                (\( range, result ) ->
                    case result of
                        Ok found ->
                            OneOf expectations (Found range found)

                        Err unexpected ->
                            OneOf expectations
                                (Unexpected
                                    { range = range
                                    , problem = unexpected
                                    }
                                )
                )
                |= withRange
                    (Parser.oneOf
                        (blockParser :: List.reverse childValues ++ [ Parser.succeed (Err (MsgBadFloat "")) ])
                    )
        }


{-| Many blocks that are all at the same indentation level.
-}
manyOf : (UnexpectedDetails -> a) -> List (Block a) -> Block (List a)
manyOf renderUnexpected blocks =
    let
        gatherParsers myBlock ( names, blks, vals ) =
            case myBlock of
                Block name { parser } ->
                    ( name :: names, Parser.map Ok parser :: blks, vals )

                Value { parser } ->
                    ( names, blks, Parser.map Ok (withRange parser) :: vals )

        ( blockNames, childBlocks, childValues ) =
            List.foldl gatherParsers ( [], [], [] ) blocks

        blockParser =
            Parser.map
                (\( pos, result ) ->
                    result
                        |> Result.mapError (\er -> ( pos, er ))
                        |> Result.map (\desc -> ( pos, desc ))
                )
                (withRange
                    (Parser.succeed identity
                        |. Parser.token (Parser.Token "|" BlockStart)
                        |. Parser.oneOf
                            [ Parser.chompIf (\c -> c == ' ') Space
                            , Parser.succeed ()
                            ]
                        |= Parser.oneOf
                            (List.reverse childBlocks
                                ++ [ Parser.getIndent
                                        |> Parser.andThen
                                            (\indent ->
                                                Parser.succeed (Err (MsgUnknownBlock blockNames))
                                                    |. word
                                                    |. newline
                                                    |. Parser.loop "" (raggedIndentedStringAbove indent)
                                            )
                                   ]
                            )
                    )
                )

        expectations =
            List.map getBlockExpectation blocks
    in
    Value
        { expect = ExpectManyOf expectations
        , converter =
            \desc ->
                let
                    applyDesc description blck found =
                        case found of
                            Nothing ->
                                case renderBlock blck description of
                                    Err err ->
                                        found

                                    Ok rendered ->
                                        Just rendered

                            _ ->
                                found

                    getRendered : Found Description -> Result AstError (List a) -> Result AstError (List a)
                    getRendered found existingResult =
                        case existingResult of
                            Err err ->
                                Err err

                            Ok existing ->
                                case found of
                                    Unexpected unexpected ->
                                        Ok (renderUnexpected unexpected :: existing)

                                    Found range child ->
                                        case List.foldl (applyDesc child) Nothing blocks of
                                            Nothing ->
                                                Err InvalidAst

                                            Just (Found _ result) ->
                                                Ok (result :: existing)

                                            Just (Unexpected unexpected) ->
                                                Ok (renderUnexpected unexpected :: existing)
                in
                case desc of
                    ManyOf _ range found ->
                        List.foldl getRendered (Ok []) found
                            |> Result.map (\items -> Found range (List.reverse items))

                    _ ->
                        Err NoMatch
        , parser =
            Parser.succeed
                (\( range, results ) ->
                    ManyOf expectations range (List.map resultToFound results)
                )
                |= withRange
                    (Parser.getIndent
                        |> Parser.andThen
                            (\indent ->
                                Parser.loop ( False, [] )
                                    (blocksOrNewlines (Parser.oneOf (blockParser :: List.reverse childValues)) indent)
                            )
                    )
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
    ((Nested nested) ->
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
    let
        expectation =
            ExpectTree (getBlockExpectation config.start) (getBlockExpectation config.item)
    in
    Value
        { expect = expectation
        , converter =
            \description ->
                case description of
                    DescribeTree details ->
                        case details.found of
                            ( pos, nestedDescriptors ) ->
                                case reduceRender (renderTreeNodeSmall config) nestedDescriptors of
                                    Err invalidAst ->
                                        Err invalidAst

                                    Ok (Err unexpectedDetails) ->
                                        Ok (Unexpected unexpectedDetails)

                                    Ok (Ok list) ->
                                        Ok (Found pos list)

                    _ ->
                        Err InvalidAst
        , parser =
            Parser.getIndent
                |> Parser.andThen
                    (\baseIndent ->
                        Parser.map
                            (\( pos, result ) ->
                                DescribeTree
                                    { found = ( pos, buildTree baseIndent result )
                                    , expected = expectation
                                    }
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
        }


{-| -}
record2 :
    String
    -> (Range Position -> one -> two -> data)
    -> (UnexpectedDetails -> data)
    -> Field one
    -> Field two
    -> Block data
record2 recordName renderer renderUnexpected field1 field2 =
    let
        expectations =
            ExpectRecord recordName [ fieldExpectation field1, fieldExpectation field2 ]
    in
    Block recordName
        { expect = expectations
        , converter =
            \desc ->
                case desc of
                    Record name fields ->
                        if name == recordName then
                            case fields.found of
                                Found pos fieldDescriptions ->
                                    Ok (Ok (renderer pos))
                                        |> Result.map2 applyField (getField field1 fieldDescriptions)
                                        |> Result.map2 applyField (getField field2 fieldDescriptions)
                                        |> renderRecordResult renderUnexpected pos

                                Unexpected unexpected ->
                                    Ok (Found unexpected.range (renderUnexpected unexpected))

                        else
                            Err NoMatch

                    _ ->
                        Err NoMatch
        , parser =
            parseRecord recordName expectations [ fieldParser field1, fieldParser field2 ]
        }



{- PRIMITIVE BLOCKS -}


{-| -}
multiline : Block String
multiline =
    Value
        { expect = ExpectMultiline
        , converter =
            \desc ->
                case desc of
                    DescribeMultiline range str ->
                        Ok (Found range str)

                    _ ->
                        Err InvalidAst
        , parser =
            Parser.map
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
        }


{-| -}
string : Block String
string =
    Value
        { expect = ExpectString
        , converter =
            \desc ->
                case desc of
                    DescribeString range str ->
                        Ok (Found range str)

                    _ ->
                        Err InvalidAst
        , parser =
            Parser.succeed
                (\start val end ->
                    DescribeString { start = start, end = end } val
                )
                |= getPosition
                |= Parser.getChompedString
                    (Parser.chompWhile
                        (\c -> c /= '\n')
                    )
                |= getPosition
        }


exactly : String -> value -> Block value
exactly key value =
    Value
        { expect = ExpectStringExactly key
        , converter =
            \desc ->
                case desc of
                    DescribeStringExactly range existingKey ->
                        if key == existingKey then
                            Ok (Found range value)

                        else
                            Err NoMatch

                    _ ->
                        Err NoMatch
        , parser =
            Parser.succeed
                (\start _ end ->
                    DescribeStringExactly { start = start, end = end } key
                )
                |= getPosition
                |= Parser.token (Parser.Token key (Expecting key))
                |= getPosition
        }


int : Block Int
int =
    Value
        { converter =
            \desc ->
                case desc of
                    DescribeInteger found ->
                        Ok found

                    _ ->
                        Err NoMatch
        , expect = ExpectInteger
        , parser = Parser.map DescribeInteger integer
        }


float : Block Float
float =
    Value
        { converter =
            \desc ->
                case desc of
                    DescribeFloat found ->
                        Ok found

                    _ ->
                        Err NoMatch
        , expect = ExpectFloat
        , parser = Parser.map DescribeFloat floating
        }


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
        { expect = ExpectIntBetween bottom top
        , converter =
            \desc ->
                case desc of
                    DescribeIntBetween low high found ->
                        Ok found

                    _ ->
                        Err InvalidAst
        , parser =
            Parser.map
                (\found ->
                    DescribeIntBetween bottom top <|
                        case found of
                            Found rng i ->
                                if i >= bottom && i <= top then
                                    found

                                else
                                    Unexpected
                                        { range = rng
                                        , problem =
                                            MsgIntOutOfRange
                                                { found = i
                                                , min = bottom
                                                , max = top
                                                }
                                        }

                            _ ->
                                found
                )
                integer
        }


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
        { expect = ExpectFloatBetween bottom top
        , converter =
            \desc ->
                case desc of
                    DescribeFloatBetween low high found ->
                        Ok found

                    _ ->
                        Err InvalidAst
        , parser =
            Parser.map
                (\found ->
                    DescribeFloatBetween bottom top <|
                        case found of
                            Found rng i ->
                                if i >= bottom && i <= top then
                                    found

                                else
                                    Unexpected
                                        { range = rng
                                        , problem =
                                            MsgFloatOutOfRange
                                                { found = i
                                                , min = bottom
                                                , max = top
                                                }
                                        }

                            _ ->
                                found
                )
                floating
        }



{- Parser Heleprs -}


{-| -}
type Context
    = InBlock String
    | InInline String
    | InRecord String
    | InRecordField String
    | InRemapped Position


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


{-| -}
raggedIndentedStringAbove : Int -> String -> Parser Context Problem (Parser.Step String String)
raggedIndentedStringAbove indent found =
    Parser.oneOf
        [ Parser.succeed
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
        , Parser.succeed
            (\indentCount str ->
                Parser.Loop (found ++ String.repeat indentCount " " ++ str)
            )
            |= Parser.oneOf
                (indentationBetween (indent + 1) (indent + 4))
            |= Parser.getChompedString
                (Parser.chompWhile
                    (\c -> c /= '\n')
                )
        , Parser.succeed (Parser.Done found)
        ]


{-| Parse any indentation between two bounds, inclusive.
-}
indentationBetween : Int -> Int -> List (Parser Context Problem Int)
indentationBetween lower higher =
    let
        bottom =
            min lower higher

        top =
            max lower higher
    in
    List.reverse
        (List.map
            (\numSpaces ->
                Parser.succeed numSpaces
                    |. Parser.token
                        (Parser.Token (String.repeat numSpaces " ")
                            (ExpectingIndent numSpaces)
                        )
            )
            (List.range bottom top)
        )


{-| -}
indentedString : Int -> String -> Parser Context Problem (Parser.Step String String)
indentedString indent found =
    Parser.oneOf
        -- First line, indentation is already handled by the block constructor.
        [ Parser.succeed
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
        , if found == "" then
            Parser.succeed (\str -> Parser.Loop (found ++ str))
                |= Parser.getChompedString
                    (Parser.chompWhile
                        (\c -> c /= '\n')
                    )

          else
            Parser.succeed
                (\str ->
                    Parser.Loop (found ++ str)
                )
                |. Parser.token (Parser.Token (String.repeat indent " ") (ExpectingIndent indent))
                |= Parser.getChompedString
                    (Parser.chompWhile
                        (\c -> c /= '\n')
                    )
        , Parser.succeed (Parser.Done found)
        ]


{-| -}
blocksOrNewlines : Parser Context Problem thing -> Int -> ( Bool, List thing ) -> Parser Context Problem (Parser.Step ( Bool, List thing ) (List thing))
blocksOrNewlines myParser indent ( parsedSomething, existing ) =
    Parser.oneOf
        [ Parser.end End
            |> Parser.map
                (\_ ->
                    Parser.Done (List.reverse existing)
                )
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
                , Parser.succeed (Parser.Loop ( True, existing ))
                    |. Parser.backtrackable (Parser.chompWhile (\c -> c == ' '))
                    |. Parser.backtrackable newline

                -- We reach here because the indentation parsing was not successful,
                -- meaning the indentation has been lowered and the block is done
                , Parser.succeed (Parser.Done (List.reverse existing))
                ]

        -- Whitespace Line
        , Parser.succeed (Parser.Loop ( True, existing ))
            |. Parser.chompWhile (\c -> c == ' ')
            |. newline
        ]


skipBlankLineWith : thing -> Parser Context Problem thing
skipBlankLineWith x =
    Parser.succeed x
        |. Parser.token (Parser.Token "\n" Newline)
        |. Parser.oneOf
            [ Parser.succeed ()
                |. Parser.backtrackable (Parser.chompWhile (\c -> c == ' '))
                |. Parser.backtrackable (Parser.token (Parser.Token "\n" Newline))
            , Parser.succeed ()
            ]


integer : Parser Context Problem (Found Int)
integer =
    Parser.map
        (\( pos, intResult ) ->
            case intResult of
                Ok i ->
                    Found pos i

                Err str ->
                    Unexpected
                        { range = pos
                        , problem = MsgBadInt str
                        }
        )
        (withRange
            (Parser.oneOf
                [ Parser.succeed
                    (\i str ->
                        if str == "" then
                            Ok (negate i)

                        else
                            Err (String.fromInt i ++ str)
                    )
                    |. Parser.token (Parser.Token "-" (Expecting "-"))
                    |= Parser.int Integer InvalidNumber
                    |= Parser.getChompedString (Parser.chompWhile (\c -> c /= ' ' && c /= '\n'))
                , Parser.succeed
                    (\i str ->
                        if str == "" then
                            Ok i

                        else
                            Err (String.fromInt i ++ str)
                    )
                    |= Parser.int Integer InvalidNumber
                    |= Parser.getChompedString (Parser.chompWhile (\c -> c /= ' ' && c /= '\n'))
                , Parser.succeed Err
                    |= word
                ]
            )
        )


{-| Parses a float and must end with whitespace, not additional characters.
-}
floating : Parser Context Problem (Found Float)
floating =
    Parser.map
        (\( pos, floatResult ) ->
            case floatResult of
                Ok f ->
                    Found pos f

                Err str ->
                    Unexpected
                        { range = pos
                        , problem = MsgBadFloat str
                        }
        )
        (withRange
            (Parser.oneOf
                [ Parser.succeed
                    (\fl str ->
                        if str == "" then
                            Ok (negate fl)

                        else
                            Err (String.fromFloat fl ++ str)
                    )
                    |. Parser.token (Parser.Token "-" (Expecting "-"))
                    |= Parser.float FloatingPoint InvalidNumber
                    |= Parser.getChompedString (Parser.chompWhile (\c -> c /= ' ' && c /= '\n'))
                , Parser.succeed
                    (\fl str ->
                        if str == "" then
                            Ok fl

                        else
                            Err (String.fromFloat fl ++ str)
                    )
                    |= Parser.float FloatingPoint InvalidNumber
                    |= Parser.getChompedString (Parser.chompWhile (\c -> c /= ' ' && c /= '\n'))
                , Parser.succeed Err
                    |= word
                ]
            )
        )



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


word : Parser Context Problem String
word =
    Parser.chompWhile Char.isAlphaNum
        |> Parser.getChompedString


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



{- RECORD HELPERS -}


{-| -}
type Field value
    = Field String (Block value)


{-| -}
field : String -> Block value -> Field value
field name child =
    Field name child


fieldParser : Field value -> ( String, Parser Context Problem ( String, Found Description ) )
fieldParser (Field name myBlock) =
    ( name
    , withFieldName
        name
        (getParser myBlock)
    )


fieldName : Field v -> String
fieldName (Field name _) =
    name


fieldExpectation (Field name fieldBlock) =
    ( name, getBlockExpectation fieldBlock )



{- RECORD PARSER HELPERS -}


parseRecord :
    String
    -> Expectation
    -> List ( String, Parser Context Problem ( String, Found Description ) )
    -> Parser Context Problem Description
parseRecord recordName expectations fields =
    Parser.succeed
        (\( pos, foundFields ) ->
            case foundFields of
                Ok ok ->
                    Record recordName
                        { expected = expectations
                        , found =
                            Found pos ok
                        }

                Err ( maybePosition, problem ) ->
                    Record recordName
                        { expected = expectations
                        , found =
                            Unexpected
                                --unexpected
                                { range = Maybe.withDefault pos maybePosition
                                , problem = problem
                                }
                        }
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
                                    { remaining = fields
                                    , found = Ok []
                                    }
                                    (parseFields recordName (List.map Tuple.first fields))
                                )
                    )
            )


withFieldName : String -> Parser Context Problem Description -> Parser Context Problem ( String, Found Description )
withFieldName name parser =
    Parser.getIndent
        |> Parser.andThen
            (\indent ->
                Parser.map
                    (\( pos, description ) ->
                        ( name, Found pos description )
                    )
                <|
                    withRange
                        (Parser.succeed identity
                            |. Parser.keyword (Parser.Token name (ExpectingFieldName name))
                            |. Parser.chompWhile (\c -> c == ' ')
                            |. Parser.chompIf (\c -> c == '=') (Expecting "=")
                            |. Parser.chompWhile (\c -> c == ' ')
                            |= Parser.withIndent (indent + 4) (Parser.inContext (InRecordField name) parser)
                        )
            )


unexpectedField recordName options =
    Parser.getIndent
        |> Parser.andThen
            (\indent ->
                let
                    _ =
                        Debug.log "unexpected indent" indent
                in
                Parser.map
                    (\( range, ( name, content ) ) ->
                        ( name
                        , Unexpected
                            { range = range
                            , problem =
                                MsgUnexpectedField
                                    { found = name
                                    , options = options
                                    , recordName = recordName
                                    }
                            }
                        )
                    )
                    (withRange
                        (Parser.succeed Tuple.pair
                            |= Parser.getChompedString (Parser.chompWhile Char.isAlphaNum)
                            |. Parser.chompWhile (\c -> c == ' ')
                            |. Parser.chompIf (\c -> c == '=') (Expecting "=")
                            |. Parser.chompWhile (\c -> c == ' ')
                            -- TODO: parse multiline string
                            |= Parser.withIndent (indent + 4) (Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n')))
                         -- |. newline
                         -- |. Parser.map (Debug.log "unexpected capture") (Parser.loop "" (raggedIndentedStringAbove (indent - 4)))
                        )
                    )
            )


resultToFound result =
    case result of
        Ok ( range, desc ) ->
            Found range desc

        Err ( range, problem ) ->
            Unexpected
                { range = range
                , problem = problem
                }


renderRecordResult renderUnexpected pos result =
    case result of
        Ok parsedCorrectly ->
            case parsedCorrectly of
                Ok rendered ->
                    Ok (Found pos rendered)

                Err unexpected ->
                    Ok
                        (Found
                            pos
                            (renderUnexpected unexpected)
                        )

        Err problem ->
            Ok
                (Found pos
                    (renderUnexpected
                        { problem = problem
                        , range = pos
                        }
                    )
                )


type alias RecordFields =
    { remaining : List ( String, Parser Context Problem ( String, Found Description ) )
    , found : Result ( Maybe (Range Position), ProblemMessage ) (List ( String, Found Description ))
    }


type Indented thing
    = Indented thing
    | WeirdIndent Int
    | EmptyLine


{-| Either:

    1. Parses indent ++ parser ++ newline
        -> Success!
    2. Parses many spaces ++ newline
        -> Ignore completely
    3. Parses some number of spaces ++ some not newlines ++ newline
        -> Is improperly indented

-}
indentOrSkip indent successParser =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.token (Parser.Token (String.repeat indent " ") (ExpectingIndent indent))
            |= Parser.oneOf
                [ Parser.map (always EmptyLine) newline
                , Parser.succeed
                    (\foundIndent content ->
                        if content /= "" then
                            WeirdIndent (String.length foundIndent)

                        else
                            EmptyLine
                    )
                    |. Parser.chompIf (\c -> c == ' ') Space
                    |= Parser.getChompedString (Parser.chompWhile (\c -> c == ' '))
                    |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n'))
                    |. newline
                , Parser.succeed Indented
                    |= successParser
                    |. newline
                ]
        , Parser.succeed
            (\foundIndent hasContent ->
                if hasContent then
                    WeirdIndent (String.length foundIndent)

                else
                    EmptyLine
            )
            |= Parser.getChompedString (Parser.chompWhile (\c -> c == ' '))
            |= Parser.oneOf
                [ Parser.map (always False) newline
                , Parser.succeed True
                    |. Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n'))
                    |. newline
                ]
        ]


newline =
    Parser.token (Parser.Token "\n" Newline)


{-| -}
parseFields :
    String
    -> List String
    -> RecordFields
    -> Parser Context Problem (Parser.Step RecordFields (Result ( Maybe (Range Position), ProblemMessage ) (List ( String, Found Description ))))
parseFields recordName fieldNames fields =
    case fields.remaining of
        [] ->
            Parser.succeed (Parser.Done fields.found)

        _ ->
            case fields.found of
                Ok found ->
                    Parser.getIndent
                        |> Parser.andThen
                            (\indent ->
                                Parser.oneOf
                                    [ indentOrSkip indent (captureField found recordName fields fieldNames)
                                        |> Parser.map
                                            (\indentedField ->
                                                case indentedField of
                                                    Indented thing ->
                                                        thing

                                                    EmptyLine ->
                                                        Parser.Loop fields

                                                    WeirdIndent i ->
                                                        Parser.Loop
                                                            { found =
                                                                Err ( Nothing, MsgExpectingIndent indent )
                                                            , remaining =
                                                                fields.remaining
                                                            }
                                            )

                                    -- We've reached here because:
                                    -- 1. We still have expected fields, but we didn't parse them.
                                    -- 2. No other errors occurred.
                                    -- 3. We did not find the correct indentation
                                    -- 4. And This is not a blank line
                                    -- So, the only thing left is that we have some fields that we didn't parse
                                    , Parser.succeed
                                        (Parser.Done
                                            (Err
                                                ( Nothing, MsgMissingFields (List.map Tuple.first fields.remaining) )
                                            )
                                        )
                                    ]
                            )

                Err unexpected ->
                    -- We've encountered an error, but we still need to parse
                    -- the entire indented block.  so that the parser can continue.
                    Parser.getIndent
                        |> Parser.andThen
                            (\indent ->
                                Parser.succeed (Parser.Done fields.found)
                                    |. Parser.map (Debug.log "unexpected capture") (Parser.loop "" (raggedIndentedStringAbove (indent - 4)))
                            )


captureField found recordName fields fieldNames =
    Parser.map
        (\maybeField ->
            case maybeField of
                Nothing ->
                    Parser.Loop fields

                Just ( foundFieldname, fieldValue ) ->
                    case fieldValue of
                        Found _ _ ->
                            Parser.Loop
                                { found = Ok (( foundFieldname, fieldValue ) :: found)
                                , remaining =
                                    List.filter
                                        (\( fieldParserName, _ ) -> fieldParserName /= foundFieldname)
                                        fields.remaining
                                }

                        Unexpected unexpected ->
                            Parser.Loop
                                { found = Err ( Just unexpected.range, unexpected.problem )
                                , remaining =
                                    List.filter
                                        (\( fieldParserName, _ ) -> fieldParserName /= foundFieldname)
                                        fields.remaining
                                }
        )
        (Parser.oneOf
            (List.map (Parser.map Just << Tuple.second) fields.remaining
                ++ [ Parser.map Just (unexpectedField recordName fieldNames)
                   ]
            )
        )



{- RECORD RENDERER HELPERS -}


applyField : Found a -> Result UnexpectedDetails (a -> b) -> Result UnexpectedDetails b
applyField foundField possiblyFn =
    case possiblyFn of
        Err err ->
            Err err

        Ok fn ->
            case foundField of
                Found pos desc ->
                    Ok (fn desc)

                Unexpected unexpected ->
                    Err unexpected


getField : Field value -> List ( String, Found Description ) -> Result ProblemMessage (Found value)
getField (Field name fieldBlock) fields =
    List.foldl (matchField name fieldBlock) (Err (MsgMissingFields [ name ])) fields


matchField : String -> Block value -> ( String, Found Description ) -> Result ProblemMessage (Found value) -> Result ProblemMessage (Found value)
matchField targetName targetBlock ( name, foundDescription ) existing =
    case existing of
        Ok _ ->
            existing

        Err err ->
            if name == targetName then
                case foundDescription of
                    Found rng description ->
                        case renderBlock targetBlock description of
                            Ok rendered ->
                                Ok rendered

                            Err invalidAst ->
                                Err err

                    Unexpected unexpected ->
                        Ok (Unexpected unexpected)

            else
                existing



{- NESTED LIST HELPERS -}
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


emptyTreeBuilder : TreeBuilder item
emptyTreeBuilder =
    TreeBuilder
        { previousIndent = 0
        , levels = []
        }


reduceRender : (thing -> Result AstError (Found other)) -> List thing -> Result AstError (Result UnexpectedDetails (List other))
reduceRender fn list =
    List.foldl
        (\x gathered ->
            case gathered of
                Err _ ->
                    gathered

                Ok (Err unexpected) ->
                    gathered

                Ok (Ok remain) ->
                    case fn x of
                        Err err ->
                            Err err

                        Ok foundSuccess ->
                            case foundSuccess of
                                Found _ success ->
                                    Ok (Ok (success :: remain))

                                Unexpected unexpected ->
                                    Ok (Err unexpected)
        )
        (Ok (Ok []))
        list
        |> Result.map (Result.map List.reverse)


renderTreeNodeSmall :
    { item : Block item
    , start : Block icon
    }
    -> Nested ( Description, List Description )
    -> Result AstError (Found (Nested ( icon, List item )))
renderTreeNodeSmall config (Nested cursor) =
    let
        renderedChildren =
            reduceRender (renderTreeNodeSmall config) cursor.children

        renderedContent =
            case cursor.content of
                ( icon, content ) ->
                    ( renderBlock config.start icon
                    , reduceRender (renderBlock config.item) content
                    )
    in
    case renderedContent of
        ( Ok (Found pos icon), Ok (Ok content) ) ->
            case renderedChildren of
                Err err ->
                    Err err

                Ok (Ok successfullyRenderedChildren) ->
                    Ok
                        (Found pos <|
                            Nested
                                { content = ( icon, content )
                                , children = successfullyRenderedChildren
                                }
                        )

                Ok (Err unexpected) ->
                    Ok (Unexpected unexpected)

        ( Ok (Unexpected unexpected), _ ) ->
            Ok (Unexpected unexpected)

        ( _, Ok (Err unexpected) ) ->
            Ok (Unexpected unexpected)

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
        , skipBlankLineWith (Parser.Loop ( indent, existing ))
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
