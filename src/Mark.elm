module Mark exposing
    ( parse
    , Document, document
    , Block, block, stub, bool, date
    , int, float, floatBetween
    , string, multiline, exactly
    , map, andThen
    , text, Text(..), Style(..)
    , Inline, inline, inlineString, inlineText
    , Replacement, replacement, balanced
    , oneOf, manyOf, startWith
    , nested, Nested(..)
    , Field, field, record2, record3, record4, record5, record6, record7, record8, record9, record10
    , Context(..), Problem(..)
    , advanced
    )

{-| `elm-markup` is about defining what you're expecting in a markup document.

The `elm-markup` language relies heavily on indentation, which always some multiple of 4 spaces.

@docs parse


# Blocks

@docs Document, document

@docs Block, block, stub, bool, date


# Numbers

@docs int, intBetweem, float, floatBetween


# Strings

@docs string, multiline, exactly


# Mapping

@docs map, andThen


# Text

@docs text, Text, Style

@docs Inline, inline, inlineString, inlineText

@docs Replacement, replacement, balanced


# Blocks

@docs oneOf, manyOf, startWith

@docs nested, Nested


# Records

Embed a data record.

Here's an implementation to render an `<img>`

    Mark.record2 "Image"
        (\src description ->
            Html.img [ Html.Attribute.src src, Html.Attribute.alt description ] []
        )
        (Mark.field "src" Mark.string)
        (Mark.field "description" Mark.string)

Which would parse

    | Image
        src = http://placekitten/200/500
        description = A super cute kitten.

**Note** The order of the fields in the document does not matter, so this will parse correctly as well:

    | Image
        description = A super cute kitten.
        src = http://placekitten/200/500

@docs Field, field, record2, record3, record4, record5, record6, record7, record8, record9, record10


# Errors

@docs Context, Problem


# Advanced

@docs advanced

-}

import Iso8601
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)
import Time


{-| -}
parse : Document result -> String -> Result (List (Parser.DeadEnd Context Problem)) result
parse (Document blocks) source =
    Parser.run blocks source


{-| A `Block data` is just a parser that results in `data`.

You'll be building up your `Document` in terms of the `Blocks`.

-}
type
    Block data
    -- A block starts with `|` and has a name(already built into the parser)
    = Block String (Parser Context Problem data)
      -- A value is just a raw parser.
    | Value (Parser Context Problem data)


getParser fromBlock =
    case fromBlock of
        Block name p ->
            Parser.succeed identity
                |. Parser.token (Parser.Token "|" (ExpectingBlockName name))
                |. Parser.chompIf (\c -> c == ' ') Space
                |= p

        Value p ->
            p


{-| A text fragment with some styling.
-}
type Text
    = Text (List Style) String


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
type Style
    = Bold
    | Italic
    | Strike



-- | Underline
-- | Code


{-| -}
type Context
    = InBlock String
    | InInline String
    | InRecordField String


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


{-| A named block.

    Mark.block "MyBlock"
        Html.text
        Mark.string

Will parse the following and render it as `Html.text`

    | MyBlock
        Here is an unformatted string!

**Note** block names should be capitalized. In the future this may be enforced.

-}
block : String -> (child -> result) -> Block child -> Block result
block name renderer child =
    Block name
        (Parser.getIndent
            |> Parser.andThen
                (\indent ->
                    Parser.succeed renderer
                        |. Parser.keyword (Parser.Token name (ExpectingBlockName name))
                        |. Parser.chompWhile (\c -> c == ' ')
                        |. Parser.chompIf (\c -> c == '\n') Newline
                        |. Parser.oneOf
                            [ Parser.succeed ()
                                |. Parser.backtrackable (Parser.chompWhile (\c -> c == ' '))
                                |. Parser.backtrackable (Parser.chompIf (\c -> c == '\n') Newline)
                            , Parser.succeed ()
                            ]
                        |. Parser.token (Parser.Token (String.repeat (indent + 4) " ") (ExpectingIndent (indent + 4)))
                        |= Parser.withIndent (indent + 4) (Parser.inContext (InBlock name) (getParser child))
                )
        )


{-| An empty block. This could be useful for inserting something that doesn't need parameters.

    | Logo

-}
stub : String -> result -> Block result
stub name result =
    Block name
        (Parser.succeed result
            |. Parser.keyword (Parser.Token name (ExpectingBlockName name))
            |. Parser.chompWhile (\c -> c == ' ')
        )


{-| -}
type Document result
    = Document (Parser Context Problem result)


{-| Create a markup `Document`. You're first goal is to describe a document in terms of the blocks you're expecting.

Here's a brief example of a document that will parse many `MyBlock` blocks, and capture the string that's indented inside the block.

    document : Mark.Document (List String)
    document =
        Mark.document
            identity
            (Mark.manyOf
                [ Mark.block "MyBlock"
                    identity
                    Mark.string
                , Mark.string
                ]
            )

    parse : String -> Result (DeadEnd Context Problem) (List String)
    parse source =
        Mark.parse document source

Which will parse a document with many of either raw strings or`MyBlock` blocks. The `identity` function usage above means that this will result in a `List String`, but you could change that function to make this parser result in anything you want!

    | MyBlock
        Here's text captured in a MyBlock

    This text is captured at the top level.

    | MyBlock
        Here's some more text in a MyBlock.

-}
document : (child -> result) -> Block child -> Document result
document renderer child =
    Document
        (Parser.map renderer (Parser.withIndent 0 (getParser child)))


{-| Parse two blocks in sequence.

This can be useful to do things like require that a specific block comes first.

So, the classic case of a blog post that has some meta data at the start could be done like this

    Mark.startWith
        (\metadata article ->
            { meta = metadata
            , article = article
            }
        )
        (Mark.record3 "Article"
            (\author title published ->
                { author = author
                , title = title
                , published = published
                }
            )
            (Mark.field "author" Mark.string)
            (Mark.field "title" Mark.string)
            (Mark.field "published" Mark.bool)
        )
        (Mark.manyOf [ Mark.text ])

Which would parse the following doc:

    | Article
        author = Mortimer, the Stylish Elephant
        published = False
        title = How to use CSS to rearrange your living room.

    First off, thank you for using my library.

    Secondly, who knew that flex-box could wrap a sofa?

-}
startWith : (start -> rest -> result) -> Block start -> Block rest -> Block result
startWith fn start rest =
    Value
        (Parser.succeed fn
            |= getParser start
            |= getParser rest
        )


{-| Define your own parser using [`elm/parser`](https://package.elm-lang.org/packages/elm/parser/latest/).

This would be the place to start if you wanted to create a parser for syntax highlighting for an Elm block.

**Warning** This is the only place where you can break the rules/conventions of `elm-markup`.

**Note** You can get the current expected indentation using [`Parser.Advanced.getIndent`](https://package.elm-lang.org/packages/elm/parser/latest/Parser-Advanced#getIndent).

-}
advanced : Parser Context Problem result -> Block result
advanced parser =
    Value parser


{-| Expect a different block based on the result of a previous one.
-}
andThen : (a -> Block b) -> Block a -> Block b
andThen fn myBlock =
    case myBlock of
        Block name blockParser ->
            Block name
                (Parser.andThen (getParser << fn) blockParser)

        Value blockParser ->
            Value
                (Parser.andThen (getParser << fn) blockParser)


{-| Change the result of a block by applying a function to it.
-}
map : (a -> b) -> Block a -> Block b
map fn child =
    case child of
        Block name prs ->
            Block name (Parser.map fn prs)

        Value prs ->
            Value (Parser.map fn prs)


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
        (Parser.getIndent
            |> Parser.andThen
                (\baseIndent ->
                    Parser.map
                        (\items ->
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
                        )
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
    -> ( NestedIndex, List ( Int, Maybe icon, thing ) )
    -> Parser Context Problem (Parser.Step ( NestedIndex, List ( Int, Maybe icon, thing ) ) (List ( Int, Maybe icon, thing )))
indentedBlocksOrNewlines icon item ( indent, existing ) =
    Parser.oneOf
        [ case existing of
            [] ->
                Parser.end End
                    |> Parser.andThen
                        (\_ -> Parser.problem UnexpectedEnd)

            _ ->
                Parser.end End
                    |> Parser.map
                        (\_ ->
                            Parser.Done (List.reverse existing)
                        )

        -- Whitespace Line
        , Parser.succeed
            (Parser.Loop ( indent, existing ))
            |. Parser.token (Parser.Token "\n" Newline)
            |. Parser.oneOf
                [ Parser.succeed ()
                    |. Parser.backtrackable (Parser.chompWhile (\c -> c == ' '))
                    |. Parser.backtrackable (Parser.token (Parser.Token "\n" Newline))
                , Parser.succeed ()
                ]
        , case existing of
            [] ->
                -- Indent is already parsed by the block constructor for first element, skip it
                Parser.succeed
                    (\foundIcon foundBlock ->
                        let
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


{-| Many blocks that are all at the same indentation level.
-}
manyOf : List (Block a) -> Block (List a)
manyOf thing =
    Value
        (Parser.getIndent
            |> Parser.andThen
                (\indent ->
                    Parser.loop ( False, [] )
                        (blocksOrNewlines (oneOf thing) indent)
                )
        )


{-| -}
blocksOrNewlines : Block thing -> Int -> ( Bool, List thing ) -> Parser Context Problem (Parser.Step ( Bool, List thing ) (List thing))
blocksOrNewlines myBlock indent ( parsedSomething, existing ) =
    Parser.oneOf
        [ Parser.end End
            |> Parser.map
                (\_ ->
                    Parser.Done (List.reverse existing)
                )

        -- Whitespace Line
        , Parser.succeed
            (Parser.Loop ( True, existing ))
            |. Parser.token (Parser.Token "\n" Newline)
            |. Parser.oneOf
                [ Parser.succeed ()
                    |. Parser.backtrackable (Parser.chompWhile (\c -> c == ' '))
                    |. Parser.backtrackable (Parser.token (Parser.Token "\n" Newline))
                , Parser.succeed ()
                ]
        , if not parsedSomething then
            -- First thing already has indentation accounted for.
            getParser myBlock
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
                    |= getParser myBlock

                -- We reach here because the indentation parsing was not successful,
                -- meaning the indentation has been lowered and the block is done
                , if indent == 0 then
                    Parser.problem UnexpectedEnd

                  else
                    Parser.succeed (Parser.Done (List.reverse existing))
                ]
        ]


{-| Parse an ISO-8601 date string.

Format: `YYYY-MM-DDTHH:mm:ss.SSSZ`

Though you don't need to specify all segments, so `YYYY-MM-DD` works as well.

Results in a `Posix` integer, which works well with [elm/time](https://package.elm-lang.org/packages/elm/time/latest/).

-}
date : Block Time.Posix
date =
    Value
        (Parser.getChompedString
            (Parser.chompWhile
                (\c -> c /= '\n')
            )
            |> Parser.andThen
                (\str ->
                    case Iso8601.toTime str of
                        Err err ->
                            Parser.problem
                                (BadDate str)

                        Ok parsedPosix ->
                            Parser.succeed parsedPosix
                )
        )


{-| -}
oneOf : List (Block a) -> Block a
oneOf blocks =
    let
        gatherParsers myBlock ( blks, vals ) =
            case myBlock of
                Block name blkParser ->
                    ( blkParser :: blks, vals )

                Value valueParser ->
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
        (Parser.oneOf
            (blockParser :: List.reverse childValues)
        )


{-| Parse an exact string. This can be useful to parse custom types if you pair it with `Mark.oneOf`.

    type Plant = IsWatered | IsDying


    Mark.oneOf
        [ Mark.exactly "IsWatered" IsWatered
        , Mark.exactly "IsDying" IsDying
        ]

-}
exactly : String -> value -> Block value
exactly key val =
    Value
        (Parser.succeed val
            |. Parser.token (Parser.Token key (Expecting key))
        )


{-| -}
int : Block Int
int =
    Value
        (Parser.oneOf
            [ Parser.succeed (\num -> negate num)
                |. Parser.token (Parser.Token "-" (Expecting "-"))
                |= Parser.int Integer InvalidNumber
            , Parser.int Integer InvalidNumber
            ]
        )


{-| Parse an `Int` within an inclusive range.

    Mark.intBetween 0 100

Will parse any Int between 0 and 100.

-}
intBetween : Int -> Int -> Block Int
intBetween one two =
    let
        top =
            max one two

        bottom =
            min one two
    in
    Value <|
        Parser.andThen
            (\i ->
                if i >= bottom && i <= top then
                    Parser.succeed i

                else
                    Parser.problem
                        (IntOutOfRange
                            { found = i
                            , min = bottom
                            , max = top
                            }
                        )
            )
            (Parser.oneOf
                [ Parser.succeed (\num -> negate num)
                    |. Parser.token (Parser.Token "-" (Expecting "-"))
                    |= Parser.int Integer InvalidNumber
                , Parser.int Integer InvalidNumber
                ]
            )


{-| -}
float : Block Float
float =
    Value
        (Parser.oneOf
            [ Parser.succeed (\num -> negate num)
                |. Parser.token (Parser.Token "-" (Expecting "-"))
                |= Parser.float FloatingPoint InvalidNumber
            , Parser.float FloatingPoint InvalidNumber
            ]
        )


{-| Parse a `Float` within an inclusive range.

    Mark.floatBetween 0 1

Will parse any Float between 0 and 1.

-}
floatBetween : Float -> Float -> Block Float
floatBetween one two =
    let
        top =
            max one two

        bottom =
            min one two
    in
    Value <|
        Parser.andThen
            (\i ->
                if i >= bottom && i <= top then
                    Parser.succeed i

                else
                    Parser.problem
                        (FloatOutOfRange
                            { found = i
                            , min = bottom
                            , max = top
                            }
                        )
            )
            (Parser.oneOf
                [ Parser.succeed (\num -> negate num)
                    |. Parser.token (Parser.Token "-" (Expecting "-"))
                    |= Parser.float FloatingPoint InvalidNumber
                , Parser.float FloatingPoint InvalidNumber
                ]
            )


{-| Parse either `True` or `False`.
-}
bool : Block Bool
bool =
    Value
        (Parser.oneOf
            [ Parser.token (Parser.Token "True" (Expecting "True"))
                |> Parser.map (always True)
            , Parser.token (Parser.Token "False" (Expecting "False"))
                |> Parser.map (always False)
            ]
        )


{-| Parse a single line and return it as a string.
-}
string : Block String
string =
    Value
        (Parser.getChompedString
            (Parser.chompWhile
                (\c -> c /= '\n')
            )
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
        (Parser.getIndent
            |> Parser.andThen
                (\indent ->
                    Parser.loop "" (indentedString indent)
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

        -- , Parser.back
        -- , Parser.token (Parser.Token "\n" Newline)
        --     |> Parser.map (\_ -> Parser.Loop (found ++ "\n"))
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


{-| -}
type Field value
    = Field String (Block value)


{-| A record with two fields.

**Note** there's no `record1`, because that's basically just a `block`.

-}
record2 :
    String
    -> (one -> two -> data)
    -> Field one
    -> Field two
    -> Block data
record2 recordName renderer field1 field2 =
    let
        recordParser =
            Parser.succeed renderer
                |= fieldParser field1
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field2
    in
    masterRecordParser recordName [ fieldName field1, fieldName field2 ] recordParser


{-| -}
record3 :
    String
    -> (one -> two -> three -> data)
    -> Field one
    -> Field two
    -> Field three
    -> Block data
record3 recordName renderer field1 field2 field3 =
    let
        recordParser =
            Parser.succeed renderer
                |= fieldParser field1
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field2
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field3
    in
    masterRecordParser recordName [ fieldName field1, fieldName field2, fieldName field3 ] recordParser


{-| -}
record4 :
    String
    -> (one -> two -> three -> four -> data)
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Block data
record4 recordName renderer field1 field2 field3 field4 =
    let
        recordParser =
            Parser.succeed renderer
                |= fieldParser field1
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field2
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field3
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field4
    in
    masterRecordParser recordName [ fieldName field1, fieldName field2, fieldName field3, fieldName field4 ] recordParser


{-| -}
record5 :
    String
    -> (one -> two -> three -> four -> five -> data)
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Field five
    -> Block data
record5 recordName renderer field1 field2 field3 field4 field5 =
    let
        recordParser =
            Parser.succeed renderer
                |= fieldParser field1
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field2
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field3
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field4
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field5
    in
    masterRecordParser recordName
        [ fieldName field1
        , fieldName field2
        , fieldName field3
        , fieldName field4
        , fieldName field5
        ]
        recordParser


{-| -}
record6 :
    String
    ->
        (one
         -> two
         -> three
         -> four
         -> five
         -> six
         -> data
        )
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Field five
    -> Field six
    -> Block data
record6 recordName renderer field1 field2 field3 field4 field5 field6 =
    let
        recordParser =
            Parser.succeed renderer
                |= fieldParser field1
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field2
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field3
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field4
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field5
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field6
    in
    masterRecordParser recordName
        [ fieldName field1
        , fieldName field2
        , fieldName field3
        , fieldName field4
        , fieldName field5
        , fieldName field6
        ]
        recordParser


{-| -}
record7 :
    String
    ->
        (one
         -> two
         -> three
         -> four
         -> five
         -> six
         -> seven
         -> data
        )
    -> Field one
    -> Field two
    -> Field three
    -> Field four
    -> Field five
    -> Field six
    -> Field seven
    -> Block data
record7 recordName renderer field1 field2 field3 field4 field5 field6 field7 =
    let
        recordParser =
            Parser.succeed renderer
                |= fieldParser field1
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field2
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field3
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field4
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field5
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field6
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field7
    in
    masterRecordParser recordName
        [ fieldName field1
        , fieldName field2
        , fieldName field3
        , fieldName field4
        , fieldName field5
        , fieldName field6
        , fieldName field7
        ]
        recordParser


{-| -}
record8 :
    String
    ->
        (one
         -> two
         -> three
         -> four
         -> five
         -> six
         -> seven
         -> eight
         -> data
        )
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
    let
        recordParser =
            Parser.succeed renderer
                |= fieldParser field1
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field2
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field3
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field4
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field5
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field6
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field7
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field8
    in
    masterRecordParser recordName
        [ fieldName field1
        , fieldName field2
        , fieldName field3
        , fieldName field4
        , fieldName field5
        , fieldName field6
        , fieldName field7
        , fieldName field8
        ]
        recordParser


{-| -}
record9 :
    String
    ->
        (one
         -> two
         -> three
         -> four
         -> five
         -> six
         -> seven
         -> eight
         -> nine
         -> data
        )
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
    let
        recordParser =
            Parser.succeed renderer
                |= fieldParser field1
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field2
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field3
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field4
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field5
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field6
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field7
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field8
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field9
    in
    masterRecordParser recordName
        [ fieldName field1
        , fieldName field2
        , fieldName field3
        , fieldName field4
        , fieldName field5
        , fieldName field6
        , fieldName field7
        , fieldName field8
        , fieldName field9
        ]
        recordParser


{-| -}
record10 :
    String
    ->
        (one
         -> two
         -> three
         -> four
         -> five
         -> six
         -> seven
         -> eight
         -> nine
         -> ten
         -> data
        )
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
    let
        recordParser =
            Parser.succeed renderer
                |= fieldParser field1
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field2
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field3
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field4
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field5
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field6
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field7
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field8
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field9
                |. Parser.chompIf (\c -> c == '\n') Newline
                |= fieldParser field10
    in
    masterRecordParser recordName
        [ fieldName field1
        , fieldName field2
        , fieldName field3
        , fieldName field4
        , fieldName field5
        , fieldName field6
        , fieldName field7
        , fieldName field8
        , fieldName field9
        , fieldName field10
        ]
        recordParser


{-| -}
masterRecordParser : String -> List String -> Parser Context Problem data -> Block data
masterRecordParser recordName names recordParser =
    Block recordName
        (Parser.getIndent
            |> Parser.andThen
                (\indent ->
                    (Parser.succeed identity
                        |. Parser.keyword (Parser.Token recordName (ExpectingBlockName recordName))
                        |. Parser.chompWhile (\c -> c == ' ')
                        |. Parser.chompIf (\c -> c == '\n') Newline
                        |= Parser.withIndent (indent + 4)
                            (Parser.inContext (InBlock recordName)
                                (Parser.loop [] (indentedFieldNames recordName (indent + 4) names))
                            )
                    )
                        |> Parser.andThen
                            (\fieldList ->
                                let
                                    join ( key, val ) str =
                                        case str of
                                            "" ->
                                                key ++ " = " ++ val

                                            _ ->
                                                str ++ "\n" ++ key ++ " = " ++ val

                                    recomposed =
                                        fieldList
                                            |> reorderFields names
                                            |> Result.map (List.foldl join "")
                                in
                                case recomposed of
                                    Ok str ->
                                        case Parser.run (Parser.withIndent 0 (Parser.inContext (InBlock recordName) recordParser)) str of
                                            Ok ok ->
                                                Parser.succeed ok

                                            Err err ->
                                                case err of
                                                    [] ->
                                                        -- NOTE: this shouldn't happen
                                                        Parser.problem RecordError

                                                    fst :: _ ->
                                                        withContextStack fst.contextStack (Parser.problem fst.problem)

                                    Err recordError ->
                                        Parser.problem recordError
                            )
                )
        )


withContextStack stack parser =
    case stack of
        [] ->
            parser

        lvl :: remaining ->
            Parser.inContext lvl.context (withContextStack remaining parser)


reorderFields : List String -> List ( String, String ) -> Result Problem (List ( String, String ))
reorderFields desiredOrder found =
    if List.length desiredOrder /= List.length found then
        Err
            (NonMatchingFields
                { expecting = desiredOrder
                , found = List.map Tuple.first found
                }
            )

    else
        List.foldl (gatherFields found) (Ok []) desiredOrder
            |> Result.map List.reverse


gatherFields : List ( String, String ) -> String -> Result Problem (List ( String, String )) -> Result Problem (List ( String, String ))
gatherFields cache desired found =
    case found of
        Ok ok ->
            case getField cache desired of
                Ok newField ->
                    Ok (newField :: ok)

                Err str ->
                    Err str

        _ ->
            found


getField cache desired =
    case cache of
        [] ->
            Err (MissingField desired)

        ( name, top ) :: rest ->
            if desired == name then
                Ok ( name, top )

            else
                getField rest desired


fieldParser (Field _ myBlock) =
    getParser myBlock


fieldName (Field name _) =
    name


{-| -}
field : String -> Block value -> Field value
field name child =
    case child of
        Block blockName childParser ->
            Field name (Block blockName (withFieldName name childParser))

        Value childParser ->
            Field name (Value (withFieldName name childParser))


withFieldName name parser =
    Parser.getIndent
        |> Parser.andThen
            (\indent ->
                Parser.succeed identity
                    |. Parser.keyword (Parser.Token name (ExpectingFieldName name))
                    |. Parser.chompIf (\c -> c == ' ') Space
                    |. Parser.chompIf (\c -> c == '=') (Expecting "=")
                    |. Parser.chompIf (\c -> c == ' ') Space
                    |= Parser.withIndent (indent + 4) (Parser.inContext (InRecordField name) parser)
            )


indentedFieldNames : String -> Int -> List String -> List ( String, String ) -> Parser Context Problem (Parser.Step (List ( String, String )) (List ( String, String )))
indentedFieldNames recordName indent fields found =
    let
        fieldNameParser name =
            Parser.succeed
                (\contentStr ->
                    Parser.Loop (( name, contentStr ) :: found)
                )
                |. Parser.token (Parser.Token name (Expecting name))
                |. Parser.chompWhile (\c -> c == ' ')
                |. Parser.chompIf (\c -> c == '=') (Expecting "=")
                |. Parser.chompWhile (\c -> c == ' ')
                |= Parser.getChompedString
                    (Parser.chompWhile
                        (\c -> c /= '\n')
                    )

        unexpectedField =
            Parser.getChompedString
                (Parser.chompWhile (\c -> c /= '=' && c /= '\n'))
                |> Parser.andThen
                    (\unexpected ->
                        let
                            trimmed =
                                String.trim unexpected
                        in
                        Parser.problem
                            (UnexpectedField
                                { found = trimmed
                                , options = fields
                                , recordName = recordName
                                }
                            )
                    )

        content =
            Parser.succeed
                (\str ->
                    case found of
                        [] ->
                            Parser.Loop found

                        ( name, contentStr ) :: remain ->
                            Parser.Loop (( name, contentStr ++ "\n " ++ str ) :: remain)
                )
                |. Parser.chompIf (\c -> c == ' ') (ExpectingIndent (indent + 4))
                |= Parser.getChompedString
                    (Parser.chompWhile
                        (\c -> c /= '\n')
                    )
    in
    Parser.oneOf
        ([ Parser.succeed
            identity
            |. Parser.token (Parser.Token (String.repeat indent " ") (ExpectingIndent indent))
            |= Parser.oneOf
                (case found of
                    [] ->
                        List.map fieldNameParser fields ++ [ unexpectedField ]

                    _ ->
                        content
                            :: List.map fieldNameParser fields
                            ++ [ unexpectedField ]
                )
         , Parser.succeed
            (Parser.Loop found)
            |. Parser.token (Parser.Token "\n" Newline)
            |. Parser.oneOf
                [ Parser.succeed ()
                    |. Parser.backtrackable (Parser.chompWhile (\c -> c == ' '))
                    |. Parser.backtrackable (Parser.token (Parser.Token "\n" Newline))
                , Parser.succeed ()
                ]
         ]
            ++ (if List.length found /= List.length fields then
                    [ Parser.succeed (Parser.Done found)
                        |. Parser.chompIf (\c -> c == ' ') (ExpectingIndent indent)
                        |. Parser.problem (ExpectingIndent indent)
                    , Parser.succeed (Parser.Done found)
                    ]

                else
                    [ Parser.succeed (Parser.Done found) ]
               )
        )


basicTextOptions =
    { view = identity
    , inlines = []
    , replacements = [ replacement "//" "/" ]
    }


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
    { view : Text -> rendered
    , inlines : List (Inline rendered)
    , replacements : List Replacement
    }
    -> Block (List rendered)
text options =
    Value (styledText options [] [])


{-| -}
type Inline result
    = Inline String (List Style -> Parser Context Problem result)


{-| Create a custom inline element.

For example, here is how you could parse a link:

    Mark.inline "Link"
        (\txt url ->
            Html.a [ Html.Attriutes.href url ] (List.map renderText txt)
        )
        |> Mark.inlineText
        |> Mark.inlineString "url"

Here's an example of a sentence with the above link:

`Here is my sentence {link| with a link to a web-comic. |url=http://www.poorlydrawnlines.com/comic/website-bird/}  You're welcome.`

-}
inline : String -> result -> Inline result
inline name renderer =
    Inline name
        (\styles ->
            Parser.succeed renderer
                |. Parser.keyword (Parser.Token name (ExpectingInlineName name))
        )


{-| Parse an inline String field
-}
inlineString : String -> Inline (String -> result) -> Inline result
inlineString name (Inline inlineName inlineParser) =
    Inline inlineName
        (\styles ->
            Parser.succeed
                (<|)
                |= inlineParser styles
                |. Parser.chompIf (\c -> c == '|') (Expecting "|")
                |. Parser.chompWhile (\c -> c == ' ')
                |. Parser.keyword (Parser.Token name (ExpectingFieldName name))
                |. Parser.chompWhile (\c -> c == ' ')
                |. Parser.chompIf (\c -> c == '=') (Expecting "=")
                |. Parser.chompWhile (\c -> c == ' ')
                |= Parser.getChompedString
                    (Parser.chompWhile (\c -> c /= '|' && c /= '}'))
        )


{-| -}
inlineText : Inline (List Text -> result) -> Inline result
inlineText (Inline inlineName inlineParser) =
    Inline inlineName
        (\styles ->
            Parser.succeed
                (<|)
                |= inlineParser styles
                |. Parser.chompIf (\c -> c == '|') (Expecting "|")
                |= styledText basicTextOptions styles [ '}', '|' ]
        )


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



{- Text Parsing -}


{-| -}
styledText :
    { view : Text -> rendered
    , inlines : List (Inline rendered)
    , replacements : List Replacement
    }
    -> List Style
    -> List Char
    -> Parser Context Problem (List rendered)
styledText options inheritedStyles until =
    let
        vacantText =
            TextAccumulator { text = Text inheritedStyles "", rendered = [], balancedReplacements = [] }

        untilStrings =
            List.map String.fromChar until

        meaningful =
            '\\' :: '\n' :: until ++ stylingChars ++ replacementStartingChars options.replacements
    in
    Parser.oneOf
        [ Parser.chompIf
            (\c -> c == ' ')
            CantStartTextWithSpace
            |> Parser.andThen
                (\_ ->
                    Parser.problem CantStartTextWithSpace
                )
        , Parser.loop vacantText
            (styledTextLoop options meaningful untilStrings)
        ]


{-| -}
styledTextLoop :
    { view : Text -> rendered
    , inlines : List (Inline rendered)
    , replacements : List Replacement
    }
    -> List Char
    -> List String
    -> TextAccumulator rendered
    -> Parser Context Problem (Parser.Step (TextAccumulator rendered) (List rendered))
styledTextLoop options meaningful untilStrings found =
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

                -- , Parser.map (always (Just Underline)) (Parser.token (Parser.Token "_" (Expecting "_")))
                , Parser.map (always (Just Strike)) (Parser.token (Parser.Token "~" (Expecting "~")))
                , Parser.map (always (Just Bold)) (Parser.token (Parser.Token "*" (Expecting "*")))

                -- , Parser.map (always (Just Code)) (Parser.token (Parser.Token "`" (Expecting "`")))
                ]

        -- Custom inline block
        , Parser.succeed
            (\rendered ->
                let
                    current =
                        case changeStyle options found Nothing of
                            TextAccumulator accum ->
                                accum
                in
                Parser.Loop
                    (TextAccumulator
                        { rendered = rendered :: current.rendered

                        -- TODO: This should inherit formatting from the inline parser
                        , text = empty
                        , balancedReplacements = current.balancedReplacements
                        }
                    )
            )
            |. Parser.token
                (Parser.Token "{" InlineStart)
            |= Parser.oneOf
                (List.map
                    (\(Inline inlineName inlineParser) ->
                        Parser.inContext
                            (InInline inlineName)
                            (inlineParser (currentStyles found))
                    )
                    options.inlines
                )
            |. Parser.token (Parser.Token "}" InlineEnd)
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
                            TextAccumulator txt ->
                                let
                                    styling =
                                        case txt.text of
                                            Text s _ ->
                                                s
                                in
                                if List.isEmpty styling then
                                    Parser.succeed (Parser.Done (List.reverse txt.rendered))

                                else
                                    Parser.problem (UnclosedStyles styling)

                    else
                        Parser.succeed (Parser.Loop (addText new found))
                )
        ]


currentStyles (TextAccumulator formatted) =
    case formatted.text of
        Text s _ ->
            s


{-| -}
almostReplacement : List Replacement -> TextAccumulator rendered -> List (Parser Context Problem (TextAccumulator rendered))
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
replace : List Replacement -> TextAccumulator rendered -> List (Parser Context Problem (TextAccumulator rendered))
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
                        -- |. Parser.loop ()
                        --     (\_ ->
                        --         Parser.oneOf
                        --             [ Parser.token (Parser.Token x (Expecting x))
                        --                 |> Parser.map (always (Parser.Loop ()))
                        --             , Parser.succeed (Parser.Done ())
                        --             ]
                        --     )
                        |= Parser.succeed ()

                Balanced range ->
                    let
                        balanceCache =
                            case existing of
                                TextAccumulator cursor ->
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


addBalance id (TextAccumulator cursor) =
    TextAccumulator <|
        { cursor | balancedReplacements = id :: cursor.balancedReplacements }


removeBalance id (TextAccumulator cursor) =
    TextAccumulator <|
        { cursor | balancedReplacements = List.filter ((/=) id) cursor.balancedReplacements }


addText newTxt (TextAccumulator cursor) =
    case cursor.text of
        Text styles txt ->
            TextAccumulator { cursor | text = Text styles (txt ++ newTxt) }


changeStyle options (TextAccumulator cursor) maybeStyleToken =
    let
        textIsEmpty =
            case cursor.text of
                Text _ "" ->
                    True

                _ ->
                    False

        newText =
            case maybeStyleToken of
                Nothing ->
                    cursor.text

                Just sty ->
                    case sty of
                        Bold ->
                            flipStyle Bold cursor.text

                        Italic ->
                            flipStyle Italic cursor.text

                        Strike ->
                            flipStyle Strike cursor.text

        -- Underline ->
        --     flipStyle Underline cursor.text
        -- Code ->
        --     flipStyle Code cursor.text
    in
    if textIsEmpty then
        TextAccumulator { rendered = cursor.rendered, text = newText, balancedReplacements = cursor.balancedReplacements }

    else
        TextAccumulator
            { rendered =
                options.view cursor.text
                    :: cursor.rendered
            , text = newText
            , balancedReplacements = cursor.balancedReplacements
            }


flipStyle newStyle textStyle =
    case textStyle of
        Text styles str ->
            if List.member newStyle styles then
                Text (List.filter ((/=) newStyle) styles) ""

            else
                Text (newStyle :: styles) ""


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



{- Fault tolerant, Incremental Parsing Sketch

   A Document is essentially a tree with this structure


   type Doc result =
       Node result (List Doc)


   Except each node isn't necessarily a `result`, it can be anything, so we have to do things a little differently.


       So, we can have the mapping function decide what to do.

       Currently, we define a block as

           block : String -> (child -> result) -> Block child -> Block result

       What are the situations that we'd like to capture

           1. [x] - Block is parsed successfully
           2. [ ] - Block fails to parse for some reason
               -> [x] - Deliver an error at the top level
               -> [ ] -


           block : String -> (Result Error child -> result) -> Block child -> Block result





-}
-- original: Document result -> String -> Result (List (Parser.DeadEnd Context Problem)) result
-- type Parsed result =
--      Success result
--     | Errors
--         { result :
--         }
-- parse : Document result -> String -> Result (List (Parser.DeadEnd Context Problem)) result
