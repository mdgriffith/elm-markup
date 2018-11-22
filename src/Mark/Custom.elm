module Mark.Custom exposing
    ( parse
    , Block, block, many, oneOf, map
    , bool, int, float
    , text, textWith, inline, Replacement, replacement, balanced
    , advanced
    , Problem(..), Context(..)
    , Text, TextFormatting(..), InlineStyle(..)
    )

{-|

@docs parse

@docs Block, block, many, oneOf, map

@docs bool, int, float

@docs text, textWith, inline, Replacement, replacement, balanced

@docs advanced

@docs Problem, Context

@docs Text, TextFormatting, InlineStyle

-}

import Parser.Advanced as Parser exposing ((|.), (|=), Parser)



{-

   block "Header"
       (\styled ->
           Element.paragraph [ Region.heading 1 ] styled
       )
       text

   block "Image"
       (\src description ->
           Element.image ...
       )
       |> field "src" file
       |> field "description" string


   advanced "List"
       (listParser)

    block "Section"
        renderer
        (many blocks)



-}


parse : Block result -> String -> Result (List (Parser.DeadEnd Context Problem)) result
parse (Block blocks) source =
    Parser.run blocks source


type Block result
    = Block (Parser Context Problem result)


type alias Text =
    { style : TextFormatting
    , link : Maybe String
    }


type TextFormatting
    = NoFormatting String
    | Styles (List InlineStyle) String


{-| -}
type TextAccumulator rendered
    = TextAccumulator
        -- Accumulator string
        { text : TextFormatting

        -- Accumulator of element constructors
        , rendered : List rendered
        , balancedReplacements : List String
        }


type Replacement
    = Replacement String String
    | Balanced
        { start : ( String, String )
        , end : ( String, String )
        }


type InlineStyle
    = NoStyleChange
    | Bold
    | Italic
    | Strike
    | Underline
    | Token


type Context
    = InBlock String
    | InInline String


type Problem
    = NoBlocks
    | ExpectedIndent
    | InlineStart
    | InlineBar
    | InlineEnd
    | Expecting String
    | ExpectingBlockName String
    | ExpectingInlineName String
    | ExpectingFieldName String
    | Escape
    | EscapedChar
    | Dash
    | DoubleQuote
    | Apostrophe
    | Newline
    | Space
    | End
    | Integer
    | FloatingPoint
    | InvalidNumber
    | ExpectingAlphaNumeric


{-| -}
block : String -> (child -> result) -> Block child -> Block result
block name renderer (Block childParser) =
    Block
        (Parser.getIndent
            |> Parser.andThen
                (\indent ->
                    Parser.succeed renderer
                        |. Parser.keyword (Parser.Token name (ExpectingBlockName name))
                        |. Parser.chompWhile (\c -> c == ' ')
                        |. Parser.chompIf (\c -> c == '\n') Newline
                        |= Parser.withIndent (indent + 4) (Parser.inContext (InBlock name) childParser)
                )
        )


{-| -}
advanced : String -> Parser Context Problem result -> Block result
advanced name parser =
    Block
        (Parser.getIndent
            |> Parser.andThen
                (\indent ->
                    Parser.succeed identity
                        |. Parser.keyword (Parser.Token name (ExpectingBlockName name))
                        |. Parser.chompWhile (\c -> c == ' ')
                        |. Parser.chompIf (\c -> c == '\n') Newline
                        |= Parser.withIndent (indent + 4) (Parser.inContext (InBlock name) parser)
                )
        )


{-| -}
map : (a -> b) -> Block a -> Block b
map fn (Block parser) =
    Block (Parser.map fn parser)


{-| -}
many : Block a -> Block (List a)
many thing =
    Block <|
        Parser.loop []
            (blocksOrNewlines thing)


{-| -}
blocksOrNewlines : Block thing -> List thing -> Parser Context Problem (Parser.Step (List thing) (List thing))
blocksOrNewlines (Block myBlock) existing =
    Parser.oneOf
        [ Parser.end End
            |> Parser.map
                (\_ ->
                    Parser.Done existing
                )
        , Parser.token (Parser.Token "\n" Newline)
            |> Parser.map
                (\_ ->
                    Parser.Loop
                        existing
                )
        , myBlock
            |> Parser.map
                (\foundBlock ->
                    Parser.Loop (foundBlock :: existing)
                )
        ]


{-| -}
oneOf : List (Block a) -> Block a
oneOf blocks =
    Block (Parser.oneOf (List.map (\(Block parser) -> parser) blocks))


{-| -}
int : Block Int
int =
    Block
        (Parser.int Integer InvalidNumber)


{-| -}
float : Block Float
float =
    Block
        (Parser.float FloatingPoint InvalidNumber)


{-| -}
bool : Block Bool
bool =
    Block
        (Parser.oneOf
            [ Parser.token (Parser.Token "True" (Expecting "True"))
                |> Parser.map (always True)
            , Parser.token (Parser.Token "False" (Expecting "False"))
                |> Parser.map (always False)
            ]
        )


{-| -}
text : Block (List Text)
text =
    textWith
        basicTextOptions


basicTextOptions =
    { view = identity
    , inlines = []
    , merge = identity
    , replacements = []
    }


{-| -}
type Inline result
    = Inline (List InlineStyle -> Parser Context Problem result)


{-| -}
inline : String -> (List Text -> result) -> Inline result
inline name renderer =
    Inline
        (\styles ->
            Parser.succeed renderer
                |. Parser.keyword (Parser.Token name (ExpectingInlineName name))
                |. Parser.token (Parser.Token "|" InlineBar)
                |= styledText basicTextOptions styles [ '}' ]
                |. Parser.token (Parser.Token "}" InlineEnd)
        )


{-| -}
textWith :
    { view : Text -> rendered
    , inlines : List (Inline rendered)
    , merge : List rendered -> result
    , replacements : List Replacement
    }
    -> Block result
textWith options =
    Block (styledText options [] [])


{-| -}
replacement : String -> String -> Replacement
replacement =
    Replacement


{-| -}
balanced :
    { end : ( String, String )
    , start : ( String, String )
    }
    -> Replacement
balanced =
    Balanced


{-| -}
emptyText : TextAccumulator rendered
emptyText =
    TextAccumulator
        { text = NoFormatting ""
        , rendered = []
        , balancedReplacements = []
        }



{- Text Parsing -}


{-| -}
styledText :
    { view : Text -> rendered
    , inlines : List (Inline rendered)
    , merge : List rendered -> result
    , replacements : List Replacement
    }
    -> List InlineStyle
    -> List Char
    -> Parser Context Problem result
styledText options inheritedStyles until =
    let
        vacantText =
            case inheritedStyles of
                [] ->
                    TextAccumulator { text = NoFormatting "", rendered = [], balancedReplacements = [] }

                x ->
                    TextAccumulator { text = Styles inheritedStyles "", rendered = [], balancedReplacements = [] }

        untilStrings =
            List.map String.fromChar until

        meaningful =
            '\n' :: until ++ stylingChars ++ replacementStartingChars options.replacements
    in
    Parser.loop vacantText
        (styledTextLoop options meaningful untilStrings)


{-| -}
styledTextLoop :
    { view : Text -> rendered
    , inlines : List (Inline rendered)
    , merge : List rendered -> result
    , replacements : List Replacement
    }
    -> List Char
    -> List String
    -> TextAccumulator rendered
    -> Parser Context Problem (Parser.Step (TextAccumulator rendered) result)
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
                [ Parser.map (always Italic) (Parser.token (Parser.Token "/" (Expecting "/")))
                , Parser.map (always Strike) (Parser.token (Parser.Token "~" (Expecting "~")))
                , Parser.map (always Bold) (Parser.token (Parser.Token "*" (Expecting "*")))
                , Parser.map (always Token) (Parser.token (Parser.Token "`" (Expecting "`")))
                ]

        -- Custom inline block
        , Parser.succeed
            (\rendered ->
                let
                    current =
                        case changeStyle options found NoStyleChange of
                            TextAccumulator accum ->
                                accum
                in
                Parser.Loop
                    (TextAccumulator
                        { rendered = rendered :: current.rendered

                        -- TODO: This should inherit formatting from the inline parser
                        , text = NoFormatting ""
                        , balancedReplacements = current.balancedReplacements
                        }
                    )
            )
            |. Parser.token
                (Parser.Token "{" InlineStart)
            |= Parser.oneOf
                (List.map (\(Inline inlineParser) -> inlineParser (currentStyles found)) options.inlines)

        -- Link
        , Parser.succeed
            (\textList url ->
                case changeStyle options found NoStyleChange of
                    TextAccumulator current ->
                        Parser.Loop <|
                            TextAccumulator
                                { rendered =
                                    List.map
                                        (\textNode ->
                                            options.view
                                                { link = Just url
                                                , style = textNode.style
                                                }
                                        )
                                        textList
                                        ++ current.rendered
                                , text =
                                    case List.map .style (List.reverse textList) of
                                        [] ->
                                            NoFormatting ""

                                        (NoFormatting _) :: _ ->
                                            NoFormatting ""

                                        (Styles styles _) :: _ ->
                                            Styles styles ""
                                , balancedReplacements = current.balancedReplacements
                                }
            )
            |. Parser.token (Parser.Token "[" (Expecting "["))
            |= styledText basicTextOptions (currentStyles found) [ ']' ]
            |. Parser.token (Parser.Token "]" (Expecting "]"))
            |. Parser.token (Parser.Token "(" (Expecting "("))
            |= Parser.getChompedString
                (Parser.chompWhile (\c -> c /= ')' && c /= '\n' && c /= ' '))
            |. Parser.token (Parser.Token ")" (Expecting ")"))

        -- chomp until a meaningful character
        , Parser.chompWhile
            (\c ->
                not (List.member c meaningful)
            )
            |> Parser.getChompedString
            |> Parser.map
                (\new ->
                    if new == "" || new == "\n" then
                        Parser.Done (finishText options found)

                    else if List.member (String.right 1 new) untilStrings then
                        Parser.Done (finishText options (addText (String.dropRight 1 new) found))

                    else
                        Parser.Loop (addText new found)
                )
        ]


currentStyles (TextAccumulator formatted) =
    case formatted.text of
        NoFormatting _ ->
            []

        Styles s _ ->
            s


finishText :
    { view : Text -> rendered
    , inlines : List (Inline rendered)
    , merge : List rendered -> result
    , replacements : List Replacement
    }
    -> TextAccumulator rendered
    -> result
finishText opts accum =
    case changeStyle opts accum NoStyleChange of
        TextAccumulator txt ->
            opts.merge txt.rendered


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
                    (Parser.chompIf (\c -> c == char && char /= '{') EscapedChar)

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
                        (addText y existing)
                        |. Parser.token (Parser.Token x (Expecting x))
                        |. Parser.loop ()
                            (\_ ->
                                Parser.oneOf
                                    [ Parser.token (Parser.Token x (Expecting x))
                                        |> Parser.map (always (Parser.Loop ()))
                                    , Parser.succeed (Parser.Done ())
                                    ]
                            )

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
    , '/'
    , '*'
    , '['
    , '\n'
    , '<'
    , '`'
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
        NoFormatting txt ->
            TextAccumulator { cursor | text = NoFormatting (txt ++ newTxt) }

        Styles styles txt ->
            TextAccumulator { cursor | text = Styles styles (txt ++ newTxt) }


{-| If we accumulating a link style, accumulate it there.
-}
addElement newElements (TextAccumulator cursor) =
    TextAccumulator { cursor | rendered = newElements :: cursor.rendered }


changeStyle options (TextAccumulator cursor) styleToken =
    let
        textIsEmpty =
            case cursor.text of
                NoFormatting "" ->
                    True

                Styles _ "" ->
                    True

                _ ->
                    False

        newText =
            case styleToken of
                NoStyleChange ->
                    cursor.text

                Bold ->
                    flipStyle Bold cursor.text

                Italic ->
                    flipStyle Italic cursor.text

                Strike ->
                    flipStyle Strike cursor.text

                Underline ->
                    flipStyle Underline cursor.text

                Token ->
                    flipStyle Token cursor.text
    in
    if textIsEmpty then
        TextAccumulator { rendered = cursor.rendered, text = newText, balancedReplacements = cursor.balancedReplacements }

    else
        TextAccumulator
            { rendered =
                options.view
                    { style = cursor.text
                    , link = Nothing
                    }
                    :: cursor.rendered
            , text = newText
            , balancedReplacements = cursor.balancedReplacements
            }


flipStyle newStyle textStyle =
    case textStyle of
        NoFormatting str ->
            Styles [ newStyle ] ""

        Styles styles str ->
            if List.member newStyle styles then
                Styles (List.filter ((/=) newStyle) styles) ""

            else
                Styles (newStyle :: styles) ""
