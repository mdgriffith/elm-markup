module Mark.Internal.Parser exposing
    ( RecordType(..)
    , Replacement(..)
    , backtrackCharacters
    , buildTree
    , float
    , getFailableBlock
    , getPosition
    , getRangeAndSource
    , indentedString
    , int
    , manyOf
    , newline
    , newlineWith
    , oneOf
    , parseInlineFields
    , raggedIndentedStringAbove
    , record
    , skipBlankLineWith
    , styledText
    , tree
    , withIndent
    , withRange
    , withRangeResult
    , word
    )

{-| -}

import Mark.Internal.Description exposing (..)
import Mark.Internal.Error as Error exposing (Context(..), Problem(..))
import Mark.Internal.Id as Id exposing (..)
import Mark.Internal.TolerantParser as Tolerant
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


newlineWith x =
    Parser.token (Parser.Token "\n" (Expecting x))


newline =
    Parser.token (Parser.Token "\n" Newline)


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


int : Parser Context Problem (Found Int)
int =
    Parser.map
        (\result ->
            case result of
                Ok details ->
                    Found details.range details.value

                Err details ->
                    Unexpected
                        { range = details.range
                        , problem = Error.BadInt
                        }
        )
        (withRangeResult
            integer
        )


integer =
    Parser.oneOf
        [ Parser.succeed
            (\i str ->
                if str == "" then
                    Ok (negate i)

                else
                    Err InvalidNumber
            )
            |. Parser.token (Parser.Token "-" (Expecting "-"))
            |= Parser.int Integer InvalidNumber
            |= Parser.getChompedString (Parser.chompWhile (\c -> c /= ' ' && c /= '\n'))
        , Parser.succeed
            (\i str ->
                if str == "" then
                    Ok i

                else
                    Err InvalidNumber
            )
            |= Parser.int Integer InvalidNumber
            |= Parser.getChompedString (Parser.chompWhile (\c -> c /= ' ' && c /= '\n'))
        , Parser.succeed (Err InvalidNumber)
            |. word
        ]


{-| Parses a float and must end with whitespace, not additional characters.
-}
float : Parser Context Problem (Found ( String, Float ))
float =
    Parser.map
        (\result ->
            case result of
                Ok details ->
                    Found details.range details.value

                Err details ->
                    Unexpected
                        { range = details.range
                        , problem = Error.BadFloat
                        }
        )
        (withRangeResult
            floating
        )


floating =
    Parser.oneOf
        [ Parser.succeed
            (\start fl end src extra ->
                if extra == "" then
                    Ok ( String.slice start end src, negate fl )

                else
                    Err InvalidNumber
            )
            |= Parser.getOffset
            |. Parser.token (Parser.Token "-" (Expecting "-"))
            |= Parser.float FloatingPoint InvalidNumber
            |= Parser.getOffset
            |= Parser.getSource
            |= Parser.getChompedString (Parser.chompWhile (\c -> c /= ' ' && c /= '\n'))
        , Parser.succeed
            (\start fl end src extra ->
                if extra == "" then
                    Ok ( String.slice start end src, fl )

                else
                    Err InvalidNumber
            )
            |= Parser.getOffset
            |= Parser.float FloatingPoint InvalidNumber
            |= Parser.getOffset
            |= Parser.getSource
            |= Parser.getChompedString (Parser.chompWhile (\c -> c /= ' ' && c /= '\n'))
        , Parser.succeed (Err InvalidNumber)
            |. word
        ]


{-| -}
indentedString : Int -> String -> Parser Context Problem (Parser.Step String String)
indentedString indentation found =
    Parser.oneOf
        -- First line, indentation is already handled by the block constructor.
        [ Parser.succeed (Parser.Done found)
            |. Parser.end End
        , Parser.succeed
            (\extra ->
                Parser.Loop <|
                    if extra then
                        found ++ "\n\n"

                    else
                        found ++ "\n"
            )
            |. newline
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
                |. Parser.token (Parser.Token (String.repeat indentation " ") (ExpectingIndentation indentation))
                |= Parser.getChompedString
                    (Parser.chompWhile
                        (\c -> c /= '\n')
                    )
        , Parser.succeed (Parser.Done found)
        ]


{-| -}
raggedIndentedStringAbove : Int -> String -> Parser Context Problem (Parser.Step String String)
raggedIndentedStringAbove indentation found =
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
                if indentCount <= 0 then
                    Parser.Done found

                else
                    Parser.Loop (found ++ String.repeat indentCount " " ++ str)
            )
            |= Parser.oneOf (indentationBetween (indentation + 1) (indentation + 4))
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
                            (ExpectingIndentation numSpaces)
                        )
            )
            (List.range bottom top)
        )


oneOf blocks expectations context seed =
    let
        children =
            List.foldl (gatherParsers context)
                { blockNames = []
                , childBlocks = []
                , childValues = []
                , seed = newSeed
                }
                blocks

        blockParser =
            failableBlocks
                { names = children.blockNames
                , parsers = children.childBlocks
                }

        ( parentId, newSeed ) =
            Id.step seed
    in
    ( children.seed
    , Parser.succeed
        (\result ->
            case result of
                Ok details ->
                    OneOf
                        { choices = expectations
                        , child = Found details.range details.value
                        , id = parentId
                        }

                Err details ->
                    OneOf
                        { choices = expectations
                        , child =
                            Unexpected
                                { range = details.range
                                , problem = details.error
                                }
                        , id = parentId
                        }
        )
        |= withRangeResult
            (Parser.oneOf
                (blockParser
                    :: List.reverse
                        (unexpectedInOneOf expectations
                            :: children.childValues
                        )
                )
            )
    )


gatherParsers context =
    \myBlock details ->
        let
            ( currentSeed, parser ) =
                getParserNoBar context details.seed myBlock
        in
        case blockName myBlock of
            Just name ->
                { blockNames = name :: details.blockNames
                , childBlocks = parser :: details.childBlocks
                , childValues = details.childValues
                , seed = currentSeed
                }

            Nothing ->
                { blockNames = details.blockNames
                , childBlocks = details.childBlocks
                , childValues = Parser.map Ok parser :: details.childValues
                , seed = currentSeed
                }


unexpectedInOneOf expectations =
    withIndent
        (\indentation ->
            Parser.succeed
                (\( pos, foundWord ) ->
                    Err (Error.FailMatchOneOf (List.map humanReadableExpectations expectations))
                )
                |= withRange word
        )


getFailableBlock context seed (Block details) =
    case details.kind of
        Named name ->
            let
                ( newSeed, blockParser ) =
                    details.parser context seed
            in
            ( newSeed
            , failableBlocks
                { names = [ name ]
                , parsers =
                    [ blockParser
                    ]
                }
            )

        Value ->
            Tuple.mapSecond (Parser.map Ok) (details.parser context seed)

        VerbatimNamed name ->
            Tuple.mapSecond (Parser.map Ok) (details.parser ParseInline seed)

        AnnotationNamed name ->
            Tuple.mapSecond (Parser.map Ok) (details.parser ParseInline seed)


{-| This parser will either:

    - Parse one of the blocks
    - Fail to parse a `|` and continue on
    - Parse a `|`, fail to parse the rest and return an Error

-}
failableBlocks blocks =
    Parser.succeed (\pos block -> Result.map (resetBlockStart pos) block)
        |= getPosition
        |. Parser.token (Parser.Token "|>" BlockStart)
        |. Parser.chompWhile (\c -> c == ' ')
        |= Parser.oneOf
            (List.map (Parser.map Ok) blocks.parsers
                ++ [ withIndent
                        (\indentation ->
                            Parser.succeed
                                (Err (Error.UnknownBlock blocks.names))
                                |. word
                                |. Parser.chompWhile (\c -> c == ' ')
                                |. newline
                                |. Parser.loop "" (raggedIndentedStringAbove indentation)
                        )
                   ]
            )



{- TEXT PARSING -}


{-| -}
type TextCursor
    = TextCursor
        { current : Text
        , start : Position
        , found : List TextDescription
        , balancedReplacements : List String
        }


{-| -}
type Replacement
    = Replacement String String
    | Balanced
        { start : ( String, String )
        , end : ( String, String )
        }


textCursor inheritedStyles startingPos =
    TextCursor
        { current = Text inheritedStyles ""
        , found = []
        , start = startingPos
        , balancedReplacements = []
        }


styledText :
    { inlines : List (Block a)
    , replacements : List Replacement
    }
    -> ParseContext
    -> Id.Seed
    -> Position
    -> Styling
    -> List Char
    -> Parser Context Problem Description
styledText options context seed startingPos inheritedStyles until =
    let
        vacantText =
            textCursor inheritedStyles startingPos

        untilStrings =
            List.map String.fromChar until

        meaningful =
            '1' :: '\\' :: '\n' :: until ++ stylingChars ++ replacementStartingChars options.replacements

        -- Note #1 : We're conting on the caller of styled text to advance the seed for us.
        ( newId, newSeed ) =
            Id.step seed
    in
    Parser.oneOf
        [ Parser.map
            (\( pos, textNodes ) ->
                DescribeText
                    { id = newId
                    , range = pos
                    , text = textNodes
                    }
            )
            (withRange
                (Parser.loop vacantText
                    (styledTextLoop options context meaningful untilStrings)
                )
            )
        ]


{-| -}
styledTextLoop :
    { inlines : List (Block a)
    , replacements : List Replacement
    }
    -> ParseContext
    -> List Char
    -> List String
    -> TextCursor
    -> Parser Context Problem (Parser.Step TextCursor (List TextDescription))
styledTextLoop options context meaningful untilStrings found =
    -- let
    --     _ =
    --         Debug.log "options" options.inlines
    -- in
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
            (Parser.Loop << changeStyle found)
            |= Parser.oneOf
                [ Parser.map (always Italic) (Parser.token (Parser.Token "/" (Expecting "/")))
                , Parser.map (always Strike) (Parser.token (Parser.Token "~" (Expecting "~")))
                , Parser.map (always Bold) (Parser.token (Parser.Token "*" (Expecting "*")))
                ]
        , case List.filter onlyStandalone options.inlines of
            standalones ->
                Parser.succeed
                    (\src start result end ->
                        let
                            range =
                                { start = start
                                , end = end
                                }
                        in
                        case result of
                            Ok desc ->
                                found
                                    |> commitText
                                    |> addToTextCursor
                                        (InlineBlock
                                            { kind = EmptyAnnotation
                                            , range =
                                                range
                                            , record = desc
                                            }
                                        )
                                    |> advanceTo end
                                    |> Parser.Loop

                            Err err ->
                                Parser.Loop (addText (sliceRange range src) found)
                    )
                    |= Parser.getSource
                    |= getPosition
                    |= attrContainer standalones
                    |= getPosition

        -- Parse Selection
        -- depending on selection type, capture attributes if applicable.
        , Parser.succeed
            (\start ( maybeNewCursor, newInlineBlock ) end ->
                let
                    resetCursor curs =
                        case maybeNewCursor of
                            Nothing ->
                                curs

                            Just (TextCursor newCursor) ->
                                curs
                                    |> resetBalancedReplacements newCursor.balancedReplacements
                                    |> resetTextWith newCursor.current
                in
                found
                    |> commitText
                    |> addToTextCursor
                        (newInlineBlock
                            { start = start
                            , end = end
                            }
                        )
                    |> resetCursor
                    |> advanceTo end
                    |> Parser.Loop
            )
            |= getPosition
            |= (textSelection options.replacements found
                    |> Parser.andThen
                        (\( maybeNewCursor, selection ) ->
                            Parser.map
                                (\attrResult ->
                                    ( maybeNewCursor
                                    , \range ->
                                        case attrResult of
                                            Err [ InlineStart ] ->
                                                case selection of
                                                    SelectString str ->
                                                        Styled range (Text (getCurrentStyle found) str)

                                                    _ ->
                                                        -- TODO: some sort of real error happend
                                                        InlineBlock
                                                            { kind = selection
                                                            , range =
                                                                range
                                                            , record = DescribeNothing (Tuple.first (Id.step Id.initialSeed))
                                                            }

                                            Err errs ->
                                                -- TODO: some sort of real error happend
                                                InlineBlock
                                                    { kind = selection
                                                    , range =
                                                        range
                                                    , record = DescribeNothing (Tuple.first (Id.step Id.initialSeed))
                                                    }

                                            Ok foundFields ->
                                                InlineBlock
                                                    { kind = selection
                                                    , range =
                                                        range
                                                    , record = foundFields
                                                    }
                                    )
                                )
                                (attrContainer
                                    (case selection of
                                        SelectString _ ->
                                            List.filter onlyVerbatim options.inlines

                                        SelectText _ ->
                                            List.filter onlyAnnotation options.inlines

                                        EmptyAnnotation ->
                                            -- TODO: parse only normal records
                                            []
                                    )
                                )
                        )
               )
            |= getPosition
        , -- chomp until a meaningful character
          Parser.succeed
            (\( new, final ) ->
                if new == "" || final then
                    case commitText (addText (String.trimRight new) found) of
                        TextCursor txt ->
                            let
                                styling =
                                    case txt.current of
                                        Text s _ ->
                                            s
                            in
                            Parser.Done (List.reverse txt.found)

                else
                    Parser.Loop (addText new found)
            )
            |= (Parser.getChompedString (Parser.chompWhile (\c -> not (List.member c meaningful)))
                    |> Parser.andThen
                        (\str ->
                            Parser.oneOf
                                [ Parser.succeed ( str, True )
                                    |. Parser.token (Parser.Token "\n\n" Newline)
                                , withIndent
                                    (\indentation ->
                                        Parser.succeed
                                            (\finished ->
                                                case finished of
                                                    StopWith add ->
                                                        ( str ++ add, True )

                                                    ContinueWith add ->
                                                        ( str ++ add, False )
                                            )
                                            |. Parser.backtrackable (Parser.token (Parser.Token ("\n" ++ String.repeat indentation " ") Newline))
                                            |= Parser.oneOf
                                                ([ Parser.map (always (StopWith "")) (Parser.end End)
                                                 , Parser.map (always (StopWith "")) newline
                                                 ]
                                                    ++ (case context of
                                                            -- in a list context, fail and backtrack if we parse
                                                            --indentation followed by `-` or `1.`
                                                            ParseInTree ->
                                                                [ Parser.backtrackable (Parser.token (Parser.Token "-" Newline))
                                                                    |> Parser.andThen (always (Parser.problem (Expecting "---")))
                                                                , Parser.backtrackable (Parser.token (Parser.Token "1." Newline))
                                                                    |> Parser.andThen (always (Parser.problem (Expecting "1.")))
                                                                , Parser.map (\c -> ContinueWith ("\n" ++ c))
                                                                    (Parser.chompIf (\c -> c /= '-' && c /= '1' && c /= ' ') (Expecting "char")
                                                                        |> Parser.getChompedString
                                                                    )
                                                                ]

                                                            _ ->
                                                                [ Parser.succeed (ContinueWith "\n") ]
                                                       )
                                                )
                                     -- TODO do we need to check that this isn't just a completely blank line?
                                    )
                                , Parser.succeed ( str, True )
                                    |. Parser.token (Parser.Token "\n" Newline)
                                , Parser.succeed ( str, True )
                                    |. Parser.end End
                                , Parser.succeed ( str, False )
                                ]
                        )
               )
        ]


type TextChompResult
    = StopWith String
    | ContinueWith String


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
    List.map captureChar ('1' :: allFirstChars)


textSelection replacements found =
    Parser.oneOf
        [ Parser.succeed (\str -> ( Nothing, SelectString str ))
            |. Parser.token (Parser.Token "`" (Expecting "`"))
            |= Parser.getChompedString
                (Parser.chompWhile (\c -> c /= '`' && c /= '\n'))
            |. Parser.chompWhile (\c -> c == '`')
        , Parser.succeed
            (\( txts, cursor ) ->
                ( Just cursor, SelectText txts )
            )
            |. Parser.token (Parser.Token "[" (Expecting "["))
            |= Parser.loop
                (textCursor (getCurrentStyles found)
                    { offset = 0
                    , line = 1
                    , column = 1
                    }
                )
                (simpleStyledTextTill [ '\n', ']' ] replacements)
            |. Parser.token (Parser.Token "]" (Expecting "]"))
        ]


simpleStyledTextTill :
    List Char
    -> List Replacement
    -> TextCursor
    -> Parser Context Problem (Parser.Step TextCursor ( List Text, TextCursor ))
simpleStyledTextTill until replacements cursor =
    -- options meaningful untilStrings found =
    Parser.oneOf
        [ Parser.oneOf (replace replacements cursor)
            |> Parser.map Parser.Loop

        -- If a char matches the first character of a replacement,
        -- but didn't match the full replacement captured above,
        -- then stash that char.
        , Parser.oneOf (almostReplacement replacements cursor)
            |> Parser.map Parser.Loop

        -- Capture style command characters
        , Parser.succeed
            (Parser.Loop << changeStyle cursor)
            |= Parser.oneOf
                [ Parser.map (always Italic) (Parser.token (Parser.Token "/" (Expecting "/")))
                , Parser.map (always Strike) (Parser.token (Parser.Token "~" (Expecting "~")))
                , Parser.map (always Bold) (Parser.token (Parser.Token "*" (Expecting "*")))
                ]
        , -- chomp until a meaningful character
          Parser.chompWhile
            (\c ->
                not (List.member c ('\\' :: '\n' :: until ++ stylingChars ++ replacementStartingChars replacements))
            )
            |> Parser.getChompedString
            |> Parser.andThen
                (\new ->
                    if new == "" || new == "\n" then
                        case commitText cursor of
                            TextCursor txt ->
                                let
                                    styling =
                                        case txt.current of
                                            Text s _ ->
                                                s
                                in
                                Parser.succeed
                                    (Parser.Done
                                        ( List.reverse <| List.filterMap toText txt.found
                                        , TextCursor txt
                                        )
                                    )

                    else
                        Parser.succeed (Parser.Loop (addText new cursor))
                )
        ]


toText textDesc =
    case textDesc of
        Styled _ txt ->
            Just txt

        _ ->
            Nothing


{-| Match one of the attribute containers

    {mytoken| attributeList }
    ^                       ^

Because there is no styled text here, we know the following can't happen:

    1. Change in text styles
    2. Any Replacements

This parser is configureable so that it will either

    fastForward or skip.

If the attributes aren't required (i.e. in a oneOf), then we want to skip to allow testing of other possibilities.

If they are required, then we can fastforward to a specific condition and continue on.

-}
attrContainer : List (Block a) -> Tolerant.Parser Context Problem Description
attrContainer recordBlocks =
    Tolerant.succeed identity
        |. Tolerant.try
            (Parser.chompIf (\c -> c == '{') (Expecting "{"))
        |> Tolerant.ignore (Tolerant.chompWhile (\c -> c == ' '))
        |> Tolerant.keep
            (Tolerant.oneOf (ExpectingInlineName "")
                (recordBlocks
                    -- NOTE: We're throwing away IDs here, maybe we dont want to do that?
                    |> List.map
                        (\rec ->
                            let
                                recordParser =
                                    getParser ParseInline Id.initialSeed rec
                            in
                            Tolerant.try (Tuple.second recordParser)
                        )
                )
            )
        |> Tolerant.ignore (Tolerant.chompWhile (\c -> c == ' '))
        |> Tolerant.ignore
            (Tolerant.token
                { match = "}"
                , problem = InlineEnd
                , onError = Tolerant.fastForwardTo [ '}', '\n' ]
                }
            )



{- Style Helpers -}


changeStyle (TextCursor cursor) styleToken =
    let
        cursorText =
            case cursor.current of
                Text _ txt ->
                    txt

        newText =
            cursor.current
                |> flipStyle styleToken
                |> clearText
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


clearText (Text styles _) =
    Text styles ""


flipStyle newStyle textStyle =
    case textStyle of
        Text styles str ->
            case newStyle of
                Bold ->
                    Text { styles | bold = not styles.bold } str

                Italic ->
                    Text { styles | italic = not styles.italic } str

                Strike ->
                    Text { styles | strike = not styles.strike } str


advanceTo target (TextCursor cursor) =
    TextCursor
        { found = cursor.found
        , current = cursor.current
        , start = target
        , balancedReplacements = cursor.balancedReplacements
        }


getCurrentStyles (TextCursor cursor) =
    getStyles cursor.current


getStyles (Text styles _) =
    styles


measure start textStr =
    let
        len =
            String.length textStr
    in
    { start
        | offset = start.offset + len
        , column = start.column + len
    }


commitText ((TextCursor cursor) as existingTextCursor) =
    case cursor.current of
        Text _ "" ->
            -- nothing to commit
            existingTextCursor

        Text styles cursorText ->
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
                , current = Text styles ""
                , balancedReplacements = cursor.balancedReplacements
                }


getCurrentStyle (TextCursor cursor) =
    case cursor.current of
        Text s _ ->
            s


addToTextCursor new (TextCursor cursor) =
    TextCursor { cursor | found = new :: cursor.found }


resetBalancedReplacements newBalance (TextCursor cursor) =
    TextCursor { cursor | balancedReplacements = newBalance }


resetTextWith (Text styles _) (TextCursor cursor) =
    TextCursor { cursor | current = Text styles "" }



-- |> resetStylesTo cursor.current
{- REPLACEMENT HELPERS -}


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
                    addText esc existing
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


addBalance id (TextCursor cursor) =
    TextCursor <|
        { cursor | balancedReplacements = id :: cursor.balancedReplacements }


removeBalance id (TextCursor cursor) =
    TextCursor <|
        { cursor | balancedReplacements = List.filter ((/=) id) cursor.balancedReplacements }


addText newTxt (TextCursor cursor) =
    case cursor.current of
        Text styles txt ->
            TextCursor { cursor | current = Text styles (txt ++ newTxt) }


stylingChars =
    [ '~'
    , '['
    , '/'
    , '*'
    , '\n'
    , '{'
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



{- GENERAL HELPERS -}


withIndent fn =
    Parser.getIndent
        |> Parser.andThen fn


withRangeResult :
    Parser Context Problem (Result err thing)
    ->
        Parser Context
            Problem
            (Result
                { range : Range
                , error : err
                }
                { range : Range
                , value : thing
                }
            )
withRangeResult parser =
    Parser.succeed
        (\start result end ->
            case result of
                Ok val ->
                    Ok
                        { range =
                            { start = start
                            , end = end
                            }
                        , value = val
                        }

                Err err ->
                    let
                        range =
                            { start = start
                            , end = end
                            }
                    in
                    Err
                        { range = range
                        , error = err
                        }
        )
        |= getPosition
        |= parser
        |= getPosition


getRangeAndSource :
    Parser Context Problem thing
    ->
        Parser Context
            Problem
            { source : String
            , range : Range
            , value : thing
            }
getRangeAndSource parser =
    Parser.succeed
        (\src start result end ->
            let
                range =
                    { start = start
                    , end = end
                    }
            in
            { range = range
            , value = result
            , source = sliceRange range src
            }
        )
        |= Parser.getSource
        |= getPosition
        |= parser
        |= getPosition


sliceRange range source =
    if range.start.line == range.end.line then
        -- single line
        let
            lineStart =
                range.start.offset - (range.start.column - 1)
        in
        String.slice lineStart (range.end.offset + 20) source
            |> String.lines
            |> List.head
            |> Maybe.withDefault ""

    else
        -- multiline
        let
            snippet =
                String.slice range.start.offset range.end.offset source

            indented =
                String.slice (range.start.offset + 1 - range.start.column)
                    range.start.offset
                    source
        in
        indented ++ snippet


withRange :
    Parser Context Problem thing
    -> Parser Context Problem ( Range, thing )
withRange parser =
    Parser.succeed
        (\start val end ->
            ( { start = start
              , end = end
              }
            , val
            )
        )
        |= getPosition
        |= parser
        |= getPosition


word : Parser Context Problem String
word =
    Parser.chompWhile Char.isAlphaNum
        |> Parser.getChompedString


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



{- MISC HELPERS -}


{-| -}
onlyStandalone : Block a -> Bool
onlyStandalone (Block details) =
    case details.kind of
        Value ->
            False

        Named name ->
            True

        VerbatimNamed _ ->
            False

        AnnotationNamed _ ->
            False


onlyVerbatim : Block a -> Bool
onlyVerbatim (Block details) =
    case details.kind of
        Value ->
            False

        Named name ->
            False

        VerbatimNamed _ ->
            True

        AnnotationNamed _ ->
            False


onlyAnnotation : Block a -> Bool
onlyAnnotation (Block details) =
    case details.kind of
        Value ->
            False

        Named name ->
            False

        VerbatimNamed _ ->
            False

        AnnotationNamed _ ->
            True


{-| -}
manyOf indentation blocks cursor =
    Parser.oneOf
        [ Parser.end End
            |> Parser.map
                (\_ ->
                    Parser.Done (List.reverse cursor.found)
                )
        , Parser.succeed
            (Parser.Loop
                { parsedSomething = True
                , found = cursor.found
                , seed = cursor.seed
                }
            )
            |. newlineWith "empty newline"
        , if not cursor.parsedSomething then
            -- First thing already has indentation accounted for.
            makeBlocksParser blocks cursor.seed
                |> Parser.map
                    (\foundBlock ->
                        let
                            ( _, newSeed ) =
                                Id.step cursor.seed
                        in
                        Parser.Loop
                            { parsedSomething = True
                            , found = foundBlock :: cursor.found
                            , seed = newSeed
                            }
                    )

          else
            Parser.oneOf
                [ Parser.succeed
                    (\foundBlock ->
                        let
                            ( _, newSeed ) =
                                Id.step cursor.seed
                        in
                        Parser.Loop
                            { parsedSomething = True
                            , found = foundBlock :: cursor.found
                            , seed = newSeed
                            }
                    )
                    |. Parser.token (Parser.Token (String.repeat indentation " ") (ExpectingIndentation indentation))
                    |= makeBlocksParser blocks cursor.seed
                , Parser.succeed
                    (Parser.Loop
                        { parsedSomething = True
                        , found = cursor.found
                        , seed = cursor.seed
                        }
                    )
                    |. Parser.backtrackable (Parser.chompWhile (\c -> c == ' '))
                    |. Parser.backtrackable newline

                -- We reach here because the indentation parsing was not successful,
                -- meaning the indentation has been lowered and the block is done
                , Parser.succeed (Parser.Done (List.reverse cursor.found))
                ]

        -- Whitespace Line
        , Parser.succeed
            (Parser.Loop
                { parsedSomething = True
                , found = cursor.found
                , seed = cursor.seed
                }
            )
            |. Parser.chompWhile (\c -> c == ' ')
            |. newlineWith "ws-line"
        ]


makeBlocksParser blocks seed =
    let
        gatherParsers2 myBlock details =
            let
                -- We don't care about the new seed because that's handled by the loop.
                ( _, parser ) =
                    getParserNoBar ParseBlock seed myBlock
            in
            case blockName myBlock of
                Just name ->
                    { blockNames = name :: details.blockNames
                    , childBlocks = Parser.map Ok parser :: details.childBlocks
                    , childValues = details.childValues
                    }

                Nothing ->
                    { blockNames = details.blockNames
                    , childBlocks = details.childBlocks
                    , childValues = Parser.map Ok (withRange parser) :: details.childValues
                    }

        children =
            List.foldl gatherParsers2
                { blockNames = []
                , childBlocks = []
                , childValues = []
                }
                blocks

        blockParser =
            Parser.map
                (\( pos, result ) ->
                    Result.map (\desc -> ( pos, desc )) result
                )
                (withRange
                    (Parser.succeed (\pos block -> Result.map (resetBlockStart pos) block)
                        |= getPosition
                        |. Parser.token (Parser.Token "|>" BlockStart)
                        |. Parser.chompWhile (\c -> c == ' ')
                        |= Parser.oneOf
                            (List.reverse children.childBlocks
                                ++ [ withIndent
                                        (\indentation ->
                                            Parser.succeed
                                                (\( pos, foundWord ) ->
                                                    Err ( pos, Error.UnknownBlock children.blockNames )
                                                )
                                                |= withRange word
                                                |. newline
                                                |. Parser.loop "" (raggedIndentedStringAbove indentation)
                                        )
                                   ]
                            )
                    )
                )
    in
    Parser.oneOf
        (blockParser
            :: List.reverse children.childValues
        )


type alias NestedIndex =
    { base : Int
    , prev : Int
    , seed : Id.Seed
    }


type alias FlatCursor =
    { icon : Maybe Icon
    , indent : Int
    , range : Range
    , content : Description
    }


{-| Results in a flattened version of the parsed list.

    ( 0, Maybe Icon, [ "item one" ] )

    ( 0, Maybe Icon, [ "item two" ] )

    ( 4, Maybe Icon, [ "nested item two", "additional text for nested item two" ] )

    ( 0, Maybe Icon, [ "item three" ] )

    ( 4, Maybe Icon, [ "nested item three" ] )

-}
tree :
    ParseContext
    -> Block thing
    -> ( NestedIndex, List FlatCursor )
    -> Parser Context Problem (Parser.Step ( NestedIndex, List FlatCursor ) (List FlatCursor))
tree context item ( indentation, existing ) =
    Parser.oneOf
        [ Parser.end End
            |> Parser.map
                (\_ ->
                    Parser.Done (List.reverse existing)
                )

        -- Whitespace Line
        , skipBlankLineWith (Parser.Loop ( indentation, existing ))
        , Parser.oneOf
            [ -- block with required indent
              expectIndentation indentation.base indentation.prev
                |> Parser.andThen
                    (\newIndent ->
                        let
                            newSeed =
                                if newIndent > indentation.prev then
                                    Id.indent indentation.seed

                                else if newIndent == indentation.prev then
                                    -- This may look weird
                                    -- but this loop is started with an indented id, which should be unique.
                                    -- Therefore we only need to increment it after the first thing found
                                    case existing of
                                        [] ->
                                            indentation.seed

                                        _ ->
                                            Id.step indentation.seed
                                                |> Tuple.second

                                else
                                    Id.dedent ((indentation.prev - newIndent) // 4) indentation.seed
                                        -- we're back in old land, so we have to increment to avoid a collision
                                        |> Id.step
                                        |> Tuple.second

                            ( itemSeed, itemParser ) =
                                getParser context newSeed item
                        in
                        -- If the indent has changed, then the delimiter is required
                        Parser.withIndent newIndent <|
                            Parser.oneOf
                                ((Parser.succeed
                                    (\start iconResult itemResult end ->
                                        let
                                            newIndex =
                                                { prev = newIndent
                                                , seed =
                                                    newSeed
                                                , base = indentation.base
                                                }
                                        in
                                        Parser.Loop
                                            ( newIndex
                                            , { indent = newIndent
                                              , range = { start = start, end = end }
                                              , icon = Just iconResult
                                              , content = itemResult
                                              }
                                                :: existing
                                            )
                                    )
                                    |= getPosition
                                    |= iconParser
                                    |= Parser.withIndent (newIndent + 4) itemParser
                                    |= getPosition
                                 )
                                    :: (if newIndent - 4 == indentation.prev then
                                            [ Parser.succeed
                                                (\start foundBlock end ->
                                                    let
                                                        newIndex =
                                                            { prev = indentation.prev
                                                            , seed = newSeed
                                                            , base = indentation.base
                                                            }
                                                    in
                                                    Parser.Loop
                                                        ( newIndex
                                                        , { indent = indentation.prev
                                                          , range = { start = start, end = end }
                                                          , icon = Nothing
                                                          , content = foundBlock
                                                          }
                                                            :: existing
                                                        )
                                                )
                                                |= getPosition
                                                |= itemParser
                                                |= getPosition
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


debugParser str =
    Parser.map
        (Debug.log str)


type RecordType
    = InlineRecord
    | BlockRecord


record recordType id recordName expectations fields =
    Parser.succeed
        (\result ->
            case result of
                Ok details ->
                    Record
                        { expected = expectations
                        , id = id
                        , name = recordName
                        , found =
                            Found details.range details.value
                        }

                Err err ->
                    Record
                        { expected = expectations
                        , id = id
                        , name = recordName
                        , found =
                            Unexpected
                                { range = Maybe.withDefault err.range (Tuple.first err.error)
                                , problem = Tuple.second err.error
                                }
                        }
        )
        |= withRangeResult
            (withIndent
                (\indentation ->
                    Parser.succeed identity
                        |. Parser.keyword (Parser.Token recordName (ExpectingBlockName recordName))
                        |. Parser.chompWhile (\c -> c == ' ')
                        |= (if List.isEmpty fields then
                                Parser.succeed (Ok [])

                            else
                                Parser.succeed identity
                                    |. (case recordType of
                                            InlineRecord ->
                                                Parser.chompIf (\c -> c == '|') (Expecting "bar")

                                            BlockRecord ->
                                                Parser.chompIf (\c -> c == '\n') Newline
                                       )
                                    |= (case recordType of
                                            InlineRecord ->
                                                Parser.loop
                                                    { remaining = fields
                                                    , found = Ok []
                                                    }
                                                    (parseInlineFields recordName (List.map Tuple.first fields))

                                            BlockRecord ->
                                                Parser.withIndent (indentation + 4)
                                                    (Parser.loop
                                                        { remaining = fields
                                                        , found = Ok []
                                                        }
                                                        (parseFields recordName (List.map Tuple.first fields))
                                                    )
                                       )
                           )
                )
            )


backtrackCharacters chars range =
    { start =
        { offset = range.start.offset - chars
        , line = range.start.line
        , column = range.start.column - chars
        }
    , end = range.end
    }


type alias RecordFields =
    { remaining :
        List ( String, Parser Context Problem ( String, Found Description ) )
    , found :
        Result ( Maybe Range, Error.Error ) (List ( String, Found Description ))
    }


type Indented thing
    = Indented thing
    | WeirdIndent Int
    | EmptyLine


{-| -}
parseFields :
    String
    -> List String
    -> RecordFields
    -> Parser Context Problem (Parser.Step RecordFields (Result ( Maybe Range, Error.Error ) (List ( String, Found Description ))))
parseFields recordName fieldNames fields =
    case fields.remaining of
        [] ->
            withIndent
                (\indentation ->
                    Parser.succeed
                        (\remaining ->
                            if String.trim remaining == "" then
                                Parser.Done fields.found

                            else
                                Parser.Done
                                    (Err
                                        ( Nothing
                                        , Error.UnexpectedField
                                            { options = fieldNames
                                            , found = String.trim remaining
                                            , recordName = recordName
                                            }
                                        )
                                    )
                        )
                        |= Parser.oneOf
                            [ Parser.succeed identity
                                |. Parser.token
                                    (Parser.Token (String.repeat indentation " ")
                                        (ExpectingIndentation indentation)
                                    )
                                |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n'))
                            , Parser.succeed ""
                            ]
                )

        _ ->
            case fields.found of
                Ok found ->
                    withIndent
                        (\indentation ->
                            Parser.oneOf
                                [ indentOrSkip indentation (captureField found recordName fields fieldNames)
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
                                                            Err ( Nothing, Error.ExpectingIndent indentation )
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
                                            ( Nothing, Error.MissingFields (List.map Tuple.first fields.remaining) )
                                        )
                                    )
                                ]
                        )

                Err unexpected ->
                    -- We've encountered an error, but we still need to parse
                    -- the entire indented block.  so that the parser can continue.
                    withIndent
                        (\indentation ->
                            Parser.succeed (Parser.Done fields.found)
                                |. Parser.loop "" (raggedIndentedStringAbove (indentation - 4))
                        )


{-| Either:

    1. Parses indent ++ parser ++ newline
        -> Outcome.Success!
    2. Parses many spaces ++ newline
        -> Ignore completely
    3. Parses some number of spaces ++ some not newlines ++ newline
        -> Is improperly indented

-}
indentOrSkip :
    Int
    -> Parser Context Problem (Parser.Step RecordFields a)
    -> Parser Context Problem (Indented (Parser.Step RecordFields a))
indentOrSkip indentation successParser =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.token (Parser.Token (String.repeat indentation " ") (ExpectingIndentation indentation))
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
                    |. newlineWith "indentOrSkip one"

                -- parse field
                , Parser.succeed Indented
                    |= successParser
                    |. Parser.chompWhile (\c -> c == '\n')
                ]

        -- We're here because there is less than the desired indent.
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


captureField :
    List ( String, Found Description )
    -> String
    -> RecordFields
    -> List String
    -> Parser Context Problem (Parser.Step RecordFields a)
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
            (List.map (Parser.map Just << parseField) fields.remaining
                ++ [ Parser.map Just (unexpectedField recordName fieldNames)
                   ]
            )
        )


parseField ( name, contentParser ) =
    Parser.succeed identity
        |. Parser.keyword (Parser.Token name (ExpectingFieldName name))
        |. Parser.chompWhile (\c -> c == ' ')
        |. Parser.chompIf (\c -> c == '=') (Expecting "=")
        |. Parser.chompWhile (\c -> c == ' ')
        |= contentParser


unexpectedField recordName options =
    withIndent
        (\indentation ->
            Parser.map
                (\{ range, value } ->
                    ( value
                    , Unexpected
                        { range = range
                        , problem =
                            Error.UnexpectedField
                                { found = value
                                , options = options
                                , recordName = recordName
                                }
                        }
                    )
                )
                (getRangeAndSource
                    (Parser.succeed identity
                        |= Parser.getChompedString (Parser.chompWhile Char.isAlphaNum)
                        |. Parser.chompWhile (\c -> c == ' ')
                        |. Parser.chompIf (\c -> c == '=') (Expecting "=")
                        |. Parser.chompWhile (\c -> c == ' ')
                        -- TODO: parse multiline string
                        |. Parser.withIndent (indentation + 4) (Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n')))
                     -- |. newline
                     -- |. Parser.map (Debug.log "unexpected capture") (Parser.loop "" (raggedIndentedStringAbove (indent - 4)))
                    )
                )
        )



{- Inline Record Fields -}


{-| -}
parseInlineFields :
    String
    -> List String
    -> RecordFields
    -> Parser Context Problem (Parser.Step RecordFields (Result ( Maybe Range, Error.Error ) (List ( String, Found Description ))))
parseInlineFields recordName fieldNames fields =
    let
        hasMore =
            case fields.remaining of
                [] ->
                    False

                fst :: [] ->
                    False

                _ ->
                    True
    in
    case fields.remaining of
        [] ->
            Parser.succeed
                (\remaining ->
                    if String.trim remaining == "" then
                        Parser.Done fields.found

                    else
                        Parser.Done
                            (Err
                                ( Nothing
                                , Error.UnexpectedField
                                    { options = fieldNames
                                    , found = String.trim remaining
                                    , recordName = recordName
                                    }
                                )
                            )
                )
                |= Parser.oneOf
                    [ Parser.getChompedString (Parser.chompWhile (\c -> c /= '}'))
                    , Parser.succeed ""
                    ]

        _ ->
            case fields.found of
                Ok found ->
                    Parser.oneOf
                        [ Parser.succeed identity
                            |. Parser.chompWhile (\c -> c == ' ')
                            |= captureField found recordName fields fieldNames
                            |. Parser.chompWhile (\c -> c == ' ')
                            |. (if hasMore then
                                    Parser.token (Parser.Token "," (Expecting ","))

                                else
                                    Parser.succeed ()
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
                                    ( Nothing, Error.MissingFields (List.map Tuple.first fields.remaining) )
                                )
                            )
                        ]

                Err unexpected ->
                    -- We've encountered an error, but we still need to parse
                    -- the entire indented block.  so that the parser can continue.
                    Parser.succeed (Parser.Done fields.found)
                        |. Parser.chompWhile (\c -> c /= '}')


{--}
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
                |. Parser.token (Parser.Token (String.repeat (previous + 4) " ") (ExpectingIndentation (previous + 4)))
             , Parser.succeed previous
                |. Parser.token (Parser.Token (String.repeat previous " ") (ExpectingIndentation previous))
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
                        (ExpectingIndentation (base + indentLevel))
            )


iconParser =
    Parser.oneOf
        [ Parser.succeed Bullet
            |. Parser.chompIf (\c -> c == '-') (Error.Expecting "-")
            |. Parser.chompWhile (\c -> c == '-' || c == ' ')
        , Parser.succeed (AutoNumber 1)
            |. Parser.token (Parser.Token "1" Newline)
            |. Parser.chompWhile (\c -> c == '.' || c == ' ')
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
                        |. Parser.token (Parser.Token (String.repeat level " ") (ExpectingIndentation level))
                )
                (List.range 0 (((prev - 4) - base) // 4))
            )


buildTree : Int -> List FlatCursor -> List (Nested Description)
buildTree baseIndent items =
    let
        gather item builder =
            addItem (item.indent - baseIndent) item.icon item.content item.range builder

        newTree =
            items
                |> List.foldl groupByIcon Nothing
                |> finalizeGrouping
                |> List.reverse
                |> List.foldl gather emptyTreeBuilder
    in
    case newTree of
        TreeBuilder builder ->
            List.reverse (renderLevels builder.levels)


finalizeGrouping :
    Maybe GroupedCursor
    ->
        List
            { indent : Int
            , icon : Icon
            , range : Range
            , content : List Description
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
                    { indent = cursor.indent
                    , icon = cursor.icon
                    , range = cursor.range
                    , content = cursor.items
                    }
                        :: cursor.accumulated


type alias GroupedCursor =
    { indent : Int
    , icon : Icon
    , items : List Description
    , range : Range
    , accumulated :
        List
            { indent : Int
            , icon : Icon
            , content : List Description
            , range : Range
            }
    }


groupByIcon : FlatCursor -> Maybe GroupedCursor -> Maybe GroupedCursor
groupByIcon item maybeCursor =
    case maybeCursor of
        Nothing ->
            case item.icon of
                Just icon ->
                    Just
                        { indent = item.indent
                        , icon = icon
                        , items = [ item.content ]
                        , range = item.range
                        , accumulated = []
                        }

                Nothing ->
                    -- Because of how the code runs, we have a tenuous guarantee that this branch won't execute.
                    -- Not entirely sure how to make the types work to eliminate this.
                    Nothing

        Just cursor ->
            Just <|
                case item.icon of
                    Nothing ->
                        { indent = cursor.indent
                        , icon = cursor.icon
                        , items = item.content :: cursor.items
                        , range = item.range
                        , accumulated = cursor.accumulated
                        }

                    Just icon ->
                        { indent = item.indent
                        , icon = icon
                        , items = [ item.content ]
                        , range = item.range
                        , accumulated =
                            { indent = cursor.indent
                            , icon = cursor.icon
                            , content = cursor.items
                            , range = item.range
                            }
                                :: cursor.accumulated
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
addItem : Int -> Icon -> List Description -> Range -> TreeBuilder -> TreeBuilder
addItem indentation icon content range (TreeBuilder builder) =
    let
        newItem : Nested Description
        newItem =
            Nested
                { icon = icon
                , children = []
                , range = range
                , content = content
                }
    in
    case builder.levels of
        [] ->
            TreeBuilder
                { previousIndent = indentation
                , levels =
                    [ newItem ]
                }

        lvl :: remaining ->
            if indentation == 0 then
                -- add to current level
                TreeBuilder
                    { previousIndent = indentation
                    , levels =
                        newItem :: lvl :: remaining
                    }

            else
                -- We've dedented, so we need to first collapse the current level
                -- into the one below, then add an item to that level
                TreeBuilder
                    { previousIndent = indentation
                    , levels =
                        addToLevel
                            ((abs indentation // 4) - 1)
                            newItem
                            lvl
                            :: remaining
                    }


addToLevel index brandNewItem (Nested parent) =
    if index <= 0 then
        Nested
            { parent
                | children =
                    brandNewItem
                        :: parent.children
            }

    else
        case parent.children of
            [] ->
                Nested parent

            top :: remain ->
                Nested
                    { parent
                        | children =
                            addToLevel (index - 1) brandNewItem top
                                :: remain
                    }



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
type TreeBuilder
    = TreeBuilder
        { previousIndent : Int
        , levels :
            -- (mostRecent :: remaining)
            List (Nested Description)
        }


emptyTreeBuilder : TreeBuilder
emptyTreeBuilder =
    TreeBuilder
        { previousIndent = 0
        , levels = []
        }


renderLevels : List (Nested Description) -> List (Nested Description)
renderLevels levels =
    case levels of
        [] ->
            []

        _ ->
            List.indexedMap
                (\index level ->
                    reverseTree { index = index, level = [] } level
                )
                levels


reverseTree cursor (Nested nest) =
    Nested
        { icon = nest.icon
        , range = nest.range
        , content = List.reverse nest.content
        , children =
            List.foldl rev ( dive cursor, [] ) nest.children
                |> Tuple.second
        }


rev nest ( cursor, found ) =
    ( next cursor, reverseTree cursor nest :: found )


type alias TreeIndex =
    { index : Int
    , level : List Int
    }


dive cursor =
    { cursor
        | index = 0
        , level = cursor.index :: cursor.level
    }


next cursor =
    { cursor
        | index = cursor.index + 1
    }
