module Mark.Internal.Parser exposing
    ( PreviouslyAdded(..)
    , RecordType(..)
    , Replacement(..)
    , backtrackCharacters
    , float
    , fullTree
    , getFailableBlock
    , getPosition
    , getRangeAndSource
    , iconParser
    , indentationString
    , indentedString
    , int
    , manyOf
    , newline
    , newlineWith
    , oneOf
    , peek
    , raggedIndentedStringAbove
    , record
    , skipBlankLineWith
    , styledText
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


int : Id -> Parser Context Problem Description
int id =
    Parser.map
        (\result ->
            case result of
                Ok details ->
                    DescribeInteger
                        { id = id
                        , found = details.value
                        }

                Err details ->
                    DescribeUnexpected id
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
float : Id -> Parser Context Problem Description
float id =
    Parser.map
        (\result ->
            case result of
                Ok details ->
                    DescribeFloat
                        { id = id
                        , found = details.value
                        }

                Err details ->
                    DescribeUnexpected id
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
        [ Parser.succeed (Parser.Done (String.trimRight found))
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
                |. Parser.token (Parser.Token (indentationString indentation) (ExpectingIndentation indentation))
                |= Parser.getChompedString
                    (Parser.chompWhile
                        (\c -> c /= '\n')
                    )
        , Parser.succeed (Parser.Done (String.trimRight found))
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
                    Parser.Loop (found ++ indentationString indentCount ++ str)
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
                        (Parser.Token (indentationString numSpaces)
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
            failableBlocks parentId
                { names = children.blockNames
                , parsers = children.childBlocks
                }

        ( parentId, newSeed ) =
            Id.step seed
    in
    ( children.seed
    , Parser.oneOf
        (blockParser
            :: List.reverse
                (unexpectedInOneOf parentId expectations
                    :: children.childValues
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
                , childValues = parser :: details.childValues
                , seed = currentSeed
                }


unexpectedInOneOf :
    Id
    -> List Expectation
    -> Parser Context Problem Description
unexpectedInOneOf id expectations =
    withIndent
        (\indentation ->
            Parser.succeed
                (\( range, err ) ->
                    DescribeUnexpected id
                        { problem = Error.FailMatchOneOf (List.map humanReadableExpectations expectations)
                        , range = range
                        }
                )
                |= withRange word
        )


getFailableBlock :
    ParseContext
    -> Seed
    -> Block data
    -> ( Seed, Parser Context Problem Description )
getFailableBlock context seed (Block details) =
    case details.kind of
        Named name ->
            let
                ( newSeed, blockParser ) =
                    details.parser context seed

                ( id, finalSeed ) =
                    Id.step newSeed
            in
            ( finalSeed
            , failableBlocks id
                { names = [ name ]
                , parsers =
                    [ blockParser
                    ]
                }
            )

        Value ->
            details.parser context seed

        VerbatimNamed name ->
            details.parser ParseInline seed

        AnnotationNamed name ->
            details.parser ParseInline seed


{-| This parser will either:

    - Parse one of the blocks
    - Fail to parse a `|` and continue on
    - Parse a `|`, fail to parse the rest and return an Error

-}
failableBlocks id blocks =
    Parser.succeed identity
        |. Parser.token (Parser.Token "|>" BlockStart)
        |. Parser.chompWhile (\c -> c == ' ')
        |= Parser.oneOf
            (blocks.parsers
                ++ [ withIndent
                        (\indentation ->
                            Parser.succeed
                                (\start end ->
                                    DescribeUnexpected id
                                        { range = { start = start, end = end }
                                        , problem = Error.UnknownBlock blocks.names
                                        }
                                )
                                |= getPosition
                                |. word
                                |. Parser.chompWhile (\c -> c == ' ')
                                |. newline
                                |. Parser.loop "" (raggedIndentedStringAbove indentation)
                                |= getPosition
                        )
                   ]
            )



{- TEXT PARSING -}


{-| -}
type TextCursor
    = TextCursor
        { current : Text
        , seed : Id.Seed
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


textCursor inheritedStyles startingPos seed =
    TextCursor
        { current = Text inheritedStyles ""
        , seed = seed
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
    -> Parser Context Problem Description
styledText options context seed startingPos inheritedStyles =
    let
        meaningful =
            '1' :: '\\' :: '\n' :: stylingChars ++ replacementStartingChars options.replacements

        indentedSeed : Seed
        indentedSeed =
            Id.indent seed

        ( parentId, newSeed ) =
            Id.step seed
    in
    Parser.oneOf
        [ Parser.map
            (\( pos, textNodes ) ->
                DescribeText
                    { id = parentId
                    , text = textNodes
                    }
            )
            (withRange
                (Parser.loop
                    (textCursor inheritedStyles startingPos seed)
                    (styledTextLoop
                        options
                        context
                        meaningful
                    )
                )
            )
        ]


textJustStarted : TextCursor -> Bool
textJustStarted (TextCursor details) =
    case details.found of
        [] ->
            case details.current of
                Text _ "" ->
                    True

                _ ->
                    False

        _ ->
            False


styleChar style =
    case style of
        Italic ->
            "/"

        Bold ->
            "*"

        Strike ->
            "~"


addStyleChar chars str =
    case chars of
        [] ->
            str

        char :: remain ->
            addStyleChar remain (str ++ styleChar char)


styleTokens =
    Parser.succeed
        (\style following ->
            case following of
                "" ->
                    ( style, [] )

                _ ->
                    ( style
                    , List.map
                        (\c ->
                            case c of
                                '/' ->
                                    Italic

                                '~' ->
                                    Strike

                                '*' ->
                                    Bold

                                _ ->
                                    -- we just parsed this, so this won't occurr
                                    Bold
                        )
                        (String.toList following)
                    )
        )
        |= Parser.oneOf
            [ Parser.map (always Italic) (Parser.token (Parser.Token "/" (Expecting "/")))
            , Parser.map (always Strike) (Parser.token (Parser.Token "~" (Expecting "~")))
            , Parser.map (always Bold) (Parser.token (Parser.Token "*" (Expecting "*")))
            ]
        |= Parser.getChompedString (Parser.chompWhile (\c -> c == '/' || c == '~' || c == '*'))


{-| -}
styledTextLoop :
    { inlines : List (Block a)
    , replacements : List Replacement
    }
    -> ParseContext
    -> List Char
    -> TextCursor
    -> Parser Context Problem (Parser.Step TextCursor (List TextDescription))
styledTextLoop options context meaningful cursor =
    Parser.oneOf
        [ Parser.oneOf (replace options.replacements cursor)
            |> Parser.map Parser.Loop

        -- If a char matches the first character of a replacement,
        -- but didn't match the full replacement captured above,
        -- then stash that char.
        , Parser.oneOf (almostReplacement options.replacements cursor)
            |> Parser.map Parser.Loop
        , Parser.succeed
            (\styling ->
                Parser.Loop (changeStyle cursor styling)
            )
            |= styleTokens
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
                            Ok ( newCursor, desc ) ->
                                newCursor
                                    |> commitText
                                    |> addToTextCursor
                                        (InlineBlock
                                            { kind = EmptyAnnotation
                                            , record = desc
                                            }
                                        )
                                    |> advanceTo end
                                    |> Parser.Loop

                            Err err ->
                                Parser.Loop (addText (sliceRange range src) cursor)
                    )
                    |= Parser.getSource
                    |= getPosition
                    |= attrContainer cursor standalones
                    |= getPosition

        -- Parse Selection
        -- depending on selection type, capture attributes if applicable.
        , Parser.succeed
            (\start ( newCursor, newInlineBlock ) end ->
                let
                    resetCursor curs =
                        case newCursor of
                            TextCursor newCursorDetails ->
                                curs
                                    |> resetBalancedReplacements newCursorDetails.balancedReplacements
                                    |> resetTextWith newCursorDetails.current
                in
                cursor
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
            |= (textSelection options.replacements cursor
                    |> Parser.andThen (parseInlineAttributes options)
               )
            |= getPosition
        , -- chomp until a meaningful character
          Parser.succeed
            (\( new, final ) ->
                if new == "" || final || endingWithEmptyLine new cursor then
                    case commitAndFinalizeText (addText new cursor) of
                        TextCursor txt ->
                            Parser.Done (List.reverse txt.found)

                else
                    Parser.Loop (addText new cursor)
            )
            |= (Parser.chompWhile (\c -> not (List.member c meaningful))
                    |> Parser.getChompedString
                    |> Parser.andThen (finalizeString context)
               )
        ]


endingWithEmptyLine : String -> TextCursor -> Bool
endingWithEmptyLine new (TextCursor cursor) =
    if String.endsWith "\n" new && String.isEmpty (String.trim new) then
        case cursor.current of
            Text _ str ->
                String.endsWith "\n" str

    else
        False


parseInlineAttributes :
    { inlines : List (Block a)
    , replacements : List Replacement
    }
    -> ( TextCursor, InlineSelection )
    -> Parser Context Problem ( TextCursor, Range -> TextDescription )
parseInlineAttributes options ( cursor, selection ) =
    Parser.map
        (\maybeAttrResult ->
            ( cursor
            , \range ->
                case maybeAttrResult of
                    Nothing ->
                        -- no attributes attached, so we capture as a verbatim string
                        case selection of
                            SelectString str ->
                                Styled (Text (getCurrentStyle cursor) str)

                            SelectText txt ->
                                Styled (Text (getCurrentStyle cursor) "")

                            EmptyAnnotation ->
                                Styled (Text (getCurrentStyle cursor) "")

                    Just (Err errs) ->
                        InlineBlock
                            { kind = selection
                            , record =
                                DescribeUnexpected (Id.Id "fail" [])
                                    { problem =
                                        Error.UnknownInline
                                            (List.filterMap (inlineExample selection) options.inlines)
                                    , range = range
                                    }
                            }

                    Just (Ok ( newCursor, foundFields )) ->
                        InlineBlock
                            { kind = selection
                            , record = foundFields
                            }
            )
        )
        (Parser.oneOf
            [ Parser.map Just
                (attrContainer cursor
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
            , Parser.succeed Nothing
            ]
        )


finalizeString context str =
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
                    |. Parser.backtrackable (Parser.token (Parser.Token ("\n" ++ indentationString indentation) Newline))
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


textSelection : List Replacement -> TextCursor -> Parser Context Problem ( TextCursor, InlineSelection )
textSelection replacements found =
    Parser.oneOf
        [ Parser.succeed (\str -> ( found, SelectString str ))
            |. Parser.token (Parser.Token "`" (Expecting "`"))
            |= Parser.getChompedString
                (Parser.chompWhile (\c -> c /= '`' && c /= '\n'))
            |. Parser.chompWhile (\c -> c == '`')
        , Parser.succeed identity
            |. Parser.token (Parser.Token "[" (Expecting "["))
            |= Parser.map (Tuple.mapSecond SelectText)
                (Parser.loop
                    (textCursor (getCurrentStyles found)
                        { offset = 0
                        , line = 1
                        , column = 1
                        }
                        -- we can't have annotations within annotations
                        -- and annotations are the only thing in styled text that requires an id
                        (Id.initialSeed "")
                    )
                    (simpleStyledTextTill replacements)
                )
            |. Parser.token (Parser.Token "]" (Expecting "]"))
        ]


simpleStyledTextTill :
    List Replacement
    -> TextCursor
    -> Parser Context Problem (Parser.Step TextCursor ( TextCursor, List Text ))
simpleStyledTextTill replacements cursor =
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
            |= styleTokens
        , -- chomp until a meaningful character
          Parser.chompWhile
            (\c ->
                not (List.member c ('\\' :: ']' :: '\n' :: stylingChars ++ replacementStartingChars replacements))
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
                                        ( TextCursor txt
                                        , List.reverse <| List.filterMap toText txt.found
                                        )
                                    )

                    else
                        Parser.succeed (Parser.Loop (addText new cursor))
                )
        ]


toText textDesc =
    case textDesc of
        Styled txt ->
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
attrContainer : TextCursor -> List (Block a) -> Tolerant.Parser Context Problem ( TextCursor, Description )
attrContainer (TextCursor cursor) recordBlocks =
    Tolerant.succeed identity
        |. Tolerant.try
            (Parser.chompIf (\c -> c == '{') (Expecting "{"))
        |> Tolerant.ignore (Tolerant.chompWhile (\c -> c == ' '))
        |> Tolerant.keep
            (Tolerant.oneOf (ExpectingInlineName "")
                (recordBlocks
                    |> List.map
                        (\rec ->
                            let
                                ( newSeed, recordParser ) =
                                    getParser ParseInline cursor.seed rec

                                newCursor : TextCursor
                                newCursor =
                                    TextCursor { cursor | seed = newSeed }
                            in
                            Tolerant.try (Parser.map (Tuple.pair newCursor) recordParser)
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


{-| Allow styling to start if preceding text is empty or a space

Allow styling to end if

-}
changeStyle ((TextCursor cursor) as full) ( styleToken, additional ) =
    let
        cursorText =
            case cursor.current of
                Text _ txt ->
                    txt

        newText =
            cursor.current
                |> flipStyles (styleToken :: additional)
                |> clearText
    in
    if cursorText == "" && List.isEmpty cursor.found then
        TextCursor
            { found = cursor.found
            , current = newText
            , start = cursor.start
            , seed = cursor.seed
            , balancedReplacements = cursor.balancedReplacements
            }

    else if stylingAllowed cursor styleToken then
        let
            end =
                measure cursor.start cursorText
        in
        TextCursor
            { found =
                Styled
                    cursor.current
                    :: cursor.found
            , start = end
            , current = newText
            , seed = cursor.seed
            , balancedReplacements = cursor.balancedReplacements
            }

    else
        addText (addStyleChar (styleToken :: additional) "") full


stylingAllowed cursor style =
    case cursor.current of
        Text styles txt ->
            case style of
                Italic ->
                    if styles.italic then
                        not (String.endsWith " " txt)

                    else
                        String.endsWith " " txt

                Bold ->
                    if styles.bold then
                        not (String.endsWith " " txt)

                    else
                        String.endsWith " " txt

                Strike ->
                    if styles.strike then
                        not (String.endsWith " " txt)

                    else
                        String.endsWith " " txt


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


flipStyles styles textStyle =
    case styles of
        [] ->
            textStyle

        top :: remain ->
            flipStyles remain (flipStyle top textStyle)


advanceTo target (TextCursor cursor) =
    TextCursor
        { found = cursor.found
        , current = cursor.current
        , start = target
        , seed = cursor.seed
        , balancedReplacements = cursor.balancedReplacements
        }


getCurrentStyles (TextCursor cursor) =
    getStyles cursor.current


getStyles (Text styles _) =
    styles


measure start textStr =
    let
        len : Int
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
                        cursor.current
                        :: cursor.found
                , seed = cursor.seed
                , start = end
                , current = Text styles ""
                , balancedReplacements = cursor.balancedReplacements
                }


{-| Commit text, but also trim all right whitespace as we know we don't need it.
-}
commitAndFinalizeText (TextCursor cursor) =
    case cursor.current of
        Text styles str ->
            commitText
                (TextCursor
                    { cursor | current = Text styles (String.trimRight str) }
                )


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
                        balanceCache : List String
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
            snippet : String
            snippet =
                String.slice range.start.offset range.end.offset source

            indented : String
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



-- manyOf : Int -> List (Block a) ->


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
                    (\( newSeed, foundBlock ) ->
                        Parser.Loop
                            { parsedSomething = True
                            , found = foundBlock :: cursor.found
                            , seed = newSeed
                            }
                    )

          else
            Parser.oneOf
                [ Parser.succeed
                    (\( newSeed, foundBlock ) ->
                        Parser.Loop
                            { parsedSomething = True
                            , found = foundBlock :: cursor.found
                            , seed = newSeed
                            }
                    )
                    |. Parser.token (Parser.Token (indentationString indentation) (ExpectingIndentation indentation))
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
                ( newSeed, parser ) =
                    getParserNoBar ParseBlock seed myBlock
            in
            case blockName myBlock of
                Just name ->
                    { blockNames = name :: details.blockNames
                    , childBlocks = Parser.map (Tuple.pair newSeed) parser :: details.childBlocks
                    , childValues = details.childValues
                    }

                Nothing ->
                    { blockNames = details.blockNames
                    , childBlocks = details.childBlocks
                    , childValues = Parser.map (Tuple.pair newSeed) parser :: details.childValues
                    }

        children =
            List.foldl gatherParsers2
                { blockNames = []
                , childBlocks = []
                , childValues = []
                }
                blocks

        blockParser =
            Parser.succeed identity
                |. Parser.token (Parser.Token "|>" BlockStart)
                |. Parser.chompWhile (\c -> c == ' ')
                |= Parser.oneOf
                    (List.reverse children.childBlocks
                        ++ [ withIndent
                                (\indentation ->
                                    Parser.succeed
                                        (\( pos, foundWord ) ->
                                            let
                                                ( newId, newSeed ) =
                                                    Id.step seed
                                            in
                                            ( newSeed
                                            , DescribeUnexpected newId
                                                { problem = Error.UnknownBlock children.blockNames
                                                , range = pos
                                                }
                                            )
                                        )
                                        |= withRange word
                                        |. newline
                                        |. Parser.loop "" (raggedIndentedStringAbove indentation)
                                )
                           ]
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


peek : String -> Parser c p thing -> Parser c p thing
peek name parser =
    Parser.succeed
        (\start val end src ->
            let
                _ =
                    Debug.log name
                        (String.slice start.offset end.offset src)
            in
            val
        )
        |= getPosition
        |= parser
        |= getPosition
        |= Parser.getSource


peekLoop : String -> Parser c p (Parser.Step one two) -> Parser c p (Parser.Step one two)
peekLoop name parser =
    Parser.succeed
        (\start val end src ->
            let
                loop : String
                loop =
                    case val of
                        Parser.Loop _ ->
                            " -> loop"

                        Parser.Done _ ->
                            " -> done"

                _ =
                    Debug.log (name ++ loop)
                        (String.slice start.offset end.offset src)
            in
            val
        )
        |= getPosition
        |= parser
        |= getPosition
        |= Parser.getSource


type alias TreeCursor =
    { captured : List Description

    -- stack is reverse ordered.  Things a the front are closer to leaves in the tree
    , stack :
        List
            { start : Position
            , description : Description
            }
    , previouslyAdded : PreviouslyAdded
    }


type PreviouslyAdded
    = AddedContent
    | AddedItem


{-| Parse a full nested tree in one go.
-}
fullTree :
    ParseContext
    -> Block thing
    -> ( NestedIndex, TreeCursor )
    -> Parser Context Problem (Parser.Step ( NestedIndex, TreeCursor ) (List Description))
fullTree context item ( indentation, existing ) =
    Parser.oneOf
        [ Parser.succeed (\pos -> Parser.Done (finalize existing pos))
            |= getPosition
            |. Parser.end End

        -- Whitespace Line
        , skipBlankLineWith (Parser.Loop ( indentation, existing ))
        , Parser.oneOf
            [ -- block with required indent
              parseIndentation indentation.base indentation.prev
                |> Parser.andThen
                    (parseIndentedItem context item indentation existing)

            -- We reach here because the indentation parsing was not successful,
            -- This means any issues are handled by whatever parser comes next.
            , Parser.succeed
                (\pos ->
                    Parser.Done (finalize existing pos)
                )
                |= getPosition
            ]
        ]


finalize : TreeCursor -> Position -> List Description
finalize cursor end =
    case collapseAll end cursor.stack of
        Just last ->
            List.foldl
                (\cap captured ->
                    reverseTree cap :: captured
                )
                []
                (last :: cursor.captured)

        Nothing ->
            List.foldl
                (\cap captured ->
                    reverseTree cap :: captured
                )
                []
                cursor.captured


topHasChildren stack =
    case stack of
        [] ->
            False

        top :: _ ->
            case top.description of
                DescribeItem item ->
                    case item.children of
                        [] ->
                            False

                        _ ->
                            True

                _ ->
                    False


parseIndentedItem :
    ParseContext
    -> Block thing
    -> NestedIndex
    -> TreeCursor
    -> Int
    -> Parser Context Problem (Parser.Step ( NestedIndex, TreeCursor ) (List Description))
parseIndentedItem context block indentation existing newIndent =
    let
        iconRequired : Bool
        iconRequired =
            -- icon required if the indentation is at the base
            (indentation.base == newIndent)
                -- or if indentation hasn't changed and we've already parsed child for the top of the stack
                || (indentation.prev
                        == newIndent
                        && topHasChildren existing.stack
                   )

        newSeed : Seed
        newSeed =
            if newIndent > indentation.prev then
                Id.indent indentation.seed

            else if newIndent == indentation.prev then
                -- This may look weird
                -- but this loop is started with an indented id, which should be unique.
                -- Therefore we only need to increment it after the first thing found
                case existing.stack of
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

        ( itemId, itemSeed ) =
            Id.step newSeed

        ( finalSeed, itemParser ) =
            getParser context itemSeed block

        newIndex =
            { prev = newIndent
            , seed =
                finalSeed
            , base = indentation.base
            }
    in
    -- if newIndent == indentation.prev then
    -- If indentation has not changed
    -- - With Icon ->
    --     child for top of stack
    -- - No Icon ->
    --     if no children,
    --         content for top of stack
    --     else
    --         failure
    Parser.succeed
        (\start maybeIcon item itemEnd ->
            if newIndent == indentation.prev then
                case existing.stack of
                    [] ->
                        Parser.Loop
                            ( newIndex
                            , existing
                            )

                    top :: [] ->
                        case maybeIcon of
                            -- no icon,
                            -- this is a second content block
                            Nothing ->
                                case top.description of
                                    DescribeItem topDetails ->
                                        Parser.Loop
                                            ( newIndex
                                            , { existing
                                                | previouslyAdded = AddedContent
                                                , stack =
                                                    [ { start = start
                                                      , description =
                                                            DescribeItem
                                                                { topDetails
                                                                    | content =
                                                                        item
                                                                            :: topDetails.content
                                                                }
                                                      }
                                                    ]
                                              }
                                            )

                                    _ ->
                                        Parser.Loop
                                            ( newIndex
                                            , existing
                                            )

                            Just icon ->
                                -- same indentaion as the previous one.
                                case existing.previouslyAdded of
                                    AddedItem ->
                                        -- if previous was an item, then this is a new item entirely
                                        --
                                        Parser.Loop
                                            ( newIndex
                                            , { existing
                                                | previouslyAdded = AddedItem
                                                , captured =
                                                    top.description
                                                        :: existing.captured
                                                , stack =
                                                    [ { start = start
                                                      , description =
                                                            DescribeItem
                                                                { id = itemId
                                                                , icon = icon
                                                                , content =
                                                                    [ item
                                                                    ]
                                                                , children = []
                                                                }
                                                      }
                                                    ]
                                              }
                                            )

                                    AddedContent ->
                                        -- if previous was content, then this is a new nested item
                                        --
                                        Parser.Loop
                                            ( newIndex
                                            , { existing
                                                | previouslyAdded = AddedItem
                                                , stack =
                                                    { start = start
                                                    , description =
                                                        DescribeItem
                                                            { id = itemId
                                                            , icon = icon
                                                            , content =
                                                                [ item
                                                                ]
                                                            , children = []
                                                            }
                                                    }
                                                        :: existing.stack
                                              }
                                            )

                    top :: pen :: remain ->
                        case maybeIcon of
                            Nothing ->
                                -- No Icon, add to content
                                -- Actually, this shouldn't be allowed
                                -- But when we move to allow content everywhere, then it can be allowed
                                case top.description of
                                    DescribeItem topDetails ->
                                        Parser.Loop
                                            ( newIndex
                                            , { existing
                                                | previouslyAdded = AddedContent
                                                , stack =
                                                    { start = top.start
                                                    , description =
                                                        DescribeItem
                                                            { topDetails
                                                                | content =
                                                                    item
                                                                        :: topDetails.content
                                                            }
                                                    }
                                                        :: remain
                                              }
                                            )

                                    _ ->
                                        Parser.Loop
                                            ( newIndex
                                            , existing
                                            )

                            Just icon ->
                                case existing.previouslyAdded of
                                    AddedContent ->
                                        -- "close" previous item, and add item as the new one.of
                                        -- so top gets merged into pen
                                        case pen.description of
                                            DescribeItem penDetails ->
                                                Parser.Loop
                                                    ( newIndex
                                                    , { existing
                                                        | previouslyAdded = AddedItem
                                                        , stack =
                                                            { start = start
                                                            , description =
                                                                DescribeItem
                                                                    { id = itemId
                                                                    , icon = icon
                                                                    , content =
                                                                        [ item
                                                                        ]
                                                                    , children = []
                                                                    }
                                                            }
                                                                :: top
                                                                :: pen
                                                                :: remain
                                                      }
                                                    )

                                            _ ->
                                                Parser.Loop
                                                    ( newIndex
                                                    , existing
                                                    )

                                    AddedItem ->
                                        -- Prevous thing was an item.
                                        -- means we can fold top into pen
                                        -- and add our new item as the top
                                        case pen.description of
                                            DescribeItem penDetails ->
                                                Parser.Loop
                                                    ( newIndex
                                                    , { existing
                                                        | previouslyAdded = AddedItem
                                                        , stack =
                                                            { start = start
                                                            , description =
                                                                DescribeItem
                                                                    { id = itemId
                                                                    , icon = icon
                                                                    , content =
                                                                        [ item
                                                                        ]
                                                                    , children = []
                                                                    }
                                                            }
                                                                :: { start = pen.start
                                                                   , description =
                                                                        DescribeItem
                                                                            { penDetails
                                                                                | children =
                                                                                    top.description
                                                                                        :: penDetails.children
                                                                            }
                                                                   }
                                                                -- :: top
                                                                -- :: pen
                                                                :: remain
                                                      }
                                                    )

                                            _ ->
                                                Parser.Loop
                                                    ( newIndex
                                                    , existing
                                                    )

            else if newIndent > indentation.prev then
                case existing.stack of
                    [] ->
                        Parser.Loop
                            ( newIndex
                            , existing
                            )

                    top :: remain ->
                        case top.description of
                            DescribeItem topDetails ->
                                case maybeIcon of
                                    Nothing ->
                                        -- No Icon, and indented, add to content
                                        Parser.Loop
                                            ( newIndex
                                            , { existing
                                                | previouslyAdded = AddedContent
                                                , stack =
                                                    { start = top.start
                                                    , description =
                                                        DescribeItem
                                                            { topDetails
                                                                | content =
                                                                    item
                                                                        :: topDetails.content
                                                            }
                                                    }
                                                        :: remain
                                              }
                                            )

                                    Just icon ->
                                        -- we have an icon, add to children
                                        Parser.Loop
                                            ( newIndex
                                            , { existing
                                                | previouslyAdded = AddedItem
                                                , stack =
                                                    { start = start
                                                    , description =
                                                        DescribeItem
                                                            { id = itemId
                                                            , icon = icon
                                                            , content =
                                                                [ item
                                                                ]
                                                            , children = []
                                                            }
                                                    }
                                                        :: top
                                                        :: remain
                                              }
                                            )

                            _ ->
                                Parser.Loop
                                    ( newIndex
                                    , existing
                                    )

            else
                -- Dedented
                -- multiple dedentations are allowed,
                -- so we need to see how far we need to collapse
                let
                    level : Int
                    level =
                        case existing.previouslyAdded of
                            AddedItem ->
                                -- we dedented, but previous was an item
                                -- so we need to collapse twice
                                ((indentation.prev - newIndent) // 4) + 1

                            AddedContent ->
                                -- we dedented, but previous was
                                (indentation.prev - newIndent) // 4

                    ( collapsed, maybeCaptured ) =
                        collapse start level existing.stack
                in
                Parser.Loop
                    ( newIndex
                    , { existing
                        | previouslyAdded = AddedItem
                        , stack =
                            { start = start
                            , description =
                                DescribeItem
                                    { id = itemId
                                    , icon = Maybe.withDefault Bullet maybeIcon
                                    , content =
                                        [ item
                                        ]
                                    , children = []
                                    }
                            }
                                :: collapsed
                        , captured =
                            case maybeCaptured of
                                Nothing ->
                                    existing.captured

                                Just capped ->
                                    capped :: existing.captured
                      }
                    )
        )
        |= getPosition
        |= (if iconRequired then
                Parser.map Just iconParser

            else
                Parser.oneOf
                    [ Parser.map Just iconParser
                    , Parser.succeed Nothing
                    ]
           )
        |= Parser.withIndent (newIndent + 4) itemParser
        |= getPosition


collapse :
    Position
    -> Int
    ->
        List
            { start : Position
            , description : Description
            }
    ->
        ( List
            { start : Position
            , description : Description
            }
        , Maybe Description
        )
collapse end level stack =
    if level == 0 then
        ( stack, Nothing )

    else
        case stack of
            [] ->
                ( stack, Nothing )

            top :: [] ->
                ( []
                , Just
                    top.description
                )

            top :: penultimate :: remain ->
                case penultimate.description of
                    DescribeItem pen ->
                        collapse end
                            (level - 1)
                            ({ description =
                                DescribeItem
                                    { pen
                                        | children =
                                            top.description
                                                :: pen.children
                                    }
                             , start = penultimate.start
                             }
                                :: remain
                            )

                    _ ->
                        ( stack, Nothing )


collapseAll : Position -> List { start : Position, description : Description } -> Maybe Description
collapseAll end stack =
    case stack of
        [] ->
            Nothing

        top :: [] ->
            Just top.description

        top :: penultimate :: remain ->
            case penultimate.description of
                DescribeItem item ->
                    collapseAll end
                        ({ description =
                            DescribeItem
                                { item
                                    | children =
                                        item.children
                                            ++ [ top.description ]
                                }
                         , start = penultimate.start
                         }
                            :: remain
                        )

                _ ->
                    Nothing


type RecordType
    = InlineRecord
    | BlockRecord


record :
    RecordType
    -> Id
    -> Id
    -> String
    -> Expectation
    -> List ( String, Parser Context Problem ( String, Description ) )
    -> Parser Context Problem Description
record recordType id failureId recordName expectations fields =
    Parser.succeed
        (\result ->
            case result of
                Ok details ->
                    Record
                        { id = id
                        , name = recordName
                        , found =
                            details.value
                        }

                Err err ->
                    DescribeUnexpected id
                        { range = Maybe.withDefault err.range (Tuple.first err.error)
                        , problem = Tuple.second err.error
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
                                                    (parseInlineFields failureId recordName (List.map Tuple.first fields))

                                            BlockRecord ->
                                                Parser.withIndent (indentation + 4)
                                                    (Parser.loop
                                                        { remaining = fields
                                                        , found = Ok []
                                                        }
                                                        (parseFields failureId recordName (List.map Tuple.first fields))
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
        List ( String, Parser Context Problem ( String, Description ) )
    , found :
        Result ( Maybe Range, Error.Error ) (List ( String, Description ))
    }


type Indented thing
    = Indented thing
    | WeirdIndent Int
    | EmptyLine


indentationString n =
    case n of
        0 ->
            ""

        4 ->
            "    "

        8 ->
            "        "

        12 ->
            "            "

        16 ->
            "                "

        _ ->
            -- this just allocates a bunch more than is needed
            String.repeat n " "


{-| -}
parseFields :
    Id
    -> String
    -> List String
    -> RecordFields
    -> Parser Context Problem (Parser.Step RecordFields (Result ( Maybe Range, Error.Error ) (List ( String, Description ))))
parseFields failureId recordName fieldNames fields =
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
                                    (Parser.Token (indentationString indentation)
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
                                [ indentOrSkip indentation (captureField failureId found recordName fields fieldNames)
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
            |. Parser.token (Parser.Token (indentationString indentation) (ExpectingIndentation indentation))
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
    Id
    -> List ( String, Description )
    -> String
    -> RecordFields
    -> List String
    -> Parser Context Problem (Parser.Step RecordFields a)
captureField failureId found recordName fields fieldNames =
    Parser.map
        (\maybeField ->
            case maybeField of
                Nothing ->
                    Parser.Loop fields

                Just ( fieldPos, ( foundFieldname, fieldValue ) ) ->
                    Parser.Loop
                        { found = Ok (( foundFieldname, fieldValue ) :: found)
                        , remaining =
                            List.filter
                                (\( fieldParserName, _ ) -> fieldParserName /= foundFieldname)
                                fields.remaining
                        }
        )
        (Parser.oneOf
            (List.map (Parser.map Just << parseField) fields.remaining
                ++ [ Parser.map Just (unexpectedField failureId recordName fieldNames)
                   ]
            )
        )


{-| Parse a field name
-}
parseField : ( String, Parser Context Problem ( String, Description ) ) -> Parser Context Problem ( Position, ( String, Description ) )
parseField ( name, contentParser ) =
    Parser.succeed Tuple.pair
        |= getPosition
        |. Parser.keyword (Parser.Token name (ExpectingFieldName name))
        |. Parser.chompWhile (\c -> c == ' ')
        |. Parser.chompIf (\c -> c == '=') (Expecting "=")
        |. Parser.chompWhile (\c -> c == ' ')
        |= contentParser


unexpectedField : Id -> String -> List String -> Parser Context Problem ( Position, ( String, Description ) )
unexpectedField id recordName options =
    withIndent
        (\indentation ->
            Parser.map
                (\{ range, value } ->
                    ( range.start
                    , ( value
                      , DescribeUnexpected id
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
    Id
    -> String
    -> List String
    -> RecordFields
    -> Parser Context Problem (Parser.Step RecordFields (Result ( Maybe Range, Error.Error ) (List ( String, Description ))))
parseInlineFields failureId recordName fieldNames fields =
    let
        hasMore : Bool
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
                            |= captureField failureId found recordName fields fieldNames
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
parseIndentation : Int -> Int -> Parser Context Problem Int
parseIndentation base previous =
    Parser.succeed Tuple.pair
        |= Parser.oneOf
            ([ Parser.succeed (previous + 4)
                |. Parser.token (Parser.Token (indentationString (previous + 4)) (ExpectingIndentation (previous + 4)))
             , Parser.succeed previous
                |. Parser.token (Parser.Token (indentationString previous) (ExpectingIndentation previous))
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


iconParser : Parser c Problem Icon
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
                        |. Parser.token (Parser.Token (indentationString level) (ExpectingIndentation level))
                )
                (List.range 0 (((prev - 4) - base) // 4))
            )


reverseTree : Description -> Description
reverseTree nest =
    case nest of
        DescribeItem item ->
            DescribeItem
                { id = item.id
                , icon = item.icon
                , content = List.reverse item.content
                , children =
                    List.foldl
                        (\cap captured ->
                            reverseTree cap :: captured
                        )
                        []
                        item.children
                }

        _ ->
            nest
