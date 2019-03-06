module Mark.Internal.Parser exposing
    ( Replacement(..)
    , attribute
    , attributeList
    , getPosition
    , peek
    , styledText
    , withRange
    , word
    )

{-| -}

import Mark.Internal.Description exposing (..)
import Mark.Internal.Error as Error exposing (Context(..), Problem(..))
import Mark.Internal.Id exposing (..)
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


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



{- TEXT PARSING -}


{-| -}
type TextCursor
    = TextCursor
        { current : Text
        , start : Position
        , found : List TextDescription
        , balancedReplacements : List String
        }


type SimpleTextCursor
    = SimpleTextCursor
        { current : Text
        , start : Position
        , text : List Text
        , balancedReplacements : List String
        }


mapTextCursor fn (TextCursor curs) =
    TextCursor (fn curs)


{-| -}
type Replacement
    = Replacement String String
    | Balanced
        { start : ( String, String )
        , end : ( String, String )
        }


empty : Text
empty =
    Text [] ""


textCursor inheritedStyles startingPos =
    TextCursor
        { current = Text inheritedStyles ""
        , found = []
        , start = startingPos
        , balancedReplacements = []
        }


styledText :
    { inlines : List InlineExpectation
    , replacements : List Replacement
    }
    -> Position
    -> List Style
    -> List Char
    -> Parser Context Problem Description
styledText options startingPos inheritedStyles until =
    let
        vacantText =
            textCursor inheritedStyles startingPos

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
                DescribeText
                    { id = Id pos
                    , text = textNodes
                    }
            )
            (withRange
                (Parser.loop vacantText
                    (styledTextLoop options meaningful untilStrings)
                )
            )
        ]


{-| -}
styledTextLoop :
    { inlines : List InlineExpectation
    , replacements : List Replacement
    }
    -> List Char
    -> List String
    -> TextCursor
    -> Parser Context Problem (Parser.Step TextCursor (List TextDescription))
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
            (Parser.Loop << changeStyle found)
            |= Parser.oneOf
                [ Parser.map (always (Just Italic)) (Parser.token (Parser.Token "/" (Expecting "/")))
                , Parser.map (always (Just Strike)) (Parser.token (Parser.Token "~" (Expecting "~")))
                , Parser.map (always (Just Bold)) (Parser.token (Parser.Token "*" (Expecting "*")))
                ]

        -- Custom inline block
        , inlineToken options found
        , inlineAnnotation options found
        , -- chomp until a meaningful character
          Parser.chompWhile
            (\c ->
                not (List.member c meaningful)
            )
            |> Parser.getChompedString
            |> Parser.andThen
                (\new ->
                    if new == "" || new == "\n" then
                        case changeStyle found Nothing of
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


inlineAnnotation options found =
    (Parser.succeed identity
        |= getPosition
        |. Parser.token (Parser.Token "[" InlineStart)
    )
        |> Parser.andThen
            (\startPos ->
                Parser.succeed
                    (\( end, new, maybeTextCursor ) ->
                        let
                            current =
                                case changeStyle found Nothing of
                                    TextCursor accum ->
                                        accum
                        in
                        case maybeTextCursor of
                            Nothing ->
                                Parser.Loop
                                    (TextCursor
                                        { found = new :: current.found
                                        , start = end
                                        , current = empty
                                        , balancedReplacements = current.balancedReplacements
                                        }
                                    )

                            Just (TextCursor curs) ->
                                Parser.Loop
                                    (TextCursor
                                        { found = new :: current.found
                                        , start = end
                                        , current =
                                            case curs.current of
                                                Text styles _ ->
                                                    Text styles ""
                                        , balancedReplacements = curs.balancedReplacements
                                        }
                                    )
                    )
                    |= Parser.oneOf
                        [ Parser.succeed
                            (\( text, cursor ) hasEnd attrs end ->
                                if hasEnd then
                                    ( end
                                    , InlineAnnotation
                                        { text = text
                                        , range =
                                            { start = startPos
                                            , end = end
                                            }
                                        , attributes = attrs
                                        }
                                    , Just cursor
                                    )

                                else
                                    ( end
                                    , UnexpectedInline
                                        { range =
                                            { start = startPos
                                            , end = end
                                            }
                                        , problem =
                                            -- TODO: This is the wrong error
                                            -- It could be:
                                            --   unexpected attributes
                                            --   missing control characters
                                            Error.UnknownInline []
                                        }
                                    , Nothing
                                    )
                            )
                            |= Parser.loop
                                (textCursor [] startPos)
                                (simpleStyledTextTill [ '\n', ']' ] options.replacements)
                            |= Parser.oneOf
                                [ Parser.map (always True) (Parser.token (Parser.Token "]" InlineEnd))
                                , Parser.succeed False
                                    |. parseTillEnd
                                ]
                            |= parseAndMatchAttributes (List.filterMap onlyAnnotations options.inlines)
                            |= getPosition
                        ]
            )


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
                [ Parser.map (always (Just Italic)) (Parser.token (Parser.Token "/" (Expecting "/")))
                , Parser.map (always (Just Strike)) (Parser.token (Parser.Token "~" (Expecting "~")))
                , Parser.map (always (Just Bold)) (Parser.token (Parser.Token "*" (Expecting "*")))
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
                        case changeStyle cursor Nothing of
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


parseAndMatchAttributes attrSets =
    Parser.succeed []


inlineToken options found =
    Parser.succeed
        (\start maybeRendered end ->
            let
                current =
                    case changeStyle found Nothing of
                        TextCursor accum ->
                            accum
            in
            Parser.Loop
                (TextCursor
                    { found =
                        case maybeRendered of
                            Nothing ->
                                UnexpectedInline
                                    { range =
                                        { start = start
                                        , end = end
                                        }
                                    , problem =
                                        Error.UnknownInline []

                                    -- TODO: FIX THIS
                                    --(List.map getInlineName options.inlines)
                                    }
                                    :: current.found

                            Just rendered ->
                                rendered :: current.found
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
        |. Parser.chompWhile (\c -> c == ' ')
        |= Parser.oneOf
            [ Parser.succeed
                (\maybeInlineResult hasEnd ->
                    if hasEnd then
                        maybeInlineResult

                    else
                        Nothing
                )
                |= Parser.oneOf
                    (List.map
                        parseTokenBody
                        (List.filterMap onlyTokens options.inlines)
                    )
                |. Parser.chompWhile (\c -> c == ' ')
                |= Parser.oneOf
                    [ Parser.map (always True) (Parser.token (Parser.Token "}" InlineEnd))
                    , Parser.succeed False
                        |. parseTillEnd
                    ]
            , Parser.succeed Nothing
                |. parseTillEnd
            ]
        |= getPosition


parseTokenBody ( name, attrs ) =
    case attrs of
        [] ->
            Parser.succeed
                (\( range, _ ) ->
                    Just (InlineToken { name = name, range = range, attributes = [] })
                )
                |= withRange (Parser.keyword (Parser.Token name (ExpectingInlineName name)))

        _ ->
            Parser.succeed
                (\( range, maybeFoundComponents ) ->
                    case maybeFoundComponents of
                        Nothing ->
                            Nothing

                        Just attributes ->
                            Just
                                (InlineToken
                                    { name = name
                                    , range = range
                                    , attributes = attributes
                                    }
                                )
                )
                |= withRange
                    (Parser.succeed identity
                        |. Parser.keyword (Parser.Token name (ExpectingInlineName name))
                        |. Parser.chompWhile (\c -> c == ' ')
                        |. Parser.chompIf (\c -> c == '|') (Expecting "|")
                        |. Parser.chompWhile (\c -> c == ' ')
                        |= Parser.loop ( attrs, [] ) attributeList
                    )


{-| Parse a set of attributes.

They can be parsed in any order.

-}
attributeList :
    ( List AttrExpectation, List InlineAttribute )
    -> Parser Context Problem (Parser.Step ( List AttrExpectation, List InlineAttribute ) (Maybe (List InlineAttribute)))
attributeList ( attrExpectations, found ) =
    case attrExpectations of
        [] ->
            Parser.succeed (Parser.Done (Just (List.reverse found)))

        _ ->
            let
                parseAttr i expectation =
                    Parser.succeed
                        (\attr ->
                            Parser.Loop
                                ( removeByIndex i attrExpectations
                                , attr :: found
                                )
                        )
                        |= attribute expectation
                        |. (if List.length attrExpectations > 1 then
                                Parser.succeed ()
                                    |. Parser.chompIf (\c -> c == ',') (Expecting ",")
                                    |. Parser.chompWhile (\c -> c == ' ')

                            else
                                Parser.succeed ()
                           )
            in
            Parser.oneOf
                (List.indexedMap parseAttr attrExpectations
                    ++ [ Parser.map (always (Parser.Done Nothing)) parseTillEnd ]
                )


attribute : AttrExpectation -> Parser Context Problem InlineAttribute
attribute attr =
    case attr of
        ExpectAttrString inlineName ->
            Parser.succeed
                (\start equals str end ->
                    AttrString
                        { name = inlineName
                        , range =
                            { start = start
                            , end = end
                            }
                        , value = str
                        }
                )
                |= getPosition
                |. Parser.keyword
                    (Parser.Token
                        inlineName
                        (ExpectingFieldName inlineName)
                    )
                |. Parser.chompWhile (\c -> c == ' ')
                |= Parser.oneOf
                    [ Parser.map (always True) (Parser.chompIf (\c -> c == '=') (Expecting "="))
                    , Parser.succeed False
                    ]
                |= Parser.getChompedString
                    (Parser.chompWhile (\c -> c /= '|' && c /= '}' && c /= '\n' && c /= ','))
                |= getPosition



{- Style Helpers -}


changeStyle (TextCursor cursor) maybeStyleToken =
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


flipStyle newStyle textStyle =
    case textStyle of
        Text styles str ->
            if List.member newStyle styles then
                Text (List.filter ((/=) newStyle) styles) ""

            else
                Text (newStyle :: styles) ""


measure start textStr =
    let
        len =
            String.length textStr
    in
    { start
        | offset = start.offset + len
        , column = start.column + len
    }



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


stylingChars =
    [ '~'
    , '['
    , '/'
    , '*'
    , '\n'
    , '{'
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


withRange : Parser Context Problem thing -> Parser Context Problem ( Range, thing )
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


peek : String -> Parser c p thing -> Parser c p thing
peek name parser =
    Parser.succeed
        (\start val end src ->
            let
                highlightParsed =
                    String.repeat (start.column - 1) " " ++ String.repeat (max 0 (end.column - start.column)) "^"

                fullLine =
                    String.slice (max 0 (start.offset - start.column)) end.offset src

                _ =
                    Debug.log name
                        -- fullLine
                        (String.slice start.offset end.offset src)

                -- _ =
                --     Debug.log name
                --         highlightParsed
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


parseTillEnd =
    Parser.succeed
        (\str endsWithBracket ->
            endsWithBracket
        )
        |= Parser.chompWhile (\c -> c /= '\n' && c /= '}')
        |= Parser.oneOf
            [ Parser.map (always True) (Parser.token (Parser.Token "}" InlineEnd))
            , Parser.succeed False
            ]



{- MISC HELPERS -}


onlyTokens inline =
    case inline of
        ExpectAnnotation attrs ->
            Nothing

        ExpectToken name attrs ->
            Just ( name, attrs )


onlyAnnotations inline =
    case inline of
        ExpectAnnotation attrs ->
            Just attrs

        ExpectToken name attrs ->
            Nothing


removeByIndex index list =
    List.foldl
        (\item ( cursor, passed ) ->
            if cursor == index then
                ( cursor + 1, passed )

            else
                ( cursor + 1, item :: passed )
        )
        ( 0, [] )
        list
        |> Tuple.second
        |> List.reverse
