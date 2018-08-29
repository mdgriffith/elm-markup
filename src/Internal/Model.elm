module Internal.Model exposing
    ( Block(..)
    , Inline(..)
    , Options
    , markup
    , styledText
    , text
    )

{-| -}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region
import Internal.Flag as Flag
import Parser exposing ((|.), (|=), Parser)


{-| -}
type alias Options styling msg =
    { styling : styling
    , blocks : List (Block styling msg)
    , inlines : List (Inline styling msg)
    }


{-| -}
type Block style msg
    = Block String (Parser (style -> Element msg))
    | Parse String (Options style msg -> Parser (style -> Element msg))


{-| -}
type Inline style msg
    = Inline String (String -> style -> List (Element msg))


markup :
    Options
        { styles
            | link : List (Element.Attribute msg)
            , token : List (Element.Attribute msg)
            , root : List (Element.Attribute msg)
            , block : List (Element.Attribute msg)
        }
        msg
    -> Parser (Element msg)
markup options =
    Parser.loop []
        (blocks options)


blocks options existing =
    Parser.oneOf
        [ Parser.end
            |> Parser.map
                (\_ ->
                    Parser.Done
                        (Element.textColumn options.styling.root (List.reverse existing))
                )
        , Parser.token "\n"
            |> Parser.map
                (\_ ->
                    Parser.Loop
                        existing
                )
        , Parser.succeed identity
            |. Parser.token "|"
            |. Parser.chompIf (\c -> c == ' ')
            |= Parser.oneOf
                (List.map
                    (blockToParser options)
                    options.blocks
                )
            |> Parser.andThen identity
            |> Parser.map
                (\found ->
                    Parser.Loop
                        (found :: existing)
                )
        , text options
            |> Parser.map
                (\found ->
                    if found == [] then
                        Parser.Done
                            (Element.textColumn options.styling.root
                                (List.reverse existing)
                            )

                    else
                        Parser.Loop
                            (Element.paragraph []
                                found
                                :: existing
                            )
                )
        ]



{- Custom Blocks -}


inlineToParser blockable =
    case blockable of
        Inline name fn ->
            Parser.keyword name
                |> Parser.map
                    (always fn)


blockToParser options blockable =
    case blockable of
        Block name arguments ->
            Parser.keyword name
                |> Parser.map
                    (\_ ->
                        Parser.succeed
                            (\fn ->
                                fn options.styling
                            )
                            |= arguments
                    )

        Parse name parser ->
            Parser.keyword name
                |> Parser.map
                    (\_ ->
                        parser options
                            |> Parser.map
                                (\fn ->
                                    fn options.styling
                                )
                    )


type StyledText msg
    = StyledText
        -- Accumulator string
        String
        -- Keep track of styles that are active
        Flag.Field
        (List (Element msg))
        --
        (Maybe (List (Element msg)))



{- Formatted Text -}


type InlineStyle
    = NoStyleChange
    | Bold
    | Italic
    | Strike
    | Underline
    | Token
    | LinkStart
    | Link String


emptyText =
    StyledText "" Flag.none [] Nothing


text :
    Options
        { styles
            | link : List (Element.Attribute msg)
            , token : List (Element.Attribute msg)
            , root : List (Element.Attribute msg)
            , block : List (Element.Attribute msg)
        }
        msg
    -> Parser (List (Element msg))
text options =
    Parser.loop emptyText (styledText options)


styledText :
    Options
        { styles
            | link : List (Element.Attribute msg)
            , token : List (Element.Attribute msg)
            , root : List (Element.Attribute msg)
            , block : List (Element.Attribute msg)
        }
        msg
    -> StyledText msg
    -> Parser (Parser.Step (StyledText msg) (List (Element msg)))
styledText options existing =
    let
        (StyledText _ styles _ _) =
            existing
    in
    Parser.oneOf
        [ Parser.oneOf (typography existing styles)

        -- Custom inline block
        , Parser.succeed
            (\fn txt ->
                existing
                    |> addElements (List.reverse (fn txt options.styling))
                    |> Parser.Loop
            )
            |. Parser.token "{"
            |. Parser.chompWhile (\c -> c == ' ')
            |= Parser.oneOf
                (List.map inlineToParser options.inlines)
            |. Parser.chompWhile (\c -> c == ' ')
            |. Parser.token "|"
            |= typographyText styles [ '}' ]
            |. Parser.token "}"

        -- Code/Token inline
        , Parser.succeed
            (\tokenText ->
                case changeStyle options.styling existing NoStyleChange of
                    new ->
                        new
                            |> addElements
                                [ Element.el
                                    options.styling.token
                                    (Element.text tokenText)
                                ]
                            |> Parser.Loop
            )
            -- consume at least one grave accent
            |. Parser.token "`"
            |= Parser.getChompedString
                (Parser.chompWhile (\c -> c /= '`'))
            |. Parser.token "`"

        -- Link
        , Parser.succeed
            (\asToken txt url ->
                changeStyle options.styling existing NoStyleChange
                    |> addElements
                        [ renderStyledText options.styling
                            (if asToken == "" then
                                styles

                             else
                                Flag.add Flag.token styles
                            )
                            txt
                            (Just url)
                        ]
                    |> Parser.Loop
            )
            |. Parser.token "["
            |= Parser.getChompedString
                (Parser.chompWhile (\c -> c == '`'))
            |= typographyText styles [ ']', '`' ]
            |. Parser.chompWhile (\c -> c == '`')
            |. Parser.token "]"
            |. Parser.token "("
            |= Parser.getChompedString
                (Parser.chompWhile (\c -> c /= ')' && c /= '\n' && c /= ' '))
            |. Parser.token ")"

        -- Capture styling
        , Parser.succeed
            (Parser.Loop << changeStyle options.styling existing)
            |= Parser.oneOf
                [ Parser.map (always Italic) (Parser.token "/")
                , Parser.map (always Strike) (Parser.token "~")
                , Parser.map (always Bold) (Parser.token "*")
                ]
        , Parser.token "\n"
            |> Parser.map
                (\_ ->
                    finalize options.styling existing
                )
        , Parser.getChompedString
            (Parser.chompWhile (notReservedInline styles))
            |> Parser.map
                (\new ->
                    if new == "" then
                        finalize options.styling existing

                    else
                        existing
                            |> addText new
                            |> Parser.Loop
                )
        ]


typographyText styles until =
    let
        done found =
            case found of
                StyledText txt _ _ _ ->
                    Parser.Done txt
    in
    Parser.loop emptyText
        (\found ->
            Parser.oneOf
                [ Parser.oneOf (typography found styles)
                , Parser.getChompedString
                    (Parser.chompWhile
                        (\c ->
                            c
                                /= '\n'
                                && not (List.member c until)
                                && not (List.member c typographyChars)
                        )
                    )
                    |> Parser.map
                        (\new ->
                            if new == "" then
                                done found

                            else
                                found
                                    |> addText new
                                    |> Parser.Loop
                        )
                , Parser.oneOf
                    (List.map
                        (\c ->
                            Parser.token (String.fromChar c)
                                |> Parser.map (always (done found))
                        )
                        until
                    )
                ]
        )


{-| **Reclaimed typography**

This function will replace certain characters with improved typographical ones.
Escaping a character will skip the replacement.

    -> "+" -> a non-breaking space.
        - This can be used to glue words together so that they don't break
        - It also avoids being used for spacing like `&nbsp;` because multiple instances will collapse down to one.
    -> "--" -> "en-dash"
    -> "---" -> "em-dash".
    -> Quotation marks will be replaced with curly quotes.
    -> "..." -> ellipses

-}
typography existing styles =
    [ -- Escaped characters are captured as-is
      Parser.succeed
        (\escaped ->
            existing
                |> addText escaped
                |> Parser.Loop
        )
        |. Parser.token "\\"
        |= Parser.getChompedString
            (Parser.chompIf (always True))

    -- Non breaking space
    , Parser.token "+"
        |. Parser.chompWhile (\c -> c == '+' || c == ' ')
        |> Parser.map
            (\_ ->
                existing
                    |> addText "\u{00A0}"
                    |> Parser.Loop
            )

    -- replace ellipses
    , Parser.succeed
        (\dots ->
            if
                String.length dots
                    == 2
                    && not (Flag.present Flag.token styles)
            then
                existing
                    |> addText "…"
                    |> Parser.Loop

            else
                existing
                    |> addText ("." ++ dots)
                    |> Parser.Loop
        )
        -- consume at least one dot
        |. Parser.token "."
        |= Parser.getChompedString
            (Parser.chompWhile (\c -> c == '.'))

    -- replace dashes with en or emdash
    , Parser.succeed
        (\dashes ->
            if Flag.present Flag.token styles then
                existing
                    |> addText ("-" ++ dashes)
                    |> Parser.Loop

            else if String.length dashes == 1 then
                existing
                    |> addText "–"
                    |> Parser.Loop

            else if String.length dashes == 2 then
                existing
                    |> addText "—"
                    |> Parser.Loop

            else
                existing
                    |> addText ("-" ++ dashes)
                    |> Parser.Loop
        )
        -- consume at least one dash
        |. Parser.token "-"
        |= Parser.getChompedString
            (Parser.chompWhile (\c -> c == '-'))

    -- Auto curly quotes
    , Parser.token "\""
        |> Parser.map
            (\_ ->
                if Flag.present Flag.token styles then
                    existing
                        |> addText "\""
                        |> Parser.Loop

                else if Flag.present Flag.doubleQuote styles then
                    existing
                        |> addText "”"
                        |> mapStyle (Flag.flip Flag.doubleQuote)
                        |> Parser.Loop

                else
                    existing
                        |> addText "“"
                        |> mapStyle (Flag.flip Flag.doubleQuote)
                        |> Parser.Loop
            )

    -- replace appostrophe
    , Parser.token "'"
        |> Parser.map
            (\_ ->
                existing
                    |> addText "’"
                    |> Parser.Loop
            )
    ]


typographyChars =
    [ doubleQuote
    , '\''
    , '.'
    , '\\'
    , '/'
    , '-'
    , '+'
    ]


stylingChars =
    [ '~'
    , '/'
    , '*'
    , '['
    , '\n'
    , '{'
    , '`'
    ]


allControl =
    typographyChars ++ stylingChars


notReservedInline styles char =
    not
        (List.member char allControl
         -- [ '-'
         -- , '~'
         -- , '\\'
         -- , '/'
         -- , '*'
         -- , '['
         -- , ']'
         -- , '\n'
         -- , '`'
         -- , '\''
         -- , '.'
         -- -- , '<'
         -- , '{'
         -- -- , '}'
         -- , doubleQuote
         -- -- , if Flag.present Flag.token styles then
         -- --     '}'
         -- --   else
         -- --     '{'
         -- ]
        )


finalize styling (StyledText txt styles els linkEls) =
    if els == [] && txt == "" then
        Parser.Done []

    else if txt == "" then
        Parser.Done <|
            List.reverse
                els

    else
        Parser.Done <|
            List.reverse
                (renderStyledText styling styles txt Nothing :: els)


addText newTxt (StyledText txt styles els linkEls) =
    StyledText (txt ++ newTxt)
        styles
        els
        linkEls


addElements newElements (StyledText txt styles els maybeLinkEls) =
    case maybeLinkEls of
        Nothing ->
            StyledText txt
                styles
                (newElements ++ els)
                maybeLinkEls

        Just linkEls ->
            StyledText txt
                styles
                els
                (Just
                    (newElements ++ linkEls)
                )


mapStyle fn (StyledText txt styles els linkEls) =
    StyledText txt
        (fn styles)
        els
        linkEls


startLink styleOptions styled =
    case changeStyle styleOptions styled NoStyleChange of
        StyledText txt styles els _ ->
            StyledText txt (Flag.add Flag.link styles) els (Just [])


captureLink styleOptions url styled =
    case changeStyle styleOptions styled NoStyleChange of
        StyledText txt styles els maybeLinkElements ->
            StyledText txt
                (Flag.remove Flag.link styles)
                (els
                    ++ (case maybeLinkElements of
                            Nothing ->
                                []

                            Just linkEls ->
                                [ Element.link
                                    styleOptions.link
                                    { url = url
                                    , label =
                                        Element.paragraph
                                            [ Element.width Element.shrink ]
                                            linkEls
                                    }
                                ]
                       )
                )
                Nothing



{- Inline -}


changeStyle styling (StyledText txt styles els linkEls) styleToken =
    let
        newStyles =
            case styleToken of
                NoStyleChange ->
                    styles

                Bold ->
                    Flag.flip Flag.bold styles

                Italic ->
                    Flag.flip Flag.italic styles

                Strike ->
                    Flag.flip Flag.strike styles

                Underline ->
                    Flag.flip Flag.underline styles

                Token ->
                    Flag.flip Flag.token styles

                LinkStart ->
                    Flag.add Flag.link styles

                Link _ ->
                    Flag.remove Flag.link styles

        link =
            case styleToken of
                Link to ->
                    Just to

                _ ->
                    Nothing
    in
    if txt == "" then
        StyledText "" newStyles els linkEls

    else
        StyledText "" newStyles els linkEls
            |> addElements [ renderStyledText styling styles txt link ]


renderStyledText styleOptions styled txt maybeLink =
    let
        styles =
            List.concat
                [ if Flag.present Flag.bold styled then
                    [ Font.bold ]

                  else
                    []
                , if Flag.present Flag.italic styled then
                    [ Font.italic ]

                  else
                    []
                , if
                    Flag.present Flag.strike styled
                        && Flag.present Flag.underline styled
                  then
                    [ Font.underline ]

                  else
                    []
                , if Flag.present Flag.strike styled then
                    [ Font.strike ]

                  else
                    []
                , if Flag.present Flag.token styled then
                    styleOptions.token

                  else
                    []
                ]
    in
    case maybeLink of
        Nothing ->
            if List.isEmpty styles && not (Flag.present Flag.token styled) then
                Element.text txt

            else
                Element.el
                    -- (if Flag.present Flag.token styled then
                    --     styleOptions.token ++ styles
                    --  else
                    styles
                    -- )
                    (Element.text txt)

        Just link ->
            Element.link
                (styleOptions.link ++ styles)
                { url = link
                , label = Element.text txt
                }


doubleQuote =
    '"'
