module Internal.Model exposing
    ( Block(..)
    , Inline(..)
    , Options
    , markup
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
type alias Options model styling msg =
    { styling : model -> styling
    , blocks : List (Block model styling msg)
    , inlines : List (Inline model styling msg)
    }


{-| -}
type Block model style msg
    = Block String (Parser (style -> model -> Element msg))
    | Parse String (Parser (style -> model -> List (Element msg)) -> Parser (style -> model -> Element msg))


{-| -}
type Inline model style msg
    = Inline String (String -> style -> model -> List (Element msg))


{-| -}
markup :
    Options model
        { styles
            | link : List (Element.Attribute msg)
            , token : List (Element.Attribute msg)
            , root : List (Element.Attribute msg)
            , block : List (Element.Attribute msg)
        }
        msg
    ->
        Parser
            (model
             -> Element msg
            )
markup options =
    Parser.map
        (\view ->
            \model ->
                view (options.styling model) model
        )
        (Parser.loop []
            (blocks options)
        )


blocks options existing =
    let
        done _ =
            Parser.Done
                (\styling model ->
                    Element.textColumn styling.root
                        (List.foldl
                            (\fn els ->
                                fn styling model :: els
                            )
                            []
                            existing
                        )
                )
    in
    Parser.oneOf
        [ Parser.end
            |> Parser.map
                (\_ ->
                    done ()
                )
        , Parser.token "\n"
            |> Parser.map
                (\_ ->
                    Parser.Loop
                        existing
                )

        -- custom blocks
        , Parser.succeed identity
            |. Parser.token "|"
            |. Parser.chompIf (\c -> c == ' ')
            |= Parser.oneOf
                (List.map
                    (blockToParser options.inlines)
                    options.blocks
                )
            |> Parser.andThen identity
            |> Parser.map
                (\found ->
                    Parser.Loop
                        (found :: existing)
                )

        -- inlines
        , inline options.inlines
            |> Parser.map
                (\found ->
                    Parser.Loop
                        ((\styling model ->
                            Element.paragraph []
                                (found styling model)
                         )
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


blockToParser inlines blockable =
    case blockable of
        Block name arguments ->
            Parser.keyword name
                |> Parser.map
                    (\_ ->
                        Parser.succeed identity
                            |= arguments
                    )

        Parse name parser ->
            Parser.keyword name
                |> Parser.map
                    (\_ ->
                        parser (inline inlines)
                    )



{- Formatted Text -}


type InlineStyle
    = NoStyleChange
    | Bold
    | Italic
    | Strike
    | Underline
    | Token


type Text styling model msg
    = Text
        -- Accumulator string
        String
        -- A bitfield of styles that are active
        Flag.Field
        -- Accumulator of element constructors
        (List (styling -> model -> List (Element msg)))


emptyText =
    Text "" Flag.none []


inline :
    List
        (Inline model
            { a
                | link : List (Element.Attribute msg)
                , token : List (Element.Attribute msg)
            }
            msg
        )
    ->
        Parser
            ({ a
                | link : List (Element.Attribute msg)
                , token : List (Element.Attribute msg)
             }
             -> model
             -> List (Element msg)
            )
inline inlines =
    Parser.loop emptyText (inlineLoop inlines)



-- type alias MinimumStyle


inlineLoop :
    List
        (Inline model
            { a
                | link : List (Element.Attribute msg)
                , token : List (Element.Attribute msg)
            }
            msg
        )
    ->
        Text
            { a
                | link : List (Element.Attribute msg)
                , token : List (Element.Attribute msg)
            }
            model
            msg
    ->
        Parser
            (Parser.Step
                (Text
                    { a
                        | link : List (Element.Attribute msg)
                        , token : List (Element.Attribute msg)
                    }
                    model
                    msg
                )
                ({ a
                    | link : List (Element.Attribute msg)
                    , token : List (Element.Attribute msg)
                 }
                 -> model
                 -> List (Element msg)
                )
            )
inlineLoop inlines existing =
    let
        (Text _ styles _) =
            existing
    in
    Parser.oneOf
        [ Parser.oneOf (typography existing styles)

        -- Custom inline block
        , Parser.succeed
            (\fn txt ->
                existing
                    |> addElement
                        (fn txt)
                    |> Parser.Loop
            )
            |. Parser.token "{"
            |. Parser.chompWhile (\c -> c == ' ')
            |= Parser.oneOf
                (List.map inlineToParser inlines)
            |. Parser.chompWhile (\c -> c == ' ')
            |. Parser.token "|"
            |= typographyText styles [ '}' ]
            |. Parser.token "}"

        -- Code/Token inline
        , Parser.succeed
            (\tokenText ->
                case changeStyle existing NoStyleChange of
                    new ->
                        new
                            |> addElement
                                (\styling _ ->
                                    [ Element.el
                                        styling.token
                                        (Element.text tokenText)
                                    ]
                                )
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
                changeStyle existing NoStyleChange
                    |> addElement
                        (renderLink
                            (if asToken == "" then
                                styles

                             else
                                Flag.add Flag.token styles
                            )
                            txt
                            url
                        )
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
            (Parser.Loop << changeStyle existing)
            |= Parser.oneOf
                [ Parser.map (always Italic) (Parser.token "/")
                , Parser.map (always Strike) (Parser.token "~")
                , Parser.map (always Bold) (Parser.token "*")
                ]

        -- end on newline
        , Parser.token "\n"
            |> Parser.map
                (\_ ->
                    finalize existing
                )

        -- chomp until a meaningful character
        , Parser.getChompedString
            (Parser.chompWhile (notReservedInline styles))
            |> Parser.map
                (\new ->
                    if new == "" then
                        finalize existing

                    else
                        existing
                            |> addText new
                            |> Parser.Loop
                )
        ]


finalize (Text txt styles els) =
    if els == [] && txt == "" then
        Parser.Done (\styling model -> [])

    else if txt == "" then
        Parser.Done <|
            \styling model ->
                List.foldl
                    (\fn elems ->
                        fn styling model ++ elems
                    )
                    []
                    els

    else
        Parser.Done <|
            \styling model ->
                List.foldl
                    (\fn elems ->
                        fn styling model ++ elems
                    )
                    []
                    (renderText styles txt :: els)


typographyText styles until =
    let
        done found =
            case found of
                Text txt _ _ ->
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

    -> "<>" -> a non-breaking space.
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
    , Parser.succeed
        (\_ ->
            existing
                |> addText "\u{00A0}"
                |> Parser.Loop
        )
        |. Parser.token "<>"
        |= Parser.chompWhile (\c -> c == '<' || c == ' ' || c == '>')

    -- capture the opening character if it wasn't chomped previously
    , Parser.token "<"
        |> Parser.map
            (\_ ->
                existing
                    |> addText "<"
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
    , '<'
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
        (List.member char allControl)


addText newTxt (Text txt styles els) =
    Text (txt ++ newTxt) styles els


{-| If we accumulating a link style, accumulate it there.
-}
addElement newElements (Text txt styles els) =
    Text txt styles (newElements :: els)


mapStyle fn (Text txt styles els) =
    Text txt (fn styles) els



{- Inline -}


changeStyle (Text txt styles els) styleToken =
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
    in
    if txt == "" then
        Text "" newStyles els

    else
        Text "" newStyles els
            |> addElement (renderText styles txt)


toStyles styleField styling =
    List.concat
        [ if Flag.present Flag.bold styleField then
            [ Font.bold ]

          else
            []
        , if Flag.present Flag.italic styleField then
            [ Font.italic ]

          else
            []
        , if
            Flag.present Flag.strike styleField
                && Flag.present Flag.underline styleField
          then
            [ Font.underline ]

          else
            []
        , if Flag.present Flag.strike styleField then
            [ Font.strike ]

          else
            []
        , if Flag.present Flag.token styleField then
            styling.token

          else
            []
        ]


renderLink styleField txt url styling _ =
    [ Element.link (styling.link ++ toStyles styleField styling)
        { url = url
        , label = Element.text txt
        }
    ]


renderText styleField txt styling _ =
    if Flag.none == styleField then
        [ Element.text txt ]

    else
        [ Element.el (toStyles styleField styling) (Element.text txt) ]


doubleQuote =
    '"'
