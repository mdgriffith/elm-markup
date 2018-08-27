module Internal.Model exposing
    ( Block(..)
    , InlineStyle(..)
    , Options
    , StyledText(..)
    , blockToParser
    , blocks
    , changeStyle
    , customBlock
    , doubleQuote
    , finalize
    , markup
    , notReservedInline
    , renderStyledText
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



{-

   -- Side Notes

       -> Tokens will ignore styling that is started or resolved in the token block
           -> This is to avoid clashes with things tha are likely embedded code.
           -> They will still inherit stying if it's within the space.




-}


type alias Options styling msg =
    { styling : styling
    , blocks : List (Block styling msg)
    , inlines : List (Block styling msg)
    }


{-| -}
type Block style msg
    = Block String (Parser (style -> Element msg))
    | Parse String (Options style msg -> Parser (style -> Element msg))


markup :
    Options
        { styles
            | link : List (Element.Attribute msg)
            , token : List (Element.Attribute msg)
            , blockSpacing : Int
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
                        (Element.textColumn [ Element.spacing options.styling.blockSpacing ] (List.reverse existing))
                )
        , Parser.token "\n"
            |> Parser.map
                (\_ ->
                    Parser.Loop
                        existing
                )
        , customBlock options
            |> Parser.map
                (\found ->
                    Parser.Loop
                        (found :: existing)
                )
        , text options
            |> Parser.map
                (\found ->
                    let
                        _ =
                            Debug.log "p" found
                    in
                    if found == [] then
                        Parser.Done
                            (Element.column []
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


customBlock options =
    Parser.succeed identity
        |. Parser.token "|"
        |. Parser.chompIf (\c -> c == ' ')
        |= Parser.oneOf
            (List.map
                (blockToParser options)
                options.blocks
            )
        |> Parser.andThen
            identity


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
            , blockSpacing : Int
        }
        msg
    -> Parser (List (Element msg))
text options =
    Parser.loop emptyText (styledText options)


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
styledText :
    Options
        { styles
            | link : List (Element.Attribute msg)
            , token : List (Element.Attribute msg)
            , blockSpacing : Int
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
        [ Parser.succeed
            (\escaped ->
                existing
                    |> addText escaped
                    |> Parser.Loop
            )
            |. Parser.token "\\"
            |= Parser.getChompedString
                (Parser.chompIf (always True))

        -- Non breaking space
        , Parser.token "<>"
            |. Parser.chompWhile (\c -> c == '<' || c == '>' || c == ' ')
            |> Parser.map
                (\_ ->
                    existing
                        |> addText "\u{00A0}"
                        |> Parser.Loop
                )
        , Parser.succeed identity
            |. Parser.token "|"
            |. Parser.chompIf (\c -> c == ' ')
            |= Parser.oneOf
                (List.map
                    (blockToParser options)
                    options.inlines
                )
            |> Parser.andThen
                (\inlineParser ->
                    case changeStyle options.styling existing NoStyleChange of
                        StyledText newTxt newStyles newEls newLinkEls ->
                            inlineParser
                                |> Parser.map
                                    (\customElement ->
                                        Parser.Loop
                                            (StyledText
                                                newTxt
                                                newStyles
                                                (newEls ++ [ customElement ])
                                                newLinkEls
                                            )
                                    )
                )

        -- , Parser.succeed
        --     (\token ->
        --         let
        --             previous =
        --             String.trim token
        --         in
        --         case changeStyle options.styling existing of
        --             StyledText updatedTxt updatedstyles updatedEls ->
        --                 StyledText updatedTxt updatedstyles updatedEls
        --         if
        --             String.length dots
        --                 == 2
        --                 && not (Flag.present Flag.token styles)
        --         then
        --             Parser.Loop
        --                 (StyledText (txt ++ "…")
        --                     styles
        --                     els
        --                 )
        --         else
        --             Parser.Loop
        --                 (StyledText (txt ++ "." ++ dots)
        --                     styles
        --                     els
        --                 )
        --     )
        --     -- consume at least one dot
        --     |. Parser.token "{"
        --     |= Parser.getChompedString
        --         (Parser.chompWhile (\c -> c /= '}'))
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
        , Parser.token "'"
            |> Parser.map
                (\_ ->
                    existing
                        |> addText "’"
                        |> Parser.Loop
                )
        , Parser.succeed
            (\txt url ->
                changeStyle options.styling existing NoStyleChange
                    |> addElements [ renderStyledText options.styling styles txt (Just url) ]
                    |> Parser.Loop
            )
            |. Parser.token "["
            |= Parser.getChompedString
                (Parser.chompWhile (\c -> c /= ']' && c /= '\n'))
            |. Parser.token "]"
            |. Parser.token "("
            |= Parser.getChompedString
                (Parser.chompWhile (\c -> c /= ')' && c /= '\n' && c /= ' '))
            |. Parser.token ")"
        , Parser.succeed
            (Parser.Loop << changeStyle options.styling existing)
            |= Parser.oneOf
                [ Parser.map (always Italic) (Parser.token "/")
                , Parser.map (always Strike) (Parser.token "~")
                , Parser.map (always Bold) (Parser.token "*")
                , if Flag.present Flag.token styles then
                    Parser.map (always Token) (Parser.token "}")

                  else
                    Parser.map (always Token) (Parser.token "{")
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
                    let
                        _ =
                            Debug.log "chomped" new
                    in
                    if new == "" then
                        finalize options.styling existing

                    else
                        existing
                            |> addText new
                            |> Parser.Loop
                )
        ]


notReservedInline styles char =
    not
        (List.member char
            [ '-'
            , '~'
            , '\\'
            , '/'
            , '*'
            , '['
            , ']'
            , '\n'
            , '\''
            , '.'
            , doubleQuote
            , if Flag.present Flag.token styles then
                '}'

              else
                '{'
            ]
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
            List.filterMap identity
                [ if Flag.present Flag.bold styled then
                    Just Font.bold

                  else
                    Nothing
                , if Flag.present Flag.italic styled then
                    Just Font.italic

                  else
                    Nothing
                , if
                    Flag.present Flag.strike styled
                        && Flag.present Flag.underline styled
                  then
                    Just Font.underline

                  else
                    Nothing
                , if Flag.present Flag.strike styled then
                    Just Font.strike

                  else
                    Nothing
                ]
    in
    case maybeLink of
        Nothing ->
            if List.isEmpty styles && not (Flag.present Flag.token styled) then
                Element.text txt

            else
                Element.el
                    (if Flag.present Flag.token styled then
                        styleOptions.token ++ styles

                     else
                        styles
                    )
                    (Element.text txt)

        Just link ->
            Element.link
                (styleOptions.link ++ styles)
                { url = link
                , label = Element.text txt
                }


doubleQuote =
    '"'
