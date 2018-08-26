module Internal.Model exposing (..)

{-| -}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region
import Internal.Flag as Flag
import Parser exposing ((|.), (|=), Parser)


{-
      Options:

           - Custom blocks
               -> normal : put this Element here
               -> function : some number of int, float, bool, string
               -> list of styled paragraphs
               -> parse unstyled text (like for parsing raw elm code for highlighting)

           - Styling Information
               -> link style
               -> code token style
               -> List icons and styling
               -> header styling



   -- Reclaimed typography

       -> "\ " A backspace followed by a space will create a non-breaking space.
           (unlike &nbsp, they will still collapse down to one space, it just won't break.)
       -> "--" -> "en-dash"
       -> "---" Dash dash signifies an "em-dash".  An incredibly useful symbol that is more of a pause then a comma, but less than a semicolon or period.
       -> Quotation marks will be replaced with curly quotes.
       -> "..." -> ellipses

    -- Side Notes

        -> Tokens will ignore styling that is started or resolved in the token block
            -> This is to avoid clashes with things tha are likely embedded code.
            -> They will still inherit stying if it's within the space.



   -- Additional, built in blocks



-}


type alias Options styling msg =
    { styling : styling
    , blocks : List (Block styling msg)
    }


{-| -}
type Block style msg
    = Block String (Parser (style -> Element msg))
    | Parse String (style -> Parser (style -> Element msg))


customBlock options =
    Parser.succeed identity
        |. Parser.token "|"
        |. Parser.chompIf (\c -> c == ' ')
        |= Parser.oneOf
            (List.map
                (blockToParser options.styling)
                options.blocks
            )
        |> Parser.andThen
            identity


blockToParser styling blockable =
    case blockable of
        Block name arguments ->
            Parser.keyword name
                |> Parser.map
                    (\_ ->
                        Parser.succeed
                            (\fn ->
                                fn styling
                            )
                            |= arguments
                    )

        Parse name parser ->
            Parser.keyword name
                |> Parser.map
                    (\_ ->
                        parser styling
                            |> Parser.map
                                (\fn ->
                                    fn styling
                                )
                    )


markup :
    Options
        { styles
            | link : List (Element.Attribute msg)
            , token : List (Element.Attribute msg)
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
                    let
                        _ =
                            Debug.log "done!" "yes"
                    in
                    Parser.Done
                        (Element.column [] (List.reverse existing))
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
        , text options.styling
            |> Parser.map
                (\found ->
                    let
                        _ =
                            Debug.log "p" found
                    in
                    if found == [] then
                        Parser.Done (Element.column [] (List.reverse existing))
                    else
                        Parser.Loop (Element.paragraph [] found :: existing)
                )
        ]


type StyledText msg
    = StyledText String Flag.Field (List (Element msg))



{- Formatted Text -}


type InlineStyle
    = Bold
    | Italic
    | Strike
    | Underline
    | Token
    | LinkStart
    | Link String


text :
    { styles
        | link : List (Element.Attribute msg)
        , token : List (Element.Attribute msg)
    }
    -> Parser (List (Element msg))
text styling =
    Parser.loop (StyledText "" Flag.none []) (styledText styling)


styledText :
    { styles
        | link : List (Element.Attribute msg)
        , token : List (Element.Attribute msg)
    }
    -> StyledText msg
    -> Parser (Parser.Step (StyledText msg) (List (Element msg)))
styledText styling ((StyledText txt styles els) as existing) =
    Parser.oneOf
        [ Parser.succeed
            (\dots ->
                if
                    String.length dots
                        == 2
                        && not (Flag.present Flag.token styles)
                then
                    Parser.Loop
                        (StyledText (txt ++ "…")
                            styles
                            els
                        )
                else
                    Parser.Loop
                        (StyledText (txt ++ "." ++ dots)
                            styles
                            els
                        )
            )
            -- consume at least one dot
            |. Parser.token "."
            |= Parser.getChompedString
                (Parser.chompWhile (\c -> c == '.'))
        , Parser.succeed
            (\dashes ->
                if Flag.present Flag.token styles then
                    Parser.Loop
                        (StyledText (txt ++ "-" ++ dashes)
                            styles
                            els
                        )
                else if String.length dashes == 1 then
                    Parser.Loop
                        (StyledText (txt ++ "–")
                            styles
                            els
                        )
                else if String.length dashes == 2 then
                    Parser.Loop
                        (StyledText (txt ++ "—")
                            styles
                            els
                        )
                else
                    Parser.Loop
                        (StyledText (txt ++ "-" ++ dashes)
                            styles
                            els
                        )
            )
            -- consume at least one dash
            |. Parser.token "-"
            |= Parser.getChompedString
                (Parser.chompWhile (\c -> c == '-'))
        , Parser.token "\""
            |> Parser.map
                (\_ ->
                    if Flag.present Flag.token styles then
                        Parser.Loop
                            (StyledText (txt ++ "\"") styles els)
                    else if Flag.present Flag.doubleQuote styles then
                        Parser.Loop
                            (StyledText (txt ++ "”")
                                (Flag.flip Flag.doubleQuote styles)
                                els
                            )
                    else
                        Parser.Loop
                            (StyledText (txt ++ "“")
                                (Flag.flip Flag.doubleQuote styles)
                                els
                            )
                )
        , Parser.token "'"
            |> Parser.map
                (\_ ->
                    if Flag.present Flag.token styles then
                        Parser.Loop
                            (StyledText (txt ++ "'") styles els)
                    else
                        Parser.Loop
                            (StyledText (txt ++ "’")
                                styles
                                els
                            )
                )
        , Parser.succeed
            (Parser.Loop << changeStyle styling existing)
            |= Parser.oneOf
                [ -- Parser.map (always Underline) (Parser.token "_")
                  Parser.map (always Italic) (Parser.token "/")

                -- , Parser.map (always Strike) (Parser.token "~")
                , Parser.map (always Bold) (Parser.token "*")
                , if Flag.present Flag.token styles then
                    Parser.map (always Token) (Parser.token "}")
                  else
                    Parser.map (always Token) (Parser.token "{")
                , if Flag.present Flag.link styles then
                    Parser.succeed (String.trim >> Link)
                        |. Parser.token "]"
                        |. Parser.token "("
                        |= Parser.getChompedString
                            (Parser.chompWhile (\c -> c /= ')' && c /= '\n' && c /= ' '))
                        |. Parser.token ")"
                  else
                    Parser.map (always LinkStart) (Parser.token "[")
                ]
        , Parser.token "\n"
            |> Parser.map
                (\_ ->
                    finalize styling existing
                )

        -- , Parser.succeed
        --         (\char ->
        --             case char of
        --                 " " ->
        --                 _ ->
        --                     Parser.Loop
        --                     (StyledText (txt ++ char) styles els)
        --         )
        --     |. Parser.token "\\"
        --     |= Parser.oneOf
        --         [ Parser.
        --         ]
        --         Parser.getChompedString
        --         (Parser.chompIf (always True)
        , Parser.getChompedString
            (Parser.chompWhile (notReservedInline styles))
            |> Parser.map
                (\new ->
                    let
                        _ =
                            Debug.log "chomped" new
                    in
                    if new == "" then
                        finalize styling existing
                    else
                        Parser.Loop
                            (StyledText (txt ++ new) styles els)
                )
        ]


notReservedInline styles char =
    not
        (List.member char
            [ '-'
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


finalize styling (StyledText txt styles els) =
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



{- Inline -}


builtinInlines =
    []


customInline =
    Parser.succeed identity
        |. Parser.token "| "
        |= Parser.oneOf builtinInlines
        |> Parser.andThen
            (\parser ->
                let
                    _ =
                        Debug.log "start inline" "yup"
                in
                Parser.succeed identity
                    |= parser
             -- |. Parser.chompWhile (\c -> c == ' ')
             -- |. Parser.token "|"
            )


changeStyle styling (StyledText txt styles els) styleToken =
    let
        newStyles =
            case styleToken of
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
    StyledText "" newStyles (renderStyledText styling styles txt link :: els)


renderStyledText styleOptions styled txt maybeLink =
    let
        _ =
            Debug.log "render" txt

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
                , if Flag.present Flag.strike styled && Flag.present Flag.underline styled then
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
            if List.isEmpty styles then
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



-- {-| -}
-- list : Styling msg -> Parser (Element msg)
-- list styling =
--     Parser.succeed (Element.column [])
--         |. Parser.chompWhile (\c -> c == '\n')
--         |= Parser.loop
--             ( emptyCursor, [] )
--             (\( cursor, existing ) ->
--                 Parser.oneOf
--                     [ Parser.succeed
--                         (\indent token el ->
--                             let
--                                 newCursor =
--                                     advanceCursor cursor indent
--                             in
--                             Parser.Loop ( newCursor, Element.paragraph [] (styling.listIcons newCursor token :: el) :: existing )
--                         )
--                         |= indentLevel
--                         |= Parser.oneOf
--                             [ Parser.token "->"
--                                 |> Parser.map (always Arrow)
--                             , Parser.token "-"
--                                 |> Parser.map (always Dash)
--                             , Parser.token "+"
--                                 |> Parser.map (always Plus)
--                             ]
--                         |. Parser.token " "
--                         |= text styling emptyStyle
--                     , Parser.token "|"
--                         |> Parser.map
--                             (\_ ->
--                                 Parser.Done (List.reverse existing)
--                             )
--                     , Parser.token "\n"
--                         |> Parser.map (always (Parser.Loop ( cursor, existing )))
--                     ]
--             )


doubleQuote =
    '"'
