module Internal.Model exposing
    ( Block(..)
    , Inline(..)
    , InlineOptions
    , InlineStyle(..)
    , Link
    , Options
    , Replacement(..)
    , TextFormatting(..)
    , markup
    )

{-| -}

import Mark.Error exposing (..)
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


{-| -}
type alias Options result =
    { blocks : List (Block result)
    , merge : List result -> result
    , inlines : InlineOptions result
    }


type alias InlineOptions result =
    { view : TextFormatting -> Maybe Link -> result
    , inlines : List (Inline result)
    , merge : List result -> result
    , replacements : List Replacement
    }


type Replacement
    = Replacement String String
    | Balanced
        { start : ( String, String )
        , end : ( String, String )
        }


{-| -}
type Block result
    = Block String (Int -> Parser Context Problem (List result) -> Parser Context Problem result)


{-| -}
type Inline result
    = Inline String (TextFormatting -> Maybe Link -> result)


type alias Link =
    { url : String }


type TextFormatting
    = NoFormatting String
    | Styles (List InlineStyle) String
    | Fragments (List Fragment)


type alias Fragment =
    { text : String
    , styles : List InlineStyle
    }


type InlineStyle
    = NoStyleChange
    | Bold
    | Italic
    | Strike
    | Underline
    | Token



{- Formatted Text -}


type Text rendered
    = Text
        -- Accumulator string
        { text : TextFormatting

        -- Accumulator of element constructors
        , rendered : List rendered
        , balancedReplacements : List String
        }


emptyText =
    Text { text = NoFormatting "", rendered = [], balancedReplacements = [] }


{-| -}
markup : Options result -> Parser Context Problem result
markup options =
    Parser.map (options.merge << List.reverse) <|
        Parser.loop []
            (blocks options)


{-| Currently:

    end -> done
    newline -> skip
    block (| block) -> parse block
    inlines -> inlines

-}
blocks : Options result -> List result -> Parser Context Problem (Parser.Step (List result) (List result))
blocks options existing =
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

        -- custom blocks
        , Parser.succeed
            (\blockParser ->
                blockParser 0 (Parser.loop emptyText (inlineLoop options.inlines))
            )
            |. Parser.token (Parser.Token "|=" (Expecting "|="))
            |. Parser.chompWhile (\c -> c == ' ')
            |= Parser.oneOf
                (case options.blocks of
                    [] ->
                        [ Parser.problem NoBlocks ]

                    _ ->
                        List.filterMap
                            blockToParser
                            options.blocks
                )
            |. Parser.chompWhile (\c -> c == ' ')
            |. Parser.chompIf (\c -> c == '\n') Newline
            |> Parser.andThen
                (\blockParser ->
                    Parser.map
                        (\found ->
                            Parser.Loop
                                (found :: existing)
                        )
                        blockParser
                )

        -- custom inlines
        , Parser.loop emptyText (inlineLoop options.inlines)
            |> Parser.map
                (\found ->
                    Parser.Loop
                        (options.inlines.merge found :: existing)
                )
        ]


{-| -}
indent : Parser Mark.Error.Context Mark.Error.Problem Int
indent =
    Parser.oneOf
        [ --     Parser.token (Parser.Token (String.repeat 12 " ") Mark.Error.ExpectedIndent)
          --     |> Parser.map (always 3)
          -- , Parser.token (Parser.Token (String.repeat 8 " ") Mark.Error.ExpectedIndent)
          --     |> Parser.map (always 2)
          Parser.token (Parser.Token (String.repeat 4 " ") Mark.Error.ExpectedIndent)
            |> Parser.map (always 1)
        ]



{- Custom Blocks -}


inlineToParser blockable =
    case blockable of
        Inline name fn ->
            Parser.keyword (Parser.Token name (Expecting name))
                |> Parser.inContext (InInline name)
                |> Parser.map
                    (always fn)


capitalize str =
    case str of
        "" ->
            Nothing

        _ ->
            let
                fst =
                    String.left 1 str

                remaining =
                    String.dropLeft 1 str
            in
            Just (String.toUpper fst ++ remaining)


blockToParser :
    Block result
    -> Maybe (Parser Context Problem (Int -> Parser Context Problem (List result) -> Parser Context Problem result))
blockToParser blockable =
    case blockable of
        Block nonstandardizedName parser ->
            let
                maybeName =
                    nonstandardizedName
                        |> String.trim
                        |> capitalize
            in
            case maybeName of
                Nothing ->
                    Nothing

                Just name ->
                    Parser.keyword (Parser.Token name (Expecting name))
                        |> Parser.inContext (InBlock name)
                        |> Parser.map
                            (\_ ->
                                parser
                            )
                        |> Just


reduceFragments txt =
    case txt of
        Fragments frags ->
            let
                finalized =
                    frags
                        |> List.filter (\f -> f.text /= "")
                        |> List.reverse
            in
            case finalized of
                [] ->
                    txt

                [ frag ] ->
                    Styles frag.styles frag.text

                remaining ->
                    Fragments remaining

        _ ->
            txt


inlineLoop :
    InlineOptions result
    -> Text result
    -> Parser Context Problem (Parser.Step (Text result) (List result))
inlineLoop inlineOptions existing =
    let
        txt =
            case existing of
                Text { text } ->
                    text
    in
    Parser.oneOf
        [ -- Do any character replacements that are necessary
          Parser.oneOf (replace inlineOptions.replacements existing)
            |> Parser.map Parser.Loop

        -- If a char matches the first character of a replacement,
        -- but didn't match the full replacement captured above,
        -- then stash that char.
        , Parser.oneOf (almostReplacement inlineOptions.replacements existing)
            |> Parser.map Parser.Loop

        -- Custom inline block
        , Parser.succeed
            (\customInline (Text styled) ->
                case changeStyle inlineOptions existing NoStyleChange of
                    Text current ->
                        Parser.Loop <|
                            Text
                                { rendered =
                                    customInline
                                        (reduceFragments styled.text)
                                        Nothing
                                        :: styled.rendered
                                        ++ current.rendered
                                , text =
                                    case styled.text of
                                        NoFormatting _ ->
                                            NoFormatting ""

                                        Styles styles _ ->
                                            Styles styles ""

                                        Fragments frags ->
                                            case frags of
                                                [] ->
                                                    NoFormatting ""

                                                f :: _ ->
                                                    Styles f.styles ""
                                , balancedReplacements = styled.balancedReplacements
                                }
            )
            |. Parser.token
                (Parser.Token "<" InlineStart)
            |= Parser.oneOf
                (List.map inlineToParser inlineOptions.inlines)
            |. Parser.token (Parser.Token "|" InlineBar)
            |= styledText inlineOptions txt [ '>' ]
            |. Parser.token (Parser.Token ">" InlineEnd)

        -- Link
        , Parser.succeed
            (\(Text linkText) url ->
                case changeStyle inlineOptions existing NoStyleChange of
                    Text current ->
                        Parser.Loop <|
                            Text
                                { rendered =
                                    inlineOptions.view
                                        (reduceFragments linkText.text)
                                        (Just { url = url })
                                        :: linkText.rendered
                                        ++ current.rendered
                                , text =
                                    case linkText.text of
                                        NoFormatting _ ->
                                            NoFormatting ""

                                        Styles styles _ ->
                                            Styles styles ""

                                        Fragments frags ->
                                            case frags of
                                                [] ->
                                                    NoFormatting ""

                                                f :: _ ->
                                                    Styles f.styles ""
                                , balancedReplacements = linkText.balancedReplacements
                                }
            )
            |. Parser.token (Parser.Token "[" (Expecting "["))
            |= styledText inlineOptions txt [ ']' ]
            |. Parser.token (Parser.Token "]" (Expecting "]"))
            |. Parser.token (Parser.Token "(" (Expecting "("))
            |= Parser.getChompedString
                (Parser.chompWhile (\c -> c /= ')' && c /= '\n' && c /= ' '))
            |. Parser.token (Parser.Token ")" (Expecting ")"))

        -- Capture styling
        , Parser.succeed
            (Parser.Loop << changeStyle inlineOptions existing)
            |= Parser.oneOf
                [ Parser.map (always Italic) (Parser.token (Parser.Token "/" (Expecting "/")))
                , Parser.map (always Strike) (Parser.token (Parser.Token "~" (Expecting "~")))
                , Parser.map (always Bold) (Parser.token (Parser.Token "*" (Expecting "*")))
                , Parser.map (always Token) (Parser.token (Parser.Token "`" (Expecting "`")))
                ]

        -- end on newline
        , Parser.token (Parser.Token "\n" Newline)
            |> Parser.map
                (\_ ->
                    finalize inlineOptions existing
                )

        -- chomp until a meaningful character
        , Parser.getChompedString
            (Parser.chompWhile
                (\c ->
                    let
                        meaningful =
                            stylingChars ++ replacementStartingChars inlineOptions.replacements
                    in
                    not (List.member c meaningful)
                )
            )
            |> Parser.map
                (\new ->
                    if new == "" then
                        finalize inlineOptions existing

                    else
                        Parser.Loop (addText new existing)
                )
        ]


finalize inlineOptions (Text cursor) =
    let
        txt =
            case cursor.text of
                NoFormatting x ->
                    x

                Styles _ x ->
                    x

                Fragments frags ->
                    frags
                        |> List.map .text
                        |> String.join ""
    in
    Parser.Done <|
        if txt == "" then
            List.reverse cursor.rendered

        else
            List.reverse (inlineOptions.view cursor.text Nothing :: cursor.rendered)


{-| This parser is used to parse styled text, but the delay rendering it.

That may seem sorta weird, but we need this in cases like for links. We want styled

-}
styledText : InlineOptions result -> TextFormatting -> List Char -> Parser Context Problem (Text result)
styledText options txt until =
    let
        vacantText =
            case txt of
                NoFormatting x ->
                    NoFormatting ""

                Styles styles _ ->
                    Styles styles ""

                -- TODO: is this an issue?
                x ->
                    x

        untilStrings =
            List.map String.fromChar until
    in
    Parser.loop (Text { text = vacantText, rendered = [], balancedReplacements = [] })
        (\found ->
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
                    (Parser.Loop << cacheStyle options found)
                    |= Parser.oneOf
                        [ Parser.map (always Italic) (Parser.token (Parser.Token "/" (Expecting "/")))
                        , Parser.map (always Strike) (Parser.token (Parser.Token "~" (Expecting "~")))
                        , Parser.map (always Bold) (Parser.token (Parser.Token "*" (Expecting "*")))
                        , Parser.map (always Token) (Parser.token (Parser.Token "`" (Expecting "`")))
                        ]

                -- chomp until a meaningful character
                , Parser.getChompedString
                    (Parser.chompWhile
                        (\c ->
                            let
                                meaningful =
                                    '\n' :: until ++ stylingChars ++ replacementStartingChars options.replacements
                            in
                            not (List.member c meaningful)
                        )
                    )
                    |> Parser.map
                        (\new ->
                            if new == "" || new == "\n" then
                                Parser.Done found

                            else if List.member (String.right 1 new) untilStrings then
                                Parser.Done (addText (String.dropRight 1 new) found)

                            else
                                Parser.Loop (addText new found)
                        )
                ]
        )


cacheStyle inlineOptions (Text cursor) styleToken =
    let
        newText =
            case styleToken of
                NoStyleChange ->
                    cursor.text

                Bold ->
                    cacheNewStyle Bold cursor.text

                Italic ->
                    cacheNewStyle Italic cursor.text

                Strike ->
                    cacheNewStyle Strike cursor.text

                Underline ->
                    cacheNewStyle Underline cursor.text

                Token ->
                    cacheNewStyle Token cursor.text
    in
    Text
        { rendered = cursor.rendered
        , text = newText
        , balancedReplacements = cursor.balancedReplacements
        }


cacheNewStyle newStyle text =
    case text of
        NoFormatting str ->
            Fragments [ { text = "", styles = [ newStyle ] }, { text = str, styles = [] } ]

        Styles styles str ->
            if List.member newStyle styles then
                Fragments [ { text = "", styles = List.filter ((/=) newStyle) styles }, { text = str, styles = styles } ]

            else
                Fragments [ { text = "", styles = [ newStyle ] }, { text = str, styles = styles } ]

        Fragments frags ->
            case frags of
                [] ->
                    Fragments [ { text = "", styles = [ newStyle ] } ]

                current :: remain ->
                    if List.member newStyle current.styles then
                        Fragments <| { text = "", styles = List.filter ((/=) newStyle) current.styles } :: current :: remain

                    else
                        Fragments <| { text = "", styles = newStyle :: current.styles } :: current :: remain



-- Fragments ({ text = "", styles = [ newStyle ] } :: frags)


{-| -}
almostReplacement : List Replacement -> Text rendered -> List (Parser Context Problem (Text rendered))
almostReplacement replacements existing =
    let
        captureChar char =
            Parser.succeed
                (\c ->
                    addText c existing
                )
                |= Parser.getChompedString
                    (Parser.chompIf (\c -> c == char) EscapedChar)

        first repl =
            case repl of
                Replacement x y ->
                    firstChar x

                Balanced balanced ->
                    firstChar (Tuple.first balanced.start)

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
replace : List Replacement -> Text rendered -> List (Parser Context Problem (Text rendered))
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

                Balanced balanced ->
                    let
                        balanceCache =
                            case existing of
                                Text cursor ->
                                    cursor.balancedReplacements

                        id =
                            balanceId balanced
                    in
                    -- TODO: implement balanced replacement
                    if List.member id balanceCache then
                        case balanced.end of
                            ( x, y ) ->
                                Parser.succeed
                                    (addText y existing
                                        |> removeBalance id
                                    )
                                    |. Parser.token (Parser.Token x (Expecting x))

                    else
                        case balanced.start of
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

                Balanced balanced ->
                    firstChar (Tuple.first balanced.start)
    in
    List.filterMap first replacements


addBalance id (Text cursor) =
    Text <|
        { cursor | balancedReplacements = id :: cursor.balancedReplacements }


removeBalance id (Text cursor) =
    Text <|
        { cursor | balancedReplacements = List.filter ((/=) id) cursor.balancedReplacements }


addText newTxt (Text cursor) =
    case cursor.text of
        NoFormatting txt ->
            Text { cursor | text = NoFormatting (txt ++ newTxt) }

        Styles styles txt ->
            Text { cursor | text = Styles styles (txt ++ newTxt) }

        Fragments frags ->
            case frags of
                [] ->
                    Text { cursor | text = Fragments [ { text = newTxt, styles = [] } ] }

                recent :: remain ->
                    Text { cursor | text = Fragments ({ text = recent.text ++ newTxt, styles = recent.styles } :: remain) }


{-| If we accumulating a link style, accumulate it there.
-}
addElement newElements (Text cursor) =
    Text { cursor | rendered = newElements :: cursor.rendered }


changeStyle inlineOptions (Text cursor) styleToken =
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
        Text { rendered = cursor.rendered, text = newText, balancedReplacements = cursor.balancedReplacements }

    else
        Text
            { rendered = inlineOptions.view cursor.text Nothing :: cursor.rendered
            , text = newText
            , balancedReplacements = cursor.balancedReplacements
            }


flipStyle newStyle text =
    case text of
        NoFormatting str ->
            Styles [ newStyle ] ""

        Styles styles str ->
            if List.member newStyle styles then
                Styles (List.filter ((/=) newStyle) styles) ""

            else
                Styles (newStyle :: styles) ""

        Fragments frags ->
            Fragments frags
