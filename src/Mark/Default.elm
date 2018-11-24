module Mark.Default exposing
    ( document
    , title, header, list, monospace
    , defaultText, replacements, text
    )

{-|

@docs document

@docs title, header, list, monospace

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region
import Html.Attributes
import Mark.Custom
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


document =
    Mark.Custom.root
        (\children model ->
            Element.textColumn []
                (List.map (\view -> view model) children)
        )
        (Mark.Custom.many
            (Mark.Custom.oneOf
                [ title [ Font.size 48 ] defaultText
                , header [ Font.size 36 ] defaultText
                , monospace
                    [ Element.spacing 5
                    , Element.padding 24
                    , Background.color
                        (Element.rgba 0 0 0 0.04)
                    , Border.rounded 2
                    , Font.size 16
                    , Font.family
                        [ Font.external
                            { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                            , name = "Source Code Pro"
                            }
                        , Font.sansSerif
                        ]
                    ]

                -- Toplevel Text
                , Mark.Custom.map (\viewEls model -> Element.paragraph [] (viewEls model)) defaultText
                ]
            )
        )


defaultText =
    text
        { code =
            [ Background.color
                (Element.rgba 0 0 0 0.04)
            , Border.rounded 2
            , Element.paddingXY 5 3
            , Font.size 16
            , Font.family
                [ Font.external
                    { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                    , name = "Source Code Pro"
                    }
                , Font.sansSerif
                ]
            ]
        , link =
            [ Font.color
                (Element.rgb
                    (17 / 255)
                    (132 / 255)
                    (206 / 255)
                )
            , Element.mouseOver
                [ Font.color
                    (Element.rgb
                        (234 / 255)
                        (21 / 255)
                        (122 / 255)
                    )
                ]
            ]
        }


{-| -}
title : List (Element.Attribute msg) -> Mark.Custom.Block (model -> List (Element msg)) -> Mark.Custom.Block (model -> Element msg)
title attrs titleText =
    Mark.Custom.block "Title"
        (\elements model ->
            Element.paragraph
                (Element.Region.heading 1 :: attrs)
                (elements model)
        )
        titleText


{-| -}
header : List (Element.Attribute msg) -> Mark.Custom.Block (model -> List (Element msg)) -> Mark.Custom.Block (model -> Element msg)
header attrs textParser =
    Mark.Custom.block "Header"
        (\elements model ->
            Element.paragraph
                (Element.Region.heading 2 :: attrs)
                (elements model)
        )
        textParser


{-| -}
monospace : List (Element.Attribute msg) -> Mark.Custom.Block (model -> Element msg)
monospace attrs =
    Mark.Custom.block "Monospace"
        (\string model ->
            Element.paragraph
                (Element.htmlAttribute (Html.Attributes.style "line-height" "1.4em")
                    :: Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
                    :: attrs
                )
                [ Element.text (String.join "\n" string) ]
        )
        (Mark.Custom.many Mark.Custom.string)


{-| -}
text :
    { code : List (Element.Attribute msg)
    , link : List (Element.Attribute msg)
    }
    -> Mark.Custom.Block (model -> List (Element msg))
text style =
    Mark.Custom.textWith
        { view = textView style
        , inlines = []
        , merge =
            \els model ->
                List.map (\view -> view model) els
        , replacements = replacements
        }


textView config textNode =
    textFragment config textNode


textFragment config node model_ =
    case node.style of
        Mark.Custom.NoFormatting txt ->
            case node.link of
                Nothing ->
                    Element.text txt

                Just url ->
                    Element.link config.link
                        { url = url
                        , label = Element.text txt
                        }

        Mark.Custom.Styles styles txt ->
            case node.link of
                Nothing ->
                    Element.el (List.concatMap (toStyles config) styles) (Element.text txt)

                Just url ->
                    Element.link (config.link ++ List.concatMap (toStyles config) styles)
                        { url = url
                        , label = Element.text txt
                        }


toStyles config style =
    case style of
        Mark.Custom.NoStyleChange ->
            []

        Mark.Custom.Bold ->
            [ Font.bold ]

        Mark.Custom.Italic ->
            [ Font.italic ]

        Mark.Custom.Strike ->
            [ Font.strike ]

        Mark.Custom.Underline ->
            [ Font.underline ]

        Mark.Custom.Token ->
            config.code


{-| Replace certain characters with improved typographical ones.
Escaping a character will skip the replacement, so this isn't mandatory.

  - `"<>"` -> A non-breaking space which glues words together so that they don't break when wrapping.
  - `"--"` -> en-dash, `–`
  - `"---"` -> em-dash, `—`
  - Quotation marks will be replaced with curly quotes.
  - `"..."` -> ellipses, `…`
  - `'` -> `’`

-}
replacements : List Mark.Custom.Replacement
replacements =
    [ Mark.Custom.replacement "..." "…"
    , Mark.Custom.replacement "<>" "\u{00A0}"
    , Mark.Custom.replacement "---" "—"
    , Mark.Custom.replacement "--" "–"
    , Mark.Custom.replacement "'" "’"
    , Mark.Custom.balanced
        { start = ( "\"", "“" )
        , end = ( "\"", "”" )
        }
    ]



{-


   Nested List Handling


-}


{-| Parse a nested list
-}
list :
    { icon : List Index -> ListIcon -> Element msg
    , style : List Index -> List (Element.Attribute msg)
    }
    -> Mark.Custom.Block (model -> List (Element msg))
    -> Mark.Custom.Block (model -> Element msg)
list listConfig textParser =
    -- Mark.Custom.advanced "List" <|
    -- Parser.loop
    --     ( emptyCursor, emptyListBuilder )
    --     (listItem listConfig textParser)
    Mark.Custom.block "List"
        (\items model ->
            Element.column []
                []
        )
        (Mark.Custom.many
            textParser
        )



{- LIST -}


{-| -}
listStyles : List Index -> List (Element.Attribute msg)
listStyles cursor =
    case List.length cursor of
        0 ->
            -- top level element
            [ Element.spacing 64 ]

        1 ->
            [ Element.spacing 32 ]

        2 ->
            [ Element.spacing 16 ]

        _ ->
            [ Element.spacing 8 ]


edges =
    { top = 0
    , left = 0
    , right = 0
    , bottom = 0
    }


{-| -}
listIcons : List Index -> ListIcon -> Element msg
listIcons cursor symbol =
    let
        pad =
            Element.paddingEach
                { edges
                    | left = 28
                    , right = 12
                }
    in
    case symbol of
        Arrow ->
            Element.el
                [ pad ]
                (Element.text "➙")

        Bullet ->
            let
                icon =
                    case List.length cursor of
                        1 ->
                            "•"

                        _ ->
                            "◦"
            in
            Element.el [ pad ] (Element.text icon)

        Number ->
            Element.el [ pad ]
                (Element.text
                    (List.foldl formatIndex "" cursor)
                )


formatIndex index formatted =
    if index.show then
        formatted ++ String.fromInt index.index ++ index.decoration

    else
        formatted


type alias Index =
    { decoration : String
    , index : Int
    , show : Bool
    }


{-| -}
type ListIcon
    = Bullet
    | Arrow
    | Number


{-| -}
type alias Cursor =
    { current : Int
    , stack : List Int
    }


emptyCursor : Cursor
emptyCursor =
    { current = 0
    , stack = []
    }


cursorLevel ( current, nested ) =
    List.length nested + 1


mapCursor fn cursor =
    List.map fn (cursor.current :: cursor.stack)


{-| -}
type CursorReset
    = CursorReset (List (Maybe Int))


{-| -}
indentLevelAbove : Int -> Parser Mark.Custom.Context Mark.Custom.Problem Int
indentLevelAbove base =
    Parser.succeed
        (\additional ->
            1 + (String.length additional // 4)
        )
        |. Parser.token (Parser.Token (String.repeat ((base + 1) * 4) " ") Mark.Custom.ExpectedIndent)
        |= Parser.getChompedString (Parser.chompWhile (\c -> c == ' '))


{-| -}
indentLevelAtOrBelow : Int -> Parser Mark.Custom.Context Mark.Custom.Problem Int
indentLevelAtOrBelow base =
    Parser.succeed
        (\additional ->
            String.length additional // 4
        )
        |= Parser.getChompedString (Parser.chompWhile (\c -> c == ' '))


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
type ListBuilder model msg
    = ListBuilder
        { previousIndent : Int
        , previousLineEmpty : Bool
        , levels :
            -- (mostRecent :: remaining)
            List (Level model msg)
        }



{-
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


type Level model msg
    = Level (List (ListItem model msg))


{-| -}
type ListItem model msg
    = ListItem
        { icon :
            Maybe
                { token : ListIcon
                , decorations :
                    List
                        { decoration : String
                        , index : Int
                        , show : Bool
                        }
                }
        , content : List (model -> Element msg)
        , children :
            -- (mostRecent :: remaining)
            List (ListItem model msg)
        }


emptyListBuilder : ListBuilder model msg
emptyListBuilder =
    ListBuilder
        { previousIndent = 0
        , previousLineEmpty = False
        , levels = []
        }


finalizeList config (ListBuilder builder) model =
    Element.column
        (config.style [])
        (renderLevels config model builder.levels)


renderLevels config model levels =
    case levels of
        [] ->
            []

        _ ->
            case collapseLevel (List.length levels - 1) levels of
                [] ->
                    []

                (Level top) :: ignore ->
                    -- We just collapsed everything down to the top level.
                    top
                        |> List.reverse
                        |> List.map (renderListItem config model)


renderListItem config model (ListItem item) =
    case item.icon of
        Nothing ->
            case item.children of
                [] ->
                    renderParagraph model item.content

                _ ->
                    Element.column
                        (config.style [])
                        (renderParagraph model item.content
                            :: (item.children
                                    |> List.reverse
                                    |> List.map (renderListItem config model)
                               )
                        )

        Just actualIcon ->
            Element.row []
                [ Element.el [ Element.alignTop ] <|
                    config.icon
                        actualIcon.decorations
                        actualIcon.token
                , Element.textColumn
                    (config.style
                        actualIcon.decorations
                    )
                    (renderParagraph model item.content
                        :: (item.children
                                |> List.reverse
                                |> List.map (renderListItem config model)
                           )
                    )
                ]


renderParagraph model content =
    Element.paragraph
        []
        (List.map (\el -> el model) content)


{-| Parses a single line item (with multiple paragraps)

    | a newline if there wasn't one before
    | end of file

    root [spacing 1]
        [ -- spacing 2
            [ first

            With an additional space before it.

            -> [inner item]

            -> spacing 3:
                [ other inner item
                -> embedded item
                -> embedded again
                ]
            ]
        ]



        [ -- spacing 2[second and some other content]

        ]



    So, to do this:

        -> each line item needs to be grouped in its entirity.
        -> More to recursion
            -> listItem -> itemContent -> listItem
            -> ensure we only recurse if we've made progress

-}
listItem :
    { icon : List Index -> ListIcon -> Element msg
    , style : List Index -> List (Element.Attribute msg)
    }
    -> Parser Mark.Custom.Context Mark.Custom.Problem (List (model -> Element msg))
    -> ( Cursor, ListBuilder model msg )
    -> Parser Mark.Custom.Context Mark.Custom.Problem (Parser.Step ( Cursor, ListBuilder model msg ) (model -> Element msg))
listItem config inlines ( cursor, ListBuilder builder ) =
    Parser.oneOf
        [ --Parser.succeed identity
          -- Get the indent level above the baseline.
          -- |= indentLevelAbove baseIndent
          Parser.getIndent
            |> Parser.andThen
                (\indent ->
                    Parser.map Parser.Loop <|
                        Parser.oneOf <|
                            [ Parser.succeed (addItem cursor indent (ListBuilder builder))
                                |= Parser.map Just listIcon
                                |. Parser.chompWhile (\c -> c == ' ')
                                |= inlines
                            , Parser.succeed (addItem cursor indent (ListBuilder builder) Nothing)
                                |= inlines
                            ]
                )
        , Parser.end Mark.Custom.End
            |> Parser.map
                (\_ ->
                    Parser.Done (finalizeList config (ListBuilder builder))
                )

        -- Whitespace Line
        , Parser.succeed
            (Parser.Loop ( cursor, ListBuilder builder ))
            |. Parser.backtrackable (Parser.token (Parser.Token "\n" Mark.Custom.Newline))
            |. Parser.backtrackable (Parser.chompWhile (\c -> c == ' '))
            |. Parser.backtrackable (Parser.token (Parser.Token "\n" Mark.Custom.Newline))

        -- Newline
        , Parser.succeed
            (Parser.Loop ( cursor, ListBuilder builder ))
            |. Parser.token (Parser.Token "\n" Mark.Custom.Newline)

        -- If we get here, we're done!
        , Parser.succeed
            (Parser.Done (finalizeList config (ListBuilder builder)))
        ]


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
    Cursor
    -> Int
    -> ListBuilder model msg
    -> Maybe ( List (Maybe Int), List String, ListIcon )
    -> List (model -> Element msg)
    -> ( Cursor, ListBuilder model msg )
addItem cursor indent (ListBuilder builder) maybeIcon styledParagraphs =
    let
        newCursor =
            case maybeIcon of
                Just ( reset, decorations, token ) ->
                    cursor
                        |> advanceCursor indent
                        |> resetCursor reset

                Nothing ->
                    cursor

        newItem =
            ListItem
                { children = []
                , content = styledParagraphs
                , icon =
                    case maybeIcon of
                        Nothing ->
                            Nothing

                        Just ( reset, decorations, token ) ->
                            Just
                                { token = token
                                , decorations = decorate decorations newCursor
                                }
                }

        deltaLevel =
            indent - List.length builder.levels

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
            ( newCursor
            , ListBuilder
                { previousLineEmpty = False
                , previousIndent = indent
                , levels =
                    [ Level
                        [ newItem ]
                    ]
                }
            )

        (Level lvl) :: remaining ->
            if deltaLevel == 0 then
                -- add to current level
                ( newCursor
                , ListBuilder
                    { previousLineEmpty = False
                    , previousIndent = indent
                    , levels =
                        Level (newItem :: lvl)
                            :: remaining
                    }
                )

            else if deltaLevel > 0 then
                -- add new level
                ( newCursor
                , ListBuilder
                    { previousLineEmpty = False
                    , previousIndent = indent
                    , levels =
                        Level [ newItem ]
                            :: Level lvl
                            :: remaining
                    }
                )

            else
                -- We've dedent, so we need to first collapse the current level into the one below.
                -- Then add an item to that level
                ( newCursor
                , ListBuilder
                    { previousLineEmpty = False
                    , previousIndent = indent
                    , levels =
                        collapseLevel (abs deltaLevel) builder.levels
                            |> addToLevel newItem
                    }
                )


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
collapseLevel : Int -> List (Level model msg) -> List (Level model msg)
collapseLevel num levels =
    if num == 0 then
        levels

    else
        case levels of
            [] ->
                levels

            (Level topLevel) :: (Level ((ListItem lowerItem) :: lower)) :: remaining ->
                collapseLevel (num - 1) <|
                    Level
                        (ListItem
                            { lowerItem
                                | children = topLevel ++ lowerItem.children
                            }
                            :: lower
                        )
                        :: remaining

            _ ->
                levels


listIcon : Parser Mark.Custom.Context Mark.Custom.Problem ( List (Maybe Int), List String, ListIcon )
listIcon =
    Parser.oneOf
        [ Parser.succeed ( [], [], Arrow )
            |. Parser.oneOf
                [ Parser.token (Parser.Token "->" (Mark.Custom.Expecting "->"))
                , Parser.token (Parser.Token "-->" (Mark.Custom.Expecting "-->"))
                ]
            |. Parser.chompWhile (\c -> c == ' ')
        , Parser.succeed identity
            |. Parser.token (Parser.Token "-" Mark.Custom.Dash)
            |= Parser.oneOf
                [ Parser.succeed ( [], [], Bullet )
                    |. Parser.oneOf
                        [ Parser.token (Parser.Token " " Mark.Custom.Space)
                        , Parser.token (Parser.Token "-" Mark.Custom.Dash)
                        ]
                    |. Parser.chompWhile (\c -> c == ' ' || c == '-')
                , Parser.succeed
                    (\( reset, decorations ) ->
                        ( reset, decorations, Number )
                    )
                    |= Parser.loop ( [], [] ) listIndex
                    |. Parser.token (Parser.Token " " Mark.Custom.Space)
                    |. Parser.chompWhile (\c -> c == ' ')
                ]
        ]


indentedInlines indent inlines alreadyFound =
    case alreadyFound of
        [] ->
            Parser.succeed (\new -> Parser.Loop (new :: alreadyFound))
                |= inlines

        _ ->
            Parser.oneOf
                [ Parser.succeed (\new -> Parser.Loop (new :: alreadyFound))
                    |. Parser.token (Parser.Token (String.repeat (4 * (indent + 1)) " ") Mark.Custom.ExpectedIndent)
                    |= inlines
                , Parser.succeed
                    (Parser.Done (List.reverse alreadyFound))
                ]


listIndex ( cursorReset, decorations ) =
    Parser.oneOf
        [ Parser.succeed
            (\reset decoration ->
                Parser.Loop
                    ( reset :: cursorReset
                    , decoration :: decorations
                    )
            )
            |= Parser.oneOf
                [ Parser.succeed
                    (\lead remaining ->
                        case ( String.toInt lead, String.toInt remaining ) of
                            ( Just l, Just r ) ->
                                Just <| (l * 10 * String.length remaining) + r

                            ( Just l, Nothing ) ->
                                Just l

                            _ ->
                                Nothing
                    )
                    |= Parser.getChompedString (Parser.chompIf Char.isDigit Mark.Custom.Integer)
                    |= Parser.getChompedString (Parser.chompWhile Char.isDigit)
                , Parser.succeed Nothing
                    |. Parser.chompIf Char.isAlpha Mark.Custom.ExpectingAlphaNumeric
                    |. Parser.chompWhile Char.isAlpha
                ]
            |= Parser.getChompedString
                (Parser.chompWhile
                    (\c ->
                        c
                            /= ' '
                            && not (Char.isAlpha c)
                            && not (Char.isDigit c)
                    )
                )
        , Parser.succeed
            (Parser.Done
                ( List.reverse cursorReset
                , List.reverse decorations
                )
            )
        ]


decorate : List String -> Cursor -> List { index : Int, decoration : String, show : Bool }
decorate decorations cursor =
    let
        cursorList =
            mapCursor identity cursor
    in
    cursorList
        |> List.foldl applyDecoration ( List.reverse decorations, [] )
        |> Tuple.second


applyDecoration index ( decs, decorated ) =
    case decs of
        [] ->
            -- If there are no decorations, skip.
            ( decs
            , { index = index
              , decoration = ""
              , show = False
              }
                :: decorated
            )

        currentDec :: remaining ->
            ( remaining
            , { index = index
              , decoration = currentDec
              , show = True
              }
                :: decorated
            )


resetCursor reset cursor =
    case List.reverse reset of
        [] ->
            cursor

        top :: remaining ->
            { current =
                Maybe.withDefault cursor.current top
            , stack =
                cursor.stack
                    |> List.foldr resetStack ( remaining, [] )
                    |> Tuple.second
            }


resetStack index ( reset, found ) =
    case reset of
        [] ->
            ( reset, index :: found )

        Nothing :: remain ->
            ( remain, index :: found )

        (Just new) :: remain ->
            ( remain, new :: found )


advanceCursor indent cursor =
    if indent == List.length cursor.stack + 1 then
        { current = cursor.current + 1
        , stack = cursor.stack
        }

    else if indent > List.length cursor.stack + 1 then
        { current = 1
        , stack = cursor.current :: cursor.stack
        }

    else
        let
            indentDelta =
                List.length cursor.stack
                    - indent
        in
        case List.drop (abs indentDelta) cursor.stack of
            [] ->
                cursor

            lower :: remaining ->
                { current = lower + 1
                , stack = remaining
                }
