module Mark.Default exposing
    ( default
    , blocks, inline
    , ListIcon(..), Index
    , root, paragraph
    , title, header, list, image, monospace
    , replacements
    , listIcon
    )

{-|

@docs default

@docs blocks, inline
@docs ListIcon, Index


# Default Mergers

@docs root, paragraph


# Default Blocks

@docs title, header, list, image, monospace


# Character Replacements

@docs replacements

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region
import Html.Attributes
import Internal.Model as Internal
import Mark.Custom as Custom
import Mark.Error
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


{-| A default set of block and inline elements as well as some `defaultStyling` to style them.
-}
default : Internal.Options (model -> Element msg)
default =
    { blocks =
        blocks
    , merge =
        root
            [ Element.spacing 64
            , Element.width (Element.px 700)
            , Element.centerX
            , Element.padding 100
            ]
    , inlines =
        { view = inline
        , inlines = []
        , merge = paragraph []
        , replacements =
            replacements
        }
    }


inline : Internal.TextFormatting -> Maybe Internal.Link -> model -> Element msg
inline format link model =
    case format of
        Internal.NoFormatting txt ->
            case link of
                Nothing ->
                    Element.text txt

                Just { url } ->
                    Element.link []
                        { url = url
                        , label = Element.text txt
                        }

        Internal.Styles styles txt ->
            case link of
                Nothing ->
                    Element.el (List.filterMap toStyles styles) (Element.text txt)

                Just { url } ->
                    Element.link (List.filterMap toStyles styles)
                        { url = url
                        , label = Element.text txt
                        }

        Internal.Fragments frags ->
            case link of
                Nothing ->
                    Element.row []
                        (List.map renderFragment frags)

                Just { url } ->
                    Element.link []
                        { url = url
                        , label =
                            Element.row []
                                (List.map renderFragment frags)
                        }


paragraph attrs els model =
    Element.paragraph attrs (List.map (\el -> el model) els)


root attrs els model =
    Element.textColumn attrs (List.map (\el -> el model) els)


replacements =
    [ Internal.Replacement "..." "…"
    , Internal.Replacement "<>" "\u{00A0}"
    , Internal.Replacement "---" "—"
    , Internal.Replacement "--" "–"
    , Internal.Replacement "'" "’"
    , Internal.Balanced
        { start = ( "\"", "“" )
        , end = ( "\"", "”" )
        }
    ]


renderFragment frag =
    Element.el (List.filterMap toStyles frag.styles) (Element.text frag.text)


toStyles style =
    case style of
        Internal.NoStyleChange ->
            Nothing

        Internal.Bold ->
            Just Font.bold

        Internal.Italic ->
            Just Font.italic

        Internal.Strike ->
            Just Font.strike

        Internal.Underline ->
            Just Font.underline

        Internal.Token ->
            --Just Font.bold
            Nothing


{-| A set of common default blocks.

  - `title` - The title of your document. This is equivalent to an `h1`. You should only have one of them.
  - `header` - A header in your document, which is equivalent to `h2`.
  - `list` - A nested list with an expected indentation of 4 spaces per level. As far as icons:
      - `-` indicates a bullet
      - `->` or `-->` indicates an arrow
      - `-x.` means auto numbering. Any lowercase letter can work.
      - `-1.` means start autonumbering at this exact number. Any number can work.
  - `image` - Expects two strings, first the src, and then a description of the image.
  - `monospace` - Basically a code block without syntax highlighting.

**Note** none of these are special, they're all defined in terms of `Mark.Custom`.

-}
blocks : List (Custom.Block (model -> Element msg))
blocks =
    [ title [ Font.size 48 ]
    , header [ Font.size 36 ]
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
    , image []
    , list
        { style = listStyles
        , icon = listIcons
        }
    ]


{-| -}
title : List (Element.Attribute msg) -> Custom.Block (model -> Element msg)
title attrs =
    Custom.paragraph "Title"
        (\elements model ->
            Element.paragraph
                (Element.Region.heading 1 :: attrs)
                (List.map (\viewEl -> viewEl model) elements)
        )


{-| -}
header : List (Element.Attribute msg) -> Custom.Block (model -> Element msg)
header attrs =
    Custom.paragraph "Header"
        (\elements model ->
            Element.paragraph
                (Element.Region.heading 2 :: attrs)
                (List.map (\viewEl -> viewEl model) elements)
        )


{-| -}
image : List (Element.Attribute msg) -> Custom.Block (model -> Element msg)
image attrs =
    Custom.block2 "Image"
        (\src description model ->
            Element.image
                attrs
                { src = String.trim src
                , description = String.trim description
                }
        )
        Custom.string
        Custom.string


{-| -}
monospace : List (Element.Attribute msg) -> Custom.Block (model -> Element msg)
monospace attrs =
    Custom.indented "Monospace"
        (\string model ->
            Element.paragraph
                (Element.htmlAttribute (Html.Attributes.style "line-height" "1.4em")
                    :: Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
                    :: attrs
                )
                [ Element.text string ]
        )


{-| Parse a nested list
-}
list :
    { icon : List Index -> ListIcon -> Element msg
    , style : List Index -> List (Element.Attribute msg)
    }
    -> Custom.Block (model -> Element msg)
list listConfig =
    Custom.parser "List"
        (listParser listConfig)



{- LIST -}


{-| -}
listStyles : List Index -> List (Element.Attribute msg)
listStyles cursor =
    case List.length cursor of
        0 ->
            -- top level element
            [ Element.spacing 64 ]

        1 ->
            [ Element.spacing 16 ]

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
indentLevel : Parser Mark.Error.Context Mark.Error.Problem Int
indentLevel =
    Parser.oneOf
        [ Parser.token (Parser.Token (String.repeat 12 " ") Mark.Error.ExpectedIndent)
            |> Parser.map (always 3)
        , Parser.token (Parser.Token (String.repeat 8 " ") Mark.Error.ExpectedIndent)
            |> Parser.map (always 2)
        , Parser.token (Parser.Token (String.repeat 4 " ") Mark.Error.ExpectedIndent)
            |> Parser.map (always 1)
        ]


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


listParser :
    { icon : List Index -> ListIcon -> Element msg
    , style : List Index -> List (Element.Attribute msg)
    }
    -> Parser Mark.Error.Context Mark.Error.Problem (List (model -> Element msg))
    -> Parser Mark.Error.Context Mark.Error.Problem (model -> Element msg)
listParser config inlines =
    Parser.loop
        ( emptyCursor, emptyListBuilder )
        (listItem config inlines)


finalizeList config (ListBuilder builder) model =
    Element.column
        []
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
    -> Parser Mark.Error.Context Mark.Error.Problem (List (model -> Element msg))
    -> ( Cursor, ListBuilder model msg )
    -> Parser Mark.Error.Context Mark.Error.Problem (Parser.Step ( Cursor, ListBuilder model msg ) (model -> Element msg))
listItem config inlines ( cursor, ListBuilder builder ) =
    Parser.oneOf
        [ Parser.succeed identity
            |= indentLevel
            |> Parser.andThen
                (\indent ->
                    Parser.map Parser.Loop
                        (indentedListItem inlines cursor (ListBuilder builder) indent)
                )
        , Parser.end Mark.Error.End
            |> Parser.map
                (\_ ->
                    Parser.Done (finalizeList config (ListBuilder builder))
                )
        , if builder.previousLineEmpty then
            Parser.token (Parser.Token "\n" Mark.Error.Newline)
                |> Parser.map
                    (\_ ->
                        Parser.Done (finalizeList config (ListBuilder builder))
                    )

          else
            Parser.token (Parser.Token "\n" Mark.Error.Newline)
                |> Parser.map
                    (always
                        (Parser.Loop ( cursor, ListBuilder { builder | previousLineEmpty = True } ))
                    )
        ]


{-| Parse a complete list item.

    -> start of item

    -> newline
        -> if previousLineEmpty -> done

We already know the indent.

-}
indentedListItem :
    Parser Mark.Error.Context Mark.Error.Problem (List (model -> Element msg))
    -> Cursor
    -> ListBuilder model msg
    -> Int
    -> Parser Mark.Error.Context Mark.Error.Problem ( { current : Int, stack : List Int }, ListBuilder model msg )
indentedListItem inlines cursor (ListBuilder builder) indent =
    Parser.oneOf <|
        List.filterMap identity
            [ Just
                (Parser.succeed (addItem cursor indent (ListBuilder builder))
                    |= Parser.map Just listIcon
                    |. Parser.chompWhile (\c -> c == ' ')
                    |= inlines
                )
            , Just
                (Parser.succeed (addItem cursor indent (ListBuilder builder) Nothing)
                    |= inlines
                )
            , if builder.previousLineEmpty then
                Nothing

              else
                Just
                    (Parser.succeed
                        ( cursor
                        , ListBuilder
                            { builder | previousLineEmpty = True }
                        )
                        |. Parser.token (Parser.Token "\n" Mark.Error.Newline)
                    )
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


listIcon : Parser Mark.Error.Context Mark.Error.Problem ( List (Maybe Int), List String, ListIcon )
listIcon =
    Parser.oneOf
        [ Parser.succeed ( [], [], Arrow )
            |. Parser.oneOf
                [ Parser.token (Parser.Token "->" (Mark.Error.Expecting "->"))
                , Parser.token (Parser.Token "-->" (Mark.Error.Expecting "-->"))
                ]
            |. Parser.chompWhile (\c -> c == ' ')
        , Parser.succeed identity
            |. Parser.token (Parser.Token "-" Mark.Error.Dash)
            |= Parser.oneOf
                [ Parser.succeed ( [], [], Bullet )
                    |. Parser.oneOf
                        [ Parser.token (Parser.Token " " Mark.Error.Space)
                        , Parser.token (Parser.Token "-" Mark.Error.Dash)
                        ]
                    |. Parser.chompWhile (\c -> c == ' ' || c == '-')
                , Parser.succeed
                    (\( reset, decorations ) ->
                        ( reset, decorations, Number )
                    )
                    |= Parser.loop ( [], [] ) listIndex
                    |. Parser.token (Parser.Token " " Mark.Error.Space)
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
                    |. Parser.token (Parser.Token (String.repeat (4 * (indent + 1)) " ") Mark.Error.ExpectedIndent)
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
                    |= Parser.getChompedString (Parser.chompIf Char.isDigit Mark.Error.Integer)
                    |= Parser.getChompedString (Parser.chompWhile Char.isDigit)
                , Parser.succeed Nothing
                    |. Parser.chompIf Char.isAlpha Mark.Error.ExpectingAlphaNumeric
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
