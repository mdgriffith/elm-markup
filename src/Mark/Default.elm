module Mark.Default exposing
    ( blocks
    , Styling, styling
    , ListIcon(..), Index, listIcon
    , listIcons, listStyles
    )

{-|

@docs blocks
@docs Styling, styling
@docs ListIcon, Index, listIcon

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region
import Html.Attributes
import Internal.Model as Internal
import Mark.Custom as Custom
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


{-| Styling options for the default blocks.

If you add custom `blocks` or `inlines`, you'll probably want to define a new `Styling` type.

-}
type alias Styling msg =
    { link : List (Element.Attribute msg)
    , token : List (Element.Attribute msg)
    , list : List Index -> List (Element.Attribute msg)
    , listIcons : List Index -> ListIcon -> Element msg
    , title : List (Element.Attribute msg)
    , header : List (Element.Attribute msg)
    , monospace : List (Element.Attribute msg)
    , root : List (Element.Attribute msg)
    , block : List (Element.Attribute msg)
    }


{-| -}
styling : Styling msg
styling =
    { root =
        [ Element.spacing 64
        , Element.width (Element.px 700)
        , Element.centerX
        , Element.padding 100
        ]
    , block = []
    , monospace =
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
    , token =
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
    , list = listStyles
    , listIcons = listIcons
    , title =
        [ Font.size 48 ]
    , header =
        [ Font.size 36 ]
    }


edges =
    { top = 0
    , left = 0
    , right = 0
    , bottom = 0
    }


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
blocks :
    List
        (Internal.Block model
            { a
                | title : List (Element.Attribute msg)
                , header : List (Element.Attribute msg)
                , link : List (Element.Attribute msg)
                , list : List Index -> List (Element.Attribute msg)
                , listIcons : List Index -> ListIcon -> Element msg
                , root : List (Element.Attribute msg)
                , monospace : List (Element.Attribute msg)
                , block : List (Element.Attribute msg)
                , token : List (Element.Attribute msg)
            }
            msg
        )
blocks =
    [ Custom.paragraph "title"
        (\elements myStyling model ->
            Element.paragraph
                (Element.Region.heading 1
                    :: myStyling.title
                )
                elements
        )
    , Custom.paragraph "header"
        (\elements myStyling model ->
            Element.paragraph
                (Element.Region.heading 2
                    :: myStyling.header
                )
                elements
        )
    , Custom.indented "monospace"
        (\string myStyling model ->
            Element.paragraph
                (Element.htmlAttribute (Html.Attributes.style "line-height" "1.4em")
                    :: Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
                    :: myStyling.monospace
                )
                [ Element.text string ]
        )
    , Custom.block2 "image"
        (\src description myStyling model ->
            Element.image
                []
                { src = String.trim src
                , description = String.trim description
                }
        )
        Custom.string
        Custom.string
    , Custom.parser "list"
        list
    ]



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
type ListIcon
    = Bullet
    | Arrow
    | Number


{-| -}
type CursorReset
    = CursorReset (List (Maybe Int))


{-| -}
indentLevel : Parser Internal.Context Internal.Problem Int
indentLevel =
    Parser.oneOf
        [ Parser.token (Parser.Token (String.repeat 12 " ") Internal.ExpectedIndent)
            |> Parser.map (always 3)
        , Parser.token (Parser.Token (String.repeat 8 " ") Internal.ExpectedIndent)
            |> Parser.map (always 2)
        , Parser.token (Parser.Token (String.repeat 4 " ") Internal.ExpectedIndent)
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
type ListBuilder styling model msg
    = ListBuilder
        { previousIndent : Int
        , previousLineEmpty : Bool
        , levels :
            -- (mostRecent :: remaining)
            List (Level styling model msg)
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


type Level styling model msg
    = Level (List (ListItem styling model msg))


{-| -}
type ListItem styling model msg
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
        , content : styling -> model -> List (Element msg)
        , children :
            -- (mostRecent :: remaining)
            List (ListItem styling model msg)
        }


emptyListBuilder : ListBuilder styling model msg
emptyListBuilder =
    ListBuilder
        { previousIndent = 0
        , previousLineEmpty = False
        , levels = []
        }


list inlines =
    Parser.loop
        ( emptyCursor, emptyListBuilder )
        (listItem inlines)


finalizeList (ListBuilder builder) myStyling model =
    Element.column
        (myStyling.list [])
        (renderLevels myStyling model builder.levels)


renderLevels myStyling model levels =
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
                        |> List.map (renderListItem myStyling model)


renderListItem myStyling model (ListItem item) =
    case item.icon of
        Nothing ->
            case item.children of
                [] ->
                    renderParagraph myStyling model item.content

                _ ->
                    Element.column (myStyling.list [])
                        (renderParagraph myStyling model item.content
                            :: (item.children
                                    |> List.reverse
                                    |> List.map (renderListItem myStyling model)
                               )
                        )

        Just actualIcon ->
            Element.row []
                [ Element.el [ Element.alignTop ] <|
                    myStyling.listIcons actualIcon.decorations actualIcon.token
                , Element.textColumn (myStyling.list actualIcon.decorations)
                    (renderParagraph myStyling model item.content
                        :: (item.children
                                |> List.reverse
                                |> List.map (renderListItem myStyling model)
                           )
                    )
                ]


renderParagraph myStyling model content =
    Element.paragraph
        []
        (content myStyling model)


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



-- listItem :
--     Parser
--         ({ styling
--             | list : List Index -> List (Element.Attribute msg)
--             , listIcons : List Index -> ListIcon -> Element msg
--          }
--          -> model
--          -> List (Element msg)
--         )
--     ->
--         ( Cursor
--         , ListBuilder
-- { styling
--     | list : List Index -> List (Element.Attribute msg)
--     , listIcons : List Index -> ListIcon -> Element msg
-- }
-- model
-- msg
--         )
--     ->
--         Parser.Parser
--             (Parser.Step
--                 ( Cursor
--                 , ListBuilder
--                     { styling
--                         | list : List Index -> List (Element.Attribute msg)
--                         , listIcons : List Index -> ListIcon -> Element msg
--                     }
--                     model
--                     msg
--                 )
-- ({ styling
--     | list : List Index -> List (Element.Attribute msg)
--     , listIcons : List Index -> ListIcon -> Element msg
--  }
--  -> model
--  -> Element msg
-- )
--             )


listItem inlines ( cursor, ListBuilder builder ) =
    Parser.oneOf
        [ Parser.succeed identity
            |= indentLevel
            |> Parser.andThen
                (\indent ->
                    Parser.map Parser.Loop
                        (indentedListItem inlines cursor (ListBuilder builder) indent)
                )
        , Parser.end Internal.End
            |> Parser.map
                (\_ ->
                    Parser.Done (finalizeList (ListBuilder builder))
                )
        , if builder.previousLineEmpty then
            Parser.token (Parser.Token "\n" Internal.Newline)
                |> Parser.map
                    (\_ ->
                        Parser.Done (finalizeList (ListBuilder builder))
                    )

          else
            Parser.token (Parser.Token "\n" Internal.Newline)
                |> Parser.map
                    (always
                        (Parser.Loop ( cursor, ListBuilder { builder | previousLineEmpty = True } ))
                    )
        ]



-- indentedListItem :
--     Parser
--         (ListStyling styling msg
--          -> model
--          -> List (Element msg)
--         )
--     -> Cursor
--     ->
--         ListBuilder
--             (ListStyling styling msg)
--             model
--             msg
--     -> Int
--     ->
--         Parser.Parser
--             (Parser.Step
--                 ( Cursor
--                 , ListBuilder
--                     (ListStyling styling msg)
--                     model
--                     msg
--                 )
--                 (ListStyling styling msg
--                  -> model
--                  -> Element msg
--                 )
--             )


{-| Parse a complete list item.

    -> start of item

    -> newline
        -> if previousLineEmpty -> done

We already know the indent.

-}
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
                        |. Parser.token (Parser.Token "\n" Internal.Newline)
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


listIcon =
    Parser.oneOf
        [ Parser.succeed ( [], [], Arrow )
            |. Parser.oneOf
                [ Parser.token (Parser.Token "->" (Internal.Expecting "->"))
                , Parser.token (Parser.Token "-->" (Internal.Expecting "-->"))
                ]
            |. Parser.chompWhile (\c -> c == ' ')
        , Parser.succeed identity
            |. Parser.token (Parser.Token "-" Internal.Dash)
            |= Parser.oneOf
                [ Parser.succeed ( [], [], Bullet )
                    |. Parser.oneOf
                        [ Parser.token (Parser.Token " " Internal.Space)
                        , Parser.token (Parser.Token "-" Internal.Dash)
                        ]
                    |. Parser.chompWhile (\c -> c == ' ' || c == '-')
                , Parser.succeed
                    (\( reset, decorations ) ->
                        ( reset, decorations, Number )
                    )
                    |= Parser.loop ( [], [] ) listIndex
                    |. Parser.token (Parser.Token " " Internal.Space)
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
                    |. Parser.token (Parser.Token (String.repeat (4 * (indent + 1)) " ") Internal.ExpectedIndent)
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
                    |= Parser.getChompedString (Parser.chompIf Char.isDigit Internal.Integer)
                    |= Parser.getChompedString (Parser.chompWhile Char.isDigit)
                , Parser.succeed Nothing
                    |. Parser.chompIf Char.isAlpha Internal.ExpectingAlphaNumeric
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
