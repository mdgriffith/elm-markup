module Mark exposing
    ( parse, parseWith
    , Options, Styling, default, defaultStyling, defaultBlocks
    , Index, ListIcon(..), defaultListIcon
    )

{-|

@docs parse, parseWith

@docs Options, Styling, default, defaultStyling, defaultBlocks

@docs Index, ListIcon, defaultListIcon

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region
import Html.Attributes
import Internal.Model as Internal
import Mark.Custom as Custom
import Parser exposing ((|.), (|=), Parser)


{-| -}
parse : String -> Result (List Parser.DeadEnd) (Element msg)
parse source =
    parseWith default source
        |> Result.map (\x -> x ())


{-| -}
parseWith :
    Options model
        { a
            | link : List (Element.Attribute msg)
            , token : List (Element.Attribute msg)
            , list : List (Element.Attribute msg)
            , root : List (Element.Attribute msg)
            , block : List (Element.Attribute msg)
        }
        msg
    -> String
    -> Result (List Parser.DeadEnd) (model -> Element msg)
parseWith options source =
    Parser.run (Internal.markup options) source


{-| -}
type alias Options model styling msg =
    Internal.Options model styling msg


{-| Styling options for the default blocks.

If you add custom `blocks` or `inlines`, you'll probably want to define a new `Styling` type.

-}
type alias Styling msg =
    { link : List (Element.Attribute msg)
    , token : List (Element.Attribute msg)
    , list : List (Element.Attribute msg)
    , listIcons : List Index -> ListIcon -> Element msg
    , title : List (Element.Attribute msg)
    , header : List (Element.Attribute msg)
    , monospace : List (Element.Attribute msg)
    , root : List (Element.Attribute msg)
    , block : List (Element.Attribute msg)
    }


{-| A default set of block and inline elements as well as some `defaultStyling` to style them.
-}
default : Options model (Styling msg) msg
default =
    { styling =
        always defaultStyling
    , blocks =
        defaultBlocks
    , inlines =
        []
    }


{-| -}
defaultStyling : Styling msg
defaultStyling =
    { root =
        [ Element.spacing 24
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

            -- , Font.underline
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
    , listIcons = defaultListIcon
    , list =
        [ Element.spacing 8
        ]
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


cursorLevel ( current, nested ) =
    List.length nested + 1


mapCursor fn cursor =
    List.map fn (cursor.current :: cursor.stack)


{-| -}
defaultListIcon : List Index -> ListIcon -> Element msg
defaultListIcon cursor symbol =
    let
        pad =
            Element.paddingEach
                { edges
                    | left =
                        case List.length cursor of
                            1 ->
                                28

                            2 ->
                                56

                            3 ->
                                84

                            _ ->
                                84
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


{-| A set of common default blocks.

  - `title` - The title of your document. This is equivalent to an `h1`. You should only have one of them.
  - `header` - A header in your document, which is equivalent to `h2`.
  - `list` - A nested list with an expected indentation of 4 spaces per level. As far as icons:
      - `-` indicates a bullet
      - `->` indicates an arrow
      - `1.` indicates it should be numbered. Any number can work.
  - `image` - Expects two strings, first the src, and then a description of the image.
  - `monospace` - Basically a code block without syntax highlighting.

**Note** none of these are special, they're all defined in terms of `Mark.Custom`.

-}
defaultBlocks :
    List
        (Internal.Block model
            { a
                | title : List (Element.Attribute msg)
                , header : List (Element.Attribute msg)
                , link : List (Element.Attribute msg)
                , list : List (Element.Attribute msg)
                , root : List (Element.Attribute msg)
                , monospace : List (Element.Attribute msg)
                , block : List (Element.Attribute msg)
                , listIcons : List Index -> ListIcon -> Element msg
                , token : List (Element.Attribute msg)
            }
            msg
        )
defaultBlocks =
    [ Custom.paragraph "title"
        (\elements styling model ->
            Element.paragraph
                (Element.Region.heading 1
                    :: styling.title
                )
                elements
        )
    , Custom.paragraph "header"
        (\elements styling model ->
            Element.paragraph
                (Element.Region.heading 2
                    :: styling.header
                )
                elements
        )
    , Custom.indented "monospace"
        (\string styling model ->
            Element.paragraph
                (Element.htmlAttribute (Html.Attributes.style "line-height" "1.4em")
                    :: Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
                    :: styling.monospace
                )
                [ Element.text string ]
        )
    , Custom.block2 "image"
        (\src description styling model ->
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
type alias Cursor =
    { current : Int
    , stack : List Int
    }


emptyCursor : Cursor
emptyCursor =
    { current = 0
    , stack = []
    }


{-| -}
type ListIcon
    = Bullet
    | Arrow
    | Number


{-| -}
type CursorReset
    = CursorReset (List (Maybe Int))


{-| -}
indentLevel : Parser Int
indentLevel =
    Parser.oneOf
        [ Parser.token (String.repeat 12 " ")
            |> Parser.map (always 3)
        , Parser.token (String.repeat 8 " ")
            |> Parser.map (always 2)
        , Parser.token (String.repeat 4 " ")
            |> Parser.map (always 1)
        ]


list inlines =
    Parser.loop
        ( emptyCursor, [] )
        (listItem inlines)


listItem inlines ( cursor, existing ) =
    Parser.oneOf
        [ Parser.succeed
            (\indent ( reset, decorations, token ) styledText ->
                let
                    newCursor =
                        cursor
                            |> advanceCursor indent
                            |> resetCursor reset

                    indexedDecorations =
                        decorate decorations newCursor
                in
                Parser.Loop
                    ( newCursor
                    , (\styling model ->
                        Element.paragraph
                            []
                            (styling.listIcons indexedDecorations token :: styledText styling model)
                      )
                        :: existing
                    )
            )
            |= indentLevel
            |= Parser.oneOf
                [ Parser.token "->"
                    |> Parser.map (always ( [], [], Arrow ))
                , Parser.token "-"
                    |> Parser.map (always ( [], [], Bullet ))
                , Parser.succeed
                    (\( reset, decorations ) ->
                        ( reset, decorations, Number )
                    )
                    |= Parser.loop ( [], [] ) listIndex
                ]
            |. Parser.token " "
            |= inlines
        , Parser.end
            |> Parser.map
                (\_ ->
                    Parser.Done
                        (\styling model ->
                            Element.column styling.list
                                (List.foldl
                                    (\fn els ->
                                        fn styling model :: els
                                    )
                                    []
                                    existing
                                )
                        )
                )
        , Parser.token "\n\n"
            |> Parser.map
                (\_ ->
                    Parser.Done
                        (\styling model ->
                            Element.column styling.list
                                (List.foldl
                                    (\fn els ->
                                        fn styling model :: els
                                    )
                                    []
                                    existing
                                )
                        )
                )
        , Parser.token "\n"
            |> Parser.map (always (Parser.Loop ( cursor, existing )))
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
                    |= Parser.getChompedString (Parser.chompIf Char.isDigit)
                    |= Parser.getChompedString (Parser.chompWhile Char.isDigit)
                , Parser.succeed Nothing
                    |. Parser.chompIf Char.isAlpha
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
