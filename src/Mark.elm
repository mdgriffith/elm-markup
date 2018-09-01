module Mark exposing
    ( parse, parseWith
    , Options, Styling, default, defaultStyling, defaultBlocks
    , Cursor, ListIcon(..)
    )

{-|

@docs parse, parseWith

@docs Options, Styling, default, defaultStyling, defaultBlocks

@docs Cursor, ListIcon

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
    , listIcons : Cursor -> ListIcon -> Element msg
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
    , listIcons = defaultListToken
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


mapCursor fn ( head, tail ) =
    List.map fn (head :: tail)


{-| -}
defaultListToken : Cursor -> ListIcon -> Element msg
defaultListToken cursor symbol =
    let
        pad =
            Element.paddingEach
                { edges
                    | left =
                        case cursorLevel cursor of
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
                    case cursorLevel cursor of
                        1 ->
                            "•"

                        _ ->
                            "◦"
            in
            Element.el [ pad ] (Element.text icon)

        Number ->
            Element.el [ pad ]
                (Element.text
                    (String.join "."
                        (mapCursor String.fromInt cursor)
                    )
                )


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
                , listIcons : Cursor -> ListIcon -> Element msg
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


{-| A Cursor which represents a position in a nested list.

`Level`

-}
type alias Cursor =
    ( Int, List Int )


emptyCursor : Cursor
emptyCursor =
    ( 1, [] )


{-| -}
type ListIcon
    = Bullet
    | Number
    | Arrow


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
        (\( cursor, existing ) ->
            Parser.oneOf
                [ Parser.succeed
                    (\indent token styledText ->
                        let
                            newCursor =
                                advanceCursor cursor indent
                        in
                        Parser.Loop
                            ( newCursor
                            , (\styling model ->
                                Element.paragraph
                                    []
                                    (styling.listIcons newCursor token :: styledText styling model)
                              )
                                :: existing
                            )
                    )
                    |= indentLevel
                    |= Parser.oneOf
                        [ Parser.token "->"
                            |> Parser.map (always Arrow)
                        , Parser.token "-"
                            |> Parser.map (always Bullet)
                        , Parser.succeed Number
                            |. Parser.chompIf Char.isDigit
                            |. Parser.chompWhile Char.isDigit
                            |. Parser.chompIf (\c -> c == '.')
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
        )


advanceCursor ( current, nested ) indent =
    if indent == List.length nested + 1 then
        ( current + 1, nested )

    else if indent > List.length nested + 1 then
        ( 1, current :: nested )

    else
        case nested of
            [] ->
                ( current, nested )

            lower :: remaining ->
                ( lower + 1, remaining )
