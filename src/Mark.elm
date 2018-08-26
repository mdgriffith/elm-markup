module Mark exposing (Options, Styling, defaultBlocks, defaultOptions, defaultStyling, parse, parseWith)

{-|

@docs parse, parseWith

@docs Options, defaultOptions

@docs Styling, Cursor, ListIcon, defaultStyling

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Internal.Model as Internal
import Mark.Custom as Custom
import Parser exposing ((|.), (|=), Parser)


{-| -}
parse : String -> Result (List Parser.DeadEnd) (Element msg)
parse =
    parseWith defaultOptions


{-| -}
parseWith :
    Options
        { a
            | link : List (Element.Attribute msg)
            , token : List (Element.Attribute msg)
        }
        msg
    -> String
    -> Result (List Parser.DeadEnd) (Element msg)
parseWith options source =
    Parser.run (Internal.markup options) source


{-| -}
type alias Options styling msg =
    Internal.Options styling msg


{-| -}
type alias Styling msg =
    { link : List (Element.Attribute msg)
    , token : List (Element.Attribute msg)
    , listIcons : Cursor -> ListIcon -> Element msg
    , header : Level -> List (Element.Attribute msg)
    }


{-| -}
defaultOptions : Options (Styling msg) msg
defaultOptions =
    { styling =
        defaultStyling
    , blocks =
        defaultBlocks
    }


{-| -}
defaultStyling : Styling msg
defaultStyling =
    { link = []
    , token =
        [ Background.color (Element.rgb 0.92 0.92 0.92)
        , Border.rounded 3
        , Element.paddingXY 4 2
        ]
    , listIcons = defaultListToken
    , header = always []
    }


{-| -}
defaultListToken : Cursor -> ListIcon -> Element msg
defaultListToken cursor symbol =
    case symbol of
        Arrow ->
            Element.el [] (Element.text "➙")

        Dash ->
            Element.el [] (Element.text "•")

        Plus ->
            case cursor.level of
                One ->
                    Element.el []
                        (Element.text (String.fromInt cursor.one))

                Two ->
                    Element.el []
                        (Element.text
                            (String.fromInt cursor.one
                                ++ "."
                                ++ String.fromInt cursor.two
                            )
                        )

                Three ->
                    Element.el []
                        (Element.text
                            (String.fromInt cursor.one
                                ++ "."
                                ++ String.fromInt cursor.two
                                ++ "."
                                ++ String.fromInt cursor.three
                            )
                        )


defaultBlocks :
    List
        (Internal.Block
            { a
                | header : Level -> List (Element.Attribute msg)
                , link : List (Element.Attribute msg)
                , listIcons :
                    { level : Level
                    , one : Int
                    , three : Int
                    , two : Int
                    }
                    -> ListIcon
                    -> Element msg
                , token : List (Element.Attribute msg)
            }
            msg
        )
defaultBlocks =
    [ Custom.block "title"
        (\elements styling ->
            Element.paragraph
                -- (Element.Region.heading 1 ::
                (styling.header One)
                elements
        )
        |> Custom.styled
    , Custom.block "=>"
        (\elements styling ->
            Element.paragraph
                -- (Element.Region.heading 1 ::
                (styling.header Three)
                elements
        )
        |> Custom.styled
    , Custom.block "header"
        (\elements styling ->
            Element.paragraph
                -- (Element.Region.heading 1 ::
                (styling.header Two)
                elements
        )
        |> Custom.styled
    , Custom.block "image"
        (\src description styling ->
            Element.image
                []
                { src = String.trim src
                , description = String.trim description
                }
        )
        |> Custom.string
        |> Custom.string
        |> Custom.done
    , Custom.parser "list"
        list
    ]



{- LIST -}


{-| -}
type alias Cursor =
    { level : Level
    , one : Int
    , two : Int
    , three : Int
    }


emptyCursor : Cursor
emptyCursor =
    { level = One
    , one = 0
    , two = 0
    , three = 0
    }


type LevelMovement
    = Indenting
    | Dedenting
    | Holding


movement : Level -> Level -> LevelMovement
movement current target =
    let
        currentI =
            levelToInt current

        targetI =
            levelToInt target
    in
    if targetI > currentI then
        Indenting
    else if targetI < currentI then
        Dedenting
    else
        Holding


type ListIcon
    = Dash
    | Plus
    | Arrow


type Level
    = One
    | Two
    | Three


indentLevel : Parser Level
indentLevel =
    Parser.oneOf
        [ Parser.token (String.repeat 12 " ")
            |> Parser.map (always Three)
        , Parser.token (String.repeat 8 " ")
            |> Parser.map (always Two)
        , Parser.token (String.repeat 4 " ")
            |> Parser.map (always One)
        ]


levelToInt : Level -> Int
levelToInt level =
    case level of
        One ->
            1

        Two ->
            2

        Three ->
            3


list styling =
    Parser.loop
        ( emptyCursor, [] )
        (\( cursor, existing ) ->
            Parser.oneOf
                [ Parser.succeed
                    (\indent token el ->
                        let
                            newCursor =
                                advanceCursor cursor indent
                        in
                        Parser.Loop
                            ( newCursor
                            , Element.paragraph
                                []
                                (styling.listIcons newCursor token :: el)
                                :: existing
                            )
                    )
                    |= indentLevel
                    |= Parser.oneOf
                        [ Parser.token "->"
                            |> Parser.map (always Arrow)
                        , Parser.token "-"
                            |> Parser.map (always Dash)
                        , Parser.succeed Plus
                            |. Parser.chompIf Char.isDigit
                            |. Parser.chompWhile Char.isDigit
                            |. Parser.chompIf (\c -> c == '.')
                        ]
                    |. Parser.token " "
                    |= Internal.text styling
                , Parser.end
                    |> Parser.map
                        (\_ ->
                            Parser.Done (\_ -> Element.column [] (List.reverse existing))
                        )
                , Parser.token "\n\n"
                    |> Parser.map
                        (\_ ->
                            Parser.Done (\_ -> Element.column [] (List.reverse existing))
                        )
                , Parser.token "\n"
                    |> Parser.map (always (Parser.Loop ( cursor, existing )))
                ]
        )


advanceCursor cursor indent =
    case movement cursor.level indent of
        Holding ->
            case indent of
                One ->
                    { level = indent
                    , one = cursor.one + 1
                    , two = cursor.two
                    , three = cursor.three
                    }

                Two ->
                    { level = indent
                    , one = cursor.one
                    , two = cursor.two + 1
                    , three = cursor.three
                    }

                Three ->
                    { level = indent
                    , one = cursor.one
                    , two = cursor.two
                    , three = cursor.three + 1
                    }

        Indenting ->
            case indent of
                One ->
                    { level = indent
                    , one = 1
                    , two = 0
                    , three = 0
                    }

                Two ->
                    { level = indent
                    , one = cursor.one
                    , two = 1
                    , three = 0
                    }

                Three ->
                    { level = indent
                    , one = cursor.one
                    , two = cursor.two
                    , three = 1
                    }

        Dedenting ->
            case indent of
                One ->
                    { level = indent
                    , one = cursor.one + 1
                    , two = 0
                    , three = 0
                    }

                Two ->
                    { level = indent
                    , one = cursor.one
                    , two = cursor.two + 1
                    , three = 0
                    }

                Three ->
                    { level = indent
                    , one = cursor.one
                    , two = cursor.two
                    , three = cursor.three + 1
                    }
