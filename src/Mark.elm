module Mark exposing (Options, Styling, defaultBlocks, defaultOptions, defaultStyling, parse, parseWith)

{-|

@docs parse, parseWith

@docs Options, defaultOptions

@docs Styling, Cursor, ListIcon, defaultStyling

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
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
            , list : List (Element.Attribute msg)
            , blockSpacing : Int
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
    , blockSpacing : Int
    , token : List (Element.Attribute msg)
    , list : List (Element.Attribute msg)
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
    , inlines =
        []
    }


{-| -}
defaultStyling : Styling msg
defaultStyling =
    { blockSpacing = 24
    , list =
        [ Element.spacing 8
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
    , header =
        \level ->
            case level of
                One ->
                    [ Font.size 48 ]

                Two ->
                    [ Font.size 36 ]

                Three ->
                    [ Font.size 20 ]
    }


edges =
    { top = 0
    , left = 0
    , right = 0
    , bottom = 0
    }


{-| -}
defaultListToken : Cursor -> ListIcon -> Element msg
defaultListToken cursor symbol =
    let
        pad =
            Element.paddingEach
                { edges
                    | left =
                        case cursor.level of
                            One ->
                                28

                            Two ->
                                56

                            Three ->
                                84
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
                    case cursor.level of
                        One ->
                            "•"

                        Two ->
                            "◦"

                        Three ->
                            "◦"
            in
            Element.el [ pad ] (Element.text icon)

        Number ->
            case cursor.level of
                One ->
                    Element.el [ pad ]
                        (Element.text (String.fromInt cursor.one ++ "."))

                Two ->
                    Element.el [ pad ]
                        (Element.text
                            (String.fromInt cursor.one
                                ++ "."
                                ++ String.fromInt cursor.two
                            )
                        )

                Three ->
                    Element.el [ pad ]
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
                , list : List (Element.Attribute msg)
                , blockSpacing : Int
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
    = Bullet
    | Number
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


list options =
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
                                (options.styling.listIcons newCursor token :: el)
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
                    |= Internal.text options
                , Parser.end
                    |> Parser.map
                        (\_ ->
                            Parser.Done (\_ -> Element.column options.styling.list (List.reverse existing))
                        )
                , Parser.token "\n\n"
                    |> Parser.map
                        (\_ ->
                            Parser.Done (\_ -> Element.column options.styling.list (List.reverse existing))
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
