module Mark.Error exposing
    ( Error
    , toString, toHtml, Theme(..), toDetails, Details, Text, Range, Position
    , Custom
    )

{-|

@docs Error


# Rendering Errors

@docs toString, toHtml, Theme, toDetails, Details, Text, Range, Position


# Creating Custom Errors

@docs Custom

-}

import Html
import Html.Attributes
import Mark.Internal.Error as Error


{-| -}
type alias Error =
    Error.Rendered


{-| -}
type alias Position =
    { offset : Int
    , line : Int
    , column : Int
    }


{-| -}
type alias Text =
    { text : String
    , bold : Bool
    , underline : Bool
    , color : Maybe String
    }


{-| -}
type alias Range =
    { start : Position
    , end : Position
    }


{-| -}
type alias Details =
    { title : String
    , message : List Text
    , region : Maybe Range
    }


{-| -}
toString : Error -> String
toString error =
    case error of
        Error.Rendered details ->
            formatErrorString
                { title = details.title
                , message = details.message
                }

        Error.Global global ->
            formatErrorString
                { title = global.title
                , message = global.message
                }


formatErrorString error =
    String.toUpper error.title
        ++ "\n\n"
        ++ String.join "" (List.map .text error.message)


{-| -}
toDetails : Error -> Details
toDetails error =
    case error of
        Error.Rendered details ->
            { title = details.title
            , message = details.message
            , region = Just details.region
            }

        Error.Global global ->
            { title = global.title
            , message = global.message
            , region = Nothing
            }


{-| -}
type Theme
    = Dark
    | Light


{-| -}
toHtml : Theme -> Error -> Html.Html msg
toHtml theme error =
    case error of
        Error.Rendered details ->
            formatErrorHtml theme
                { title = details.title
                , message = details.message
                }

        Error.Global global ->
            formatErrorHtml theme
                { title = global.title
                , message = global.message
                }


monospaceFonts =
    "\"SFMono-Regular\",Consolas,\"Liberation Mono\",Menlo,Courier,monospace"


formatErrorHtml theme error =
    Html.div
        [ Html.Attributes.style "color" (foregroundClr theme)
        , Html.Attributes.style "white-space" "pre"
        , Html.Attributes.style "font-family" monospaceFonts
        ]
        [ Html.div
            [ Html.Attributes.style "font-size" "1.4em"
            , Html.Attributes.style "line-height" "2.1em"
            ]
            [ Html.text
                (String.toUpper error.title)
            ]
        , Html.div [ Html.Attributes.style "line-height" "1.4" ]
            (List.map (renderMessageHtml theme) error.message)
        ]


foregroundClr theme =
    case theme of
        Dark ->
            "#eeeeec"

        Light ->
            "rgba(16,16,16, 0.9)"


renderMessageHtml theme message =
    Html.span
        (List.filterMap identity
            [ if message.bold then
                Just (Html.Attributes.style "font-weight" "bold")

              else
                Nothing
            , if message.underline then
                Just (Html.Attributes.style "text-decoration" "underline")

              else
                Nothing
            , case message.color of
                Nothing ->
                    Just <| Html.Attributes.style "color" (foregroundClr theme)

                Just "red" ->
                    Just <| Html.Attributes.style "color" (redClr theme)

                Just "yellow" ->
                    Just <| Html.Attributes.style "color" (yellowClr theme)

                _ ->
                    Nothing
            ]
        )
        [ Html.text message.text ]


redClr theme =
    case theme of
        Dark ->
            "#ef2929"

        Light ->
            "#cc0000"


yellowClr theme =
    case theme of
        Dark ->
            "#edd400"

        Light ->
            "#c4a000"


{-| Create a custom error message to use with [`Mark.verify`](Mark#verify)
-}
type alias Custom =
    { title : String
    , message : List String
    }
