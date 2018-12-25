module Mark.Error exposing (Error, Text, toJson)

{-| -}

-- import Json.Decode as Decode
-- import Json.Encode as Json

import Mark



-- import Parser.Advanced as Parser


{-| -}



-- toJson : String -> List (Parser.DeadEnd Mark.Context Mark.Problem) -> List Error


toJson source errors =
    errors
        |> List.foldl mergeErrors []
        |> List.map (renderErrors (String.lines source))



-- {-| -}
-- toString : String -> List (Parser.DeadEnd Mark.Context Mark.Problem) -> List String
-- toString source errors =
--     errors
--         |> List.foldl mergeErrors []
--         |> List.map (errorToString << renderErrors (String.lines source))
-- errorToString error =
--     String.toUpper error.title
--         ++ "\n"
--         ++ String.join "" (List.map .text error.message)
-- {-| -}
-- toMessages : List (Parser.DeadEnd Mark.Context Mark.Problem) -> List Message
-- toMessages errors =
--     errors
--         |> List.foldl mergeErrors []


{-| -}
type Problem
    = UnknownBlock (List String)
    | UnknownInline (List String)
    | NonMatchingFields
        { expecting : List String
        , found : List String
        }
    | UnexpectedField
        { found : String
        , options : List String
        , recordName : String
        }
    | ExpectingIndent Int
    | CantStartTextWithSpace
    | UnclosedStyle (List Mark.Style)
    | BadDate String
    | IntOutOfRange
        { found : Int
        , min : Int
        , max : Int
        }
    | FloatOutOfRange
        { found : Float
        , min : Float
        , max : Float
        }


type alias Similarity =
    Int


similarity : String -> String -> Similarity
similarity source target =
    let
        length =
            if String.length source == String.length target then
                1

            else
                0

        first str =
            Maybe.map (String.fromChar << Tuple.first) (String.uncons str)
                |> Maybe.withDefault ""

        last str =
            Maybe.map (String.fromChar << Tuple.first) (String.uncons (String.reverse str))
                |> Maybe.withDefault

        firstChar =
            if first source == first target then
                1

            else
                0

        lastChar =
            if first source == first target then
                1

            else
                0

        addCompared ( x, y ) total =
            if x == y then
                total + 1

            else
                total
    in
    -- List.foldl (+) 0 [ length, firstChar, lastChar ]
    List.foldl addCompared 0 (List.map2 Tuple.pair (String.toList source) (String.toList target))


type alias Message =
    { row : Int
    , col : Int
    , problem : Problem
    }


type alias Text =
    { text : String
    , bold : Bool
    , underline : Bool
    , color : Maybe String
    }


type alias Error =
    { message : List Text
    , region : { start : Position, end : Position }
    , title : String
    }


type alias Position =
    { line : Int
    , column : Int
    }


mergeErrors current merged =
    case merged of
        [] ->
            case current.problem of
                Mark.ExpectingBlockName block ->
                    [ { row = current.row
                      , col = current.col
                      , problem =
                            UnknownBlock [ block ]
                      }
                    ]

                Mark.ExpectingInlineName inline ->
                    [ { row = current.row
                      , col = current.col
                      , problem =
                            UnknownInline [ inline ]
                      }
                    ]

                Mark.NonMatchingFields fields ->
                    [ { row = current.row
                      , col = current.col
                      , problem =
                            NonMatchingFields fields
                      }
                    ]

                Mark.UnexpectedField fields ->
                    [ { row = current.row
                      , col = current.col
                      , problem =
                            UnexpectedField fields
                      }
                    ]

                Mark.ExpectingIndent indentation ->
                    [ { row = current.row
                      , col = current.col
                      , problem =
                            ExpectingIndent indentation
                      }
                    ]

                Mark.CantStartTextWithSpace ->
                    [ { row = current.row
                      , col = current.col
                      , problem =
                            CantStartTextWithSpace
                      }
                    ]

                Mark.UnclosedStyles styles ->
                    [ { row = current.row
                      , col = current.col
                      , problem =
                            UnclosedStyle styles
                      }
                    ]

                _ ->
                    merged

        last :: remaining ->
            if last.col == current.col && last.row == current.row then
                case current.problem of
                    Mark.ExpectingBlockName block ->
                        case last.problem of
                            UnknownBlock blocks ->
                                { row = current.row
                                , col = current.col
                                , problem =
                                    UnknownBlock (block :: blocks)
                                }
                                    :: remaining

                            _ ->
                                remaining

                    Mark.ExpectingInlineName block ->
                        case last.problem of
                            UnknownInline blocks ->
                                { row = current.row
                                , col = current.col
                                , problem =
                                    UnknownInline (block :: blocks)
                                }
                                    :: remaining

                            _ ->
                                remaining

                    Mark.ExpectingIndent indentation ->
                        [ { row = current.row
                          , col = current.col
                          , problem =
                                ExpectingIndent indentation
                          }
                        ]

                    _ ->
                        merged

            else
                case current.problem of
                    Mark.ExpectingBlockName block ->
                        { row = current.row
                        , col = current.col
                        , problem =
                            UnknownBlock [ block ]
                        }
                            :: merged

                    _ ->
                        merged


renderErrors : List String -> Message -> Error
renderErrors lines current =
    case current.problem of
        UnknownBlock expecting ->
            let
                line =
                    getLine current.row lines

                word =
                    getWord current line
            in
            { title = "UNKNOWN BLOCK"
            , region =
                focusWord current line
            , message =
                [ text "I ran into an unexpected block name.\n\n"
                , singleLine current.row (line ++ "\n")
                , highlightWord current line
                , text "But I was expecting one of these instead:\n\n"
                , expecting
                    |> List.sortBy (\exp -> 0 - similarity word exp)
                    |> List.map (indent 4)
                    |> String.join "\n"
                    |> text
                    |> yellow
                ]
            }

        UnknownInline expecting ->
            let
                line =
                    getLine current.row lines
            in
            { title = "UNKNOWN INLINE"
            , region =
                focusWord current line
            , message =
                [ text "I ran into an unexpected inline name.\n\n"
                , singleLine current.row (line ++ "\n")
                , highlightWord current line
                , text "But I was expecting one of these instead:\n"
                , expecting
                    |> List.sortBy (\exp -> 0 - similarity line exp)
                    |> List.map (indent 4)
                    |> String.join "\n"
                    |> text
                    |> yellow
                ]
            }

        ExpectingIndent indentation ->
            let
                line =
                    getLine current.row lines
            in
            { title = "MISMATCHED INDENTATION"
            , region = focusSpace current line
            , message =
                [ text ("I was expecting " ++ String.fromInt indentation ++ " spaces of indentation.\n\n")
                , singleLine current.row (line ++ "\n")
                , highlightSpace current.col line
                ]
                    ++ hint "All indentation in `elm-markup` is a multiple of 4."
            }

        CantStartTextWithSpace ->
            let
                line =
                    getLine current.row lines
            in
            { title = "TOO MUCH SPACE"
            , region = focusSpace current line
            , message =
                [ text "This line of text starts with extra space.\n\n"
                , singleLine current.row (line ++ "\n")
                , highlightSpace (current.col - 1) line
                , text "Beyond the required indentation, text should start with non-whitespace characters."
                ]
            }

        UnclosedStyle styles ->
            let
                line =
                    getLine current.row lines
            in
            { title = "UNCLOSED STYLE"
            , region = focusSpace current line
            , message =
                [ text (styleNames styles ++ " still open.  Add " ++ String.join " and " (List.map styleChars styles) ++ " to close it.\n\n")
                , singleLine current.row (line ++ "\n")
                , text (String.join "" (List.map styleChars styles) ++ "\n")
                    |> red
                , highlightSpace current.col line
                ]
                    ++ hint "`*` is used for bold and `/` is used for italic."
            }

        UnexpectedField field ->
            let
                line =
                    getLine current.row lines
            in
            { title = "UNKNOWN FIELD"
            , region =
                focusWord current line
            , message =
                [ text "I ran into an unexpected field name for a "
                , text field.recordName
                    |> yellow
                , text " record\n\n"
                , singleLine current.row (line ++ "\n")
                , highlightWord current line
                , text "Do you mean one of these instead?\n"
                , field.options
                    |> List.sortBy (\exp -> 0 - similarity line exp)
                    |> List.map (indent 4)
                    |> String.join "\n"
                    |> text
                    |> yellow
                ]
            }

        BadDate found ->
            let
                line =
                    getLine current.row lines
            in
            { title = "BAD DATE"
            , region =
                focusWord current line
            , message =
                [ text "I was trying to parse a date, but this format looks off.\n\n"
                , singleLine current.row (line ++ "\n")
                , highlightWord current line
                , text "Dates should be in ISO 8601 format:\n\n"
                , text (indent 4 "YYYY-MM-DDTHH:mm:ss.SSSZ")
                    |> yellow
                ]
            }

        IntOutOfRange found ->
            let
                line =
                    getLine current.row lines
            in
            { title = "INTEGER OUT OF RANGE"
            , region =
                focusWord current line
            , message =
                [ text "I was expecting an "
                , yellow (text "Int")
                , text (" between " ++ String.fromInt found.min ++ " and " ++ String.fromInt found.max ++ ", but found:\n\n")
                , singleLine current.row (line ++ "\n")
                , highlightWord current line
                ]
            }

        FloatOutOfRange found ->
            let
                line =
                    getLine current.row lines
            in
            { title = "FLOAT OUT OF RANGE"
            , region =
                focusWord current line
            , message =
                [ text "I was expecting a "
                , yellow (text "Float")
                , text (" between " ++ String.fromFloat found.min ++ " and " ++ String.fromFloat found.max ++ ", but found:\n\n")
                , singleLine current.row (line ++ "\n")
                , highlightWord current line
                ]
            }

        NonMatchingFields fields ->
            let
                line =
                    getLine current.row lines

                remaining =
                    List.filter
                        (\f -> List.member f fields.found)
                        fields.expecting
            in
            { title = "MISSING FIELD"
            , region = focusSpace current line
            , message =
                -- TODO: Highlight entire record section
                -- TODO: mention record name
                case remaining of
                    [] ->
                        -- TODO: This should never happen actually.
                        --  Maybe error should be a nonempty list?
                        [ text "It looks like a field is missing." ]

                    [ single ] ->
                        [ text "It looks like a field is missing.\n\n"
                        , text "You need to add the "
                        , yellow (text single)
                        , text " field."
                        ]

                    multiple ->
                        [ text "It looks like a field is missing.\n\n"
                        , text "You still need to add:\n"
                        , remaining
                            |> List.sortBy (\exp -> 0 - similarity line exp)
                            |> List.map (indent 4)
                            |> String.join "\n"
                            |> text
                            |> yellow
                        ]
            }


styleChars style =
    case style of
        Mark.Bold ->
            "*"

        Mark.Italic ->
            "/"

        Mark.Strike ->
            "~"


styleNames styles =
    let
        italic =
            List.any ((==) Mark.Italic) styles

        isBold =
            List.any ((==) Mark.Bold) styles

        strike =
            List.any ((==) Mark.Strike) styles
    in
    case ( italic, isBold, strike ) of
        ( False, False, False ) ->
            "Some formatting is"

        ( True, True, False ) ->
            "Italic and bold formatting are"

        ( True, True, True ) ->
            "Italic, strike, and bold formatting are"

        ( True, False, True ) ->
            "Italic and strike formatting are"

        ( False, True, True ) ->
            "Strike, and bold formatting are"

        ( True, False, False ) ->
            "Italic formatting is"

        ( False, True, False ) ->
            "Bold formatting is"

        ( False, False, True ) ->
            "Strike formatting is"


text : String -> Text
text str =
    { text = str
    , color = Nothing
    , bold = False
    , underline = False
    }


underline : Text -> Text
underline txt =
    { txt | underline = True }


bold : Text -> Text
bold txt =
    { txt | bold = True }


red : Text -> Text
red txt =
    { txt | color = Just "red" }


yellow : Text -> Text
yellow txt =
    { txt | color = Just "yellow" }


cyan : Text -> Text
cyan txt =
    { txt | color = Just "cyan" }


focusWord cursor line =
    let
        highlightLength =
            line
                |> String.dropLeft cursor.col
                |> String.words
                |> List.head
                |> Maybe.map String.length
                |> Maybe.withDefault 1
    in
    { start =
        { column = cursor.col
        , line = cursor.row
        }
    , end =
        { column = cursor.col + highlightLength
        , line = cursor.row
        }
    }


focusSpace cursor line =
    let
        start =
            String.dropLeft (cursor.col - 1) line

        trimmed =
            String.trimLeft start

        highlightLength =
            String.length start
                - String.length trimmed
                |> max 1
    in
    { start =
        { column = cursor.col
        , line = cursor.row
        }
    , end =
        { column = cursor.col + highlightLength
        , line = cursor.row
        }
    }


highlightSpace col line =
    let
        start =
            String.dropLeft (col - 1) line

        trimmed =
            String.trimLeft start

        highlightLength =
            String.length start
                - String.length trimmed
                |> max 1
    in
    red <| text (" " ++ String.repeat col " " ++ String.repeat highlightLength "^" ++ "\n")


highlightWord cursor line =
    let
        rowNumLength =
            String.length (String.fromInt cursor.row)

        highlightLength =
            line
                |> String.dropLeft (cursor.col - rowNumLength)
                |> String.words
                |> List.head
                |> Maybe.map String.length
                |> Maybe.withDefault 1
    in
    red <| text (String.repeat (rowNumLength + cursor.col - 1) " " ++ String.repeat highlightLength "^" ++ "\n")


newline =
    { text = "\n"
    , color = Nothing
    , underline = False
    , bold = False
    }


indent x str =
    String.repeat x " " ++ str


singleLine row line =
    text <|
        String.fromInt row
            ++ (if String.startsWith "|" line then
                    ""

                else
                    "|"
               )
            ++ line


hint str =
    [ text "Hint"
        |> underline
    , text (": " ++ str)
    ]


getLine row lines =
    case List.head (List.drop (row - 1) lines) of
        Nothing ->
            "Empty"

        Just l ->
            l


getWord cursor line =
    let
        rowNumLength =
            String.length (String.fromInt cursor.row)

        highlightLength =
            line
                |> String.dropLeft (cursor.col - rowNumLength)
                |> String.words
                |> List.head
                |> Maybe.map String.length
                |> Maybe.withDefault 1

        end =
            cursor.col + highlightLength
    in
    String.slice (cursor.col - 1) end line
