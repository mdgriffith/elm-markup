module Mark.Internal.Error exposing
    ( Error(..), render
    , Context(..), Problem(..)
    )

{-|

@docs Error, render

@docs Context, Problem

-}

import Mark.Format as Format
import Parser.Advanced as Parser


{-| -}
type Error
    = DocumentMismatch
    | ParsingIssue (List (Parser.DeadEnd Context Problem))
    | UnknownBlock (List String)
    | UnknownInline (List String)
    | FailMatchOneOf (List String)
    | MissingFields (List String)
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
    | UnclosedStyle
        { bold : Bool
        , italic : Bool
        , strike : Bool
        }
    | BadDate String
    | BadFloat String
    | BadInt String
    | BadBool String
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
    | Custom
        { title : String
        , message : List String
        }



{- PARSING PROBLEMS -}


{-| -}
type Context
    = InBlock String
    | InInline String
    | InRecord String
    | InRecordField String


{-| -}
type Problem
    = ExpectingIndentation Int
    | InlineStart
    | InlineEnd
    | BlockStart
    | Expecting String
    | ExpectingBlockName String
    | ExpectingInlineName String
    | ExpectingFieldName String
    | Escape
    | EscapedChar
    | Newline
    | Space
    | End
    | Integer
    | FloatingPoint
    | InvalidNumber


type alias ErrorMessage =
    { message : List Format.Text
    , region : { start : Position, end : Position }
    , title : String
    }


{-| -}
type alias Position =
    { offset : Int
    , line : Int
    , column : Int
    }


{-| -}
type alias Range =
    { start : Position
    , end : Position
    }


{-| -}
type alias UnexpectedDetails =
    { range : Range
    , problem : Error
    }


render : String -> UnexpectedDetails -> ErrorMessage
render source current =
    case current.problem of
        DocumentMismatch ->
            { title = "DOCUMENT MISMATCH"
            , region =
                current.range
            , message =
                [ Format.text "Your "
                , Format.yellow (Format.text "document")
                , Format.text " and your "
                , Format.yellow (Format.text "Parsed")
                , Format.text " structure don't match for some reason.\n\n"
                , Format.text "This usually occurs because you've stored the "
                , Format.yellow (Format.text "Parsed")
                , Format.text " data somewhere and then made a breaking change to your document."
                ]
            }

        ParsingIssue issues ->
            { title = "PARSING ISSUE"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I ran into an issue parsing your document.\n\n" ]
                    , renderParserIssue issues
                    , [ Format.text "\n\n" ]
                    ]
            }

        UnknownBlock expecting ->
            let
                target =
                    String.slice current.range.start.offset current.range.end.offset source
            in
            { title = "UNKNOWN BLOCK"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I don't recognize this block name.\n\n" ]
                    , highlight current.range source
                    , [ Format.text "Do you mean one of these instead?\n\n"
                      , expecting
                            |> List.sortBy (\exp -> 0 - similarity target exp)
                            |> List.map (addIndent 4)
                            |> String.join "\n"
                            |> Format.text
                            |> Format.yellow
                      ]
                    ]
            }

        UnknownInline expecting ->
            let
                target =
                    String.slice current.range.start.offset current.range.end.offset source
            in
            { title = "UNKNOWN INLINE"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I ran into an unexpected inline name.\n\n" ]
                    , highlight current.range source
                    , [ Format.text "But I was expecting one of these instead:\n\n"
                      , expecting
                            |> List.sortBy (\exp -> 0 - similarity target exp)
                            |> List.map (addIndent 4)
                            |> String.join "\n"
                            |> Format.text
                            |> Format.yellow
                      ]
                    ]
            }

        FailMatchOneOf expecting ->
            let
                target =
                    String.slice current.range.start.offset current.range.end.offset source
            in
            { title = "NO MATCH"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I wasn't able to match this.\n\n" ]
                    , highlight current.range source
                    , [ Format.text "to one of the following:\n\n"
                      , expecting
                            |> List.map (addIndent 4)
                            |> String.join "\n"
                            |> Format.text
                            |> Format.yellow
                      ]
                    ]
            }

        ExpectingIndent indentation ->
            { title = "MISMATCHED INDENTATION"
            , region = current.range
            , message =
                [ Format.text ("I was expecting " ++ String.fromInt indentation ++ " spaces of indentation.\n\n")
                ]
                    ++ highlight current.range source
                    ++ hint "All indentation in `elm-markup` is a multiple of 4."
            }

        CantStartTextWithSpace ->
            let
                line =
                    String.slice current.range.start.offset current.range.end.offset source
            in
            { title = "TOO MUCH SPACE"
            , region = current.range
            , message =
                List.concat
                    [ [ Format.text "This line of text starts with extra space.\n\n" ]
                    , highlight current.range source
                    , [ Format.text "Beyond the required indentation, text should start with non-whitespace characters." ]
                    ]
            }

        UnclosedStyle styles ->
            let
                line =
                    String.slice current.range.start.offset current.range.end.offset source
            in
            { title = "UNCLOSED STYLE"
            , region = current.range
            , message =
                List.concat
                    [ [ Format.text (styleNames styles ++ " still open.  Add " ++ styleChars styles ++ " to close it.\n\n") ] ]
                    ++ highlight current.range source
                    ++ hint "`*` is used for bold and `/` is used for italic."
            }

        UnexpectedField msgUnexpectedField ->
            let
                target =
                    String.slice current.range.start.offset current.range.end.offset source
            in
            { title = "UNKNOWN FIELD"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I ran into an unexpected field name for a "
                      , Format.text msgUnexpectedField.recordName
                            |> Format.yellow
                      , Format.text " record\n\n"
                      ]
                    , highlight current.range source
                    , [ Format.text "\nDo you mean one of these instead?\n\n"
                      , msgUnexpectedField.options
                            |> List.sortBy (\exp -> 0 - similarity target exp)
                            |> List.map (addIndent 4)
                            |> String.join "\n"
                            |> Format.text
                            |> Format.yellow
                      ]
                    ]
            }

        BadDate found ->
            { title = "BAD DATE"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I was trying to parse a date, but this format looks off.\n\n" ]
                    , highlight current.range source
                    , [ Format.text "Dates should be in ISO 8601 format:\n\n"
                      , Format.text (addIndent 4 "YYYY-MM-DDTHH:mm:ss.SSSZ")
                            |> Format.yellow
                      ]
                    ]
            }

        BadFloat found ->
            { title = "BAD FLOAT"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I was trying to parse a float, but this format looks off.\n\n" ]
                    , highlight current.range source
                    ]
            }

        BadInt found ->
            { title = "BAD INT"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I was trying to parse an integer, but this format looks off.\n\n" ]
                    , highlight current.range source
                    ]
            }

        BadBool found ->
            { title = "BAD INT"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I was trying to parse a boolean, but this format looks off.\n\n" ]
                    , highlight current.range source
                    ]
            }

        IntOutOfRange found ->
            { title = "INTEGER OUT OF RANGE"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I was expecting an "
                      , Format.yellow (Format.text "Int")
                      , Format.text " between "
                      , Format.text (String.fromInt found.min)
                            |> Format.yellow
                      , Format.text " and "
                      , Format.text (String.fromInt found.max)
                            |> Format.yellow
                      , Format.text ", but found:\n\n"
                      ]
                    , highlight current.range source
                    ]
            }

        FloatOutOfRange found ->
            { title = "FLOAT OUT OF RANGE"
            , region =
                current.range
            , message =
                List.concat
                    [ [ Format.text "I was expecting a "
                      , Format.yellow (Format.text "Float")
                      , Format.text " between "
                      , Format.text (String.fromFloat found.min)
                            |> Format.yellow
                      , Format.text " and "
                      , Format.text (String.fromFloat found.max)
                            |> Format.yellow
                      , Format.text ", but found:\n\n"
                      ]
                    , highlight current.range source
                    ]
            }

        NonMatchingFields fields ->
            let
                line =
                    String.slice current.range.start.offset current.range.end.offset source

                remaining =
                    List.filter
                        (\f -> not <| List.member f fields.found)
                        fields.expecting
            in
            { title = "MISSING FIELD"
            , region = current.range
            , message =
                -- TODO: Highlight entire record section
                -- TODO: mention record name
                case remaining of
                    [] ->
                        -- TODO: This should never happen actually.
                        --  Maybe error should be a nonempty list?
                        [ Format.text "It looks like a field is missing." ]

                    [ single ] ->
                        [ Format.text "It looks like a field is missing.\n\n"
                        , Format.text "You need to add the "
                        , Format.yellow (Format.text single)
                        , Format.text " field."
                        ]

                    multiple ->
                        [ Format.text "It looks like a field is missing.\n\n"
                        , Format.text "You still need to add:\n"
                        , remaining
                            |> List.sortBy (\exp -> 0 - similarity line exp)
                            |> List.map (addIndent 4)
                            |> String.join "\n"
                            |> Format.text
                            |> Format.yellow
                        ]
            }

        MissingFields remaining ->
            let
                line =
                    String.slice current.range.start.offset current.range.end.offset source
            in
            { title = "MISSING FIELD"
            , region = current.range
            , message =
                -- TODO: Highlight entire record section
                -- TODO: mention record name
                case remaining of
                    [] ->
                        -- TODO: This should never happen actually.
                        --  Maybe error should be a nonempty list?
                        [ Format.text "It looks like a field is missing." ]

                    [ single ] ->
                        [ Format.text "It looks like a field is missing.\n\n"
                        , Format.text "You need to add the "
                        , Format.yellow (Format.text single)
                        , Format.text " field."
                        ]

                    multiple ->
                        [ Format.text "It looks like a field is missing.\n\n"
                        , Format.text "You still need to add:\n"
                        , remaining
                            |> List.sortBy (\exp -> 0 - similarity line exp)
                            |> List.map (addIndent 4)
                            |> String.join "\n"
                            |> Format.text
                            |> Format.yellow
                        ]
            }

        Custom custom ->
            { title = String.toUpper custom.title
            , region = current.range
            , message =
                List.map Format.text custom.message
            }


styleChars styles =
    let
        italic =
            styles.italic

        isBold =
            styles.bold

        strike =
            styles.strike
    in
    case ( italic, isBold, strike ) of
        ( False, False, False ) ->
            "Some formatting is"

        ( True, True, False ) ->
            "/ and *"

        ( True, True, True ) ->
            "/, *, and ~"

        ( True, False, True ) ->
            "/ and ~"

        ( False, True, True ) ->
            "* and ~"

        ( True, False, False ) ->
            "/"

        ( False, True, False ) ->
            "*"

        ( False, False, True ) ->
            "~"


styleNames styles =
    let
        italic =
            styles.italic

        isBold =
            styles.bold

        strike =
            styles.strike
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


type alias Similarity =
    Int


similarity : String -> String -> Similarity
similarity source target =
    let
        lenSimilarity =
            0 - min 2 (abs (String.length source - String.length target))

        addCompared ( x, y ) total =
            if x == y then
                total + 1

            else
                total
    in
    List.map2 Tuple.pair (String.toList source) (String.toList target)
        |> List.foldl addCompared 0
        |> (+) lenSimilarity


renderParserIssue deadends =
    List.map
        (Format.yellow
            << Format.text
            << addIndent 4
            << parsingProblemToString
            << .problem
        )
        deadends


addIndent x str =
    String.repeat x " " ++ str


parsingProblemToString prob =
    case prob of
        ExpectingIndentation i ->
            "ExpectingIndent i" ++ "\n"

        InlineStart ->
            "InlineStart\n"

        InlineEnd ->
            "InlineEnd\n"

        BlockStart ->
            "BlockStart\n"

        Expecting str ->
            "Expecting " ++ str ++ "\n"

        ExpectingBlockName name ->
            "ExpectingBlockName " ++ name ++ "\n"

        ExpectingInlineName name ->
            "ExpectingInlineName " ++ name ++ "\n"

        ExpectingFieldName name ->
            "ExpectingFieldName " ++ name ++ "\n"

        Escape ->
            "Escape\n"

        EscapedChar ->
            "EscapedChar\n"

        Newline ->
            "Newline\n"

        Space ->
            "Space\n"

        End ->
            "End\n"

        Integer ->
            "Integer\n"

        FloatingPoint ->
            "FloatingPoint\n"

        InvalidNumber ->
            "InvalidNumber\n"


formatNewline =
    { text = "\n"
    , color = Nothing
    , underline = False
    , bold = False
    }


hint str =
    [ Format.text "Hint"
        |> Format.underline
    , Format.text (": " ++ str)
    ]


highlight range source =
    if range.start.line == range.end.line then
        -- single line
        let
            lineStart =
                range.start.offset - (range.start.column - 1)

            line =
                String.slice lineStart (range.end.offset + 20) source
                    |> String.lines
                    |> List.head
                    |> Maybe.withDefault ""

            lineNumber =
                String.fromInt range.start.line
                    ++ (if String.startsWith "|" line then
                            ""

                        else
                            "|"
                       )
        in
        [ Format.text (lineNumber ++ line ++ "\n")
        , Format.red
            (Format.text
                (String.repeat
                    (range.start.column - 1 + String.length lineNumber)
                    " "
                    ++ String.repeat (range.end.column - range.start.column) "^"
                    ++ "\n"
                )
            )
        ]

    else
        -- multiline
        let
            snippet =
                String.slice range.start.offset range.end.offset source

            indented =
                String.slice (range.start.offset + 1 - range.start.column) range.start.offset source

            lines =
                String.lines (indented ++ snippet)
                    |> List.indexedMap
                        (\i str ->
                            ( i + range.start.line - 1, str )
                        )
        in
        List.concatMap highlightLine lines


highlightLine ( index, line ) =
    [ Format.text (String.fromInt index)
    , Format.red (Format.text ">")
    , Format.text (line ++ "\n")
    ]
