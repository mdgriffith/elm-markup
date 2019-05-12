module Mark.Internal.Error exposing
    ( Error(..), render
    , Context(..), Problem(..), UnexpectedDetails, documentMismatch, renderParsingErrors, idNotFound
    , AstError(..), Rendered(..), compilerError
    )

{-|

@docs Error, render

@docs Context, Problem, UnexpectedDetails, documentMismatch, renderParsingErrors, idNotFound

-}

import Mark.Internal.Format as Format
import Parser.Advanced as Parser


type AstError
    = NoMatch


{-| -}
type Error
    = DocumentMismatch
    | CompilerError
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
    | BadFloat
    | BadInt
    | BadBool
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
      --
    | IdNotFound



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


{-| -}
type Rendered
    = Rendered
        { title : String
        , problem : Error
        , region :
            { start : Position
            , end : Position
            }
        , message : List Format.Text
        }
    | Global
        { problem : Error
        , title : String
        , message : List Format.Text
        }


documentMismatch =
    Global
        { title = "DOCUMENT MISMATCH"
        , problem = DocumentMismatch
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


compilerError =
    Global
        { title = "COMPILER ERROR"
        , problem = CompilerError
        , message =
            [ Format.text "Oh boy, this looks like a  "
            , Format.yellow (Format.text "compiler error")
            , Format.text "\n\n"
            , Format.text "If you have time, could you file an "
            , Format.yellow (Format.text "issue")
            , Format.text " on the elm-markup respository(https://github.com/mdgriffith/elm-markup) describing how you got here?"
            ]
        }


{-| -}
renderParsingErrors : String -> List (Parser.DeadEnd Context Problem) -> Rendered
renderParsingErrors source issues =
    Rendered
        { title = "PARSING ISSUE"
        , problem = ParsingIssue issues
        , region =
            -- TODO:  How to get offset + guarantee of non-null region
            -- Really, we should fold over all the issues and combine them if their ranges match
            -- and return a list.
            case issues of
                [] ->
                    { start =
                        { offset = 0
                        , line = 0
                        , column = 0
                        }
                    , end =
                        { offset = 0
                        , line = 0
                        , column = 0
                        }
                    }

                first :: _ ->
                    { start =
                        { offset = 0
                        , line = first.row
                        , column = first.col
                        }
                    , end =
                        { offset = 0
                        , line = first.row
                        , column = first.col
                        }
                    }
        , message =
            List.concat
                [ [ Format.text "\n" ]
                , renderParserIssue issues
                ]
        }


idNotFound =
    Global
        { title = "ID NOT FOUND"
        , problem = IdNotFound
        , message =
            [ Format.text "I tried to edit your document but couldn't find any blocks with the "
            , Format.yellow (Format.text "Mark.Edit.Id")
            , Format.text " that you provided."
            ]
        }


render : String -> UnexpectedDetails -> Rendered
render source current =
    case current.problem of
        CompilerError ->
            compilerError

        DocumentMismatch ->
            documentMismatch

        IdNotFound ->
            idNotFound

        ParsingIssue issues ->
            renderParsingErrors source issues

        UnknownBlock expecting ->
            let
                target =
                    String.slice current.range.start.offset current.range.end.offset source
            in
            Rendered
                { title = "UNKNOWN BLOCK"
                , problem = current.problem
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
            Rendered
                { title = "UNKNOWN INLINE"
                , problem = current.problem
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
            Rendered
                { title = "NO MATCH"
                , problem = current.problem
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
            Rendered
                { title = "MISMATCHED INDENTATION"
                , problem = current.problem
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
            Rendered
                { title = "TOO MUCH SPACE"
                , problem = current.problem
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
            Rendered
                { title = "UNCLOSED STYLE"
                , problem = current.problem
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
            Rendered
                { title = "UNKNOWN FIELD"
                , problem = current.problem
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
            Rendered
                { title = "BAD DATE"
                , problem = current.problem
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

        BadFloat ->
            Rendered
                { title = "BAD FLOAT"
                , problem = current.problem
                , region =
                    current.range
                , message =
                    List.concat
                        [ [ Format.text "I was trying to parse a float, but this format looks off.\n\n" ]
                        , highlight current.range source
                        ]
                }

        BadInt ->
            Rendered
                { title = "BAD INT"
                , problem = current.problem
                , region =
                    current.range
                , message =
                    List.concat
                        [ [ Format.text "I was trying to parse an integer, but this format looks off.\n\n" ]
                        , highlight current.range source
                        ]
                }

        BadBool ->
            Rendered
                { title = "BAD INT"
                , problem = current.problem
                , region =
                    current.range
                , message =
                    List.concat
                        [ [ Format.text "I was trying to parse a boolean, but this format looks off.\n\n" ]
                        , highlight current.range source
                        ]
                }

        IntOutOfRange found ->
            Rendered
                { title = "INTEGER OUT OF RANGE"
                , problem = current.problem
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
            Rendered
                { title = "FLOAT OUT OF RANGE"
                , problem = current.problem
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
            Rendered
                { title = "MISSING FIELD"
                , problem = current.problem
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
            Rendered
                { title = "MISSING FIELD"
                , problem = current.problem
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
            Rendered
                { title = String.toUpper custom.title
                , problem = current.problem
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


{-|

    Parser.DeadEnd

    { row = Int
    , col = Int
    , problem = problem
    , contextStack =
        List
            { row = Int
            , col = Int
            , context = context
            }
    }

-}
renderParserIssue deadends =
    List.concatMap
        (\dead ->
            renderParsingProblem dead.problem
                ++ [ Format.text "\n" ]
        )
        deadends


addIndent x str =
    String.repeat x " " ++ str


renderParsingProblem prob =
    case prob of
        ExpectingIndentation i ->
            [ Format.text ("I was expecting an indent of " ++ String.fromInt i ++ " spaces") ]

        InlineStart ->
            [ Format.text "InlineStart" ]

        InlineEnd ->
            [ Format.text "I was expecting the end of an inline: "
            , Format.yellow (Format.text "}")
            ]

        BlockStart ->
            [ Format.text "I was expecting the start of a block: "
            , Format.yellow (Format.text "|>")
            ]

        Expecting str ->
            [ Format.text ("I was expecting \"" ++ str ++ "\"") ]

        ExpectingBlockName name ->
            [ Format.text "I was expecting a block named "
            , Format.yellow (Format.text name)
            ]

        ExpectingInlineName name ->
            [ Format.text "I was expecting an inline named "
            , Format.yellow (Format.text name)
            ]

        ExpectingFieldName name ->
            [ Format.text "I was expecting a field named "
            , Format.yellow (Format.text name)
            ]

        Escape ->
            [ Format.text "I was expectng a backslash" ]

        EscapedChar ->
            [ Format.text "I was expecting an escaped character" ]

        Newline ->
            [ Format.text "I was expecting a newline" ]

        Space ->
            [ Format.text "I was expecting a space" ]

        End ->
            [ Format.text "I was expecting the end of a document." ]

        Integer ->
            [ Format.text "I was expecting an "
            , Format.yellow (Format.text "Int")
            ]

        FloatingPoint ->
            [ Format.text "I was expecting a "
            , Format.yellow (Format.text "Float")
            ]

        InvalidNumber ->
            [ Format.text "I ran into an invalid number." ]


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
                            "|>"
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
