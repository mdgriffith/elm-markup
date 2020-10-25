module RealworldParsing exposing (suite)

{-| -}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html exposing (Html)
import Html.Attributes as Attr
import Mark
import Mark.Error
import Mark.Internal.Description as Description
import Mark.Internal.Id as Id
import Mark.Internal.Parser
import Parser.Advanced as Parser
import Test exposing (..)


suite =
    describe "realworld cases"
        [ test "basic" <|
            \_ ->
                let
                    parsed : Mark.Outcome (List Mark.Error.Error) (Mark.Partial Mark.Parsed) Mark.Parsed
                    parsed =
                        Mark.parse
                            document
                            elmOptimizeLevelTwoTODO
                in
                Expect.true
                    "Parsed successfully"
                    (isSuccessful parsed)
        , test "Mark.toString creates a valid, pretty printed version of the source" <|
            \_ ->
                let
                    parsed : Mark.Outcome (List Mark.Error.Error) (Mark.Partial Mark.Parsed) Mark.Parsed
                    parsed =
                        Mark.parse
                            document
                            elmOptimizeLevelTwoTODOTiny

                    stringified : String
                    stringified =
                        case parsed of
                            Mark.Success success ->
                                Mark.toString success

                            _ ->
                                ""
                in
                Expect.equal
                    """|> Page
    author = Matthew Griffith
    title = Elm Optimize TODO
    icon = ?
    description = Imported

|> H2
    Benchmarks

-- {x} elm-markdown

    With some additional content.  What should go here?

    And here?

    -- {x} Here, but runs into a transformation error
        What is happening?

-- {x} elm-animator
-- {x} elm-ui
    -- {x} Might run afoul of elm-benchmark using style-elements"""
                    stringified
        , test "parse just metadata" <|
            \_ ->
                let
                    parsed =
                        Mark.metadata
                            document
                            elmOptimizeLevelTwoTODO
                in
                Expect.true
                    "Parsed successfully"
                    (case parsed of
                        Ok p ->
                            True

                        Err _ ->
                            False
                    )
        ]


isSuccessful outcome =
    case outcome of
        Mark.Success success ->
            True

        _ ->
            False



{- SOURCES

   These are real notes that I've taken that have failed in some way.

-}


elmOptimizeLevelTwoTODOTiny =
    """|> Page
    author = Matthew Griffith
    title = Elm Optimize TODO
    icon = ?
    description = Imported


|> H2
    Benchmarks


-- {x} elm-markdown
    
    With some additional content.  What should go here?

    And here?

    -- {x} Here, but runs into a transformation error
        What is happening?
-- {x} elm-animator
-- {x} elm-ui

    -- {x} Might run afoul of elm-benchmark using style-elements


"""


elmOptimizeLevelTwoTODO =
    """|> Page
    author = Matthew Griffith
    title = Elm Optimize TODO
    icon = ?
    description = Imported



-- {x} Script to run benchmark via webdriver.

    -- {x} Reports results from elm-benchmark via json through a port

        -- {x} Can we standardize this as a Benchmark.JsonRunner?

|> H2
    Benchmarks



-- {x} elm-markdown

    -- {x} Here, but runs into a transformation error

-- {x} elm-ui

    -- {x} Might run afoul of elm-benchmark using style-elements

-- {x} elm-animator

-- {_} elm-json

-- {_} [https://github.com/ianmackenzie/elm-iso-10303]{link| url = https://github.com/ianmackenzie/elm-iso-10303}

-- {x} [https://github.com/w0rm/elm-obj-file/blob/master/benchmarks/src/Decode.elm]{link| url = https://github.com/w0rm/elm-obj-file/blob/master/benchmarks/src/Decode.elm}

-- {_} Rewrite Readme

    -- {x} Running the CLI

        -- {x} A drop in replacement for elm make --optimize

        -- {x} Also a --no-ie option

    -- {x} Overview of Results

        -- {x} Transformation Overview

        -- {x} Latest suite of benchmarks in table

        -- {x} Benchmarks of Breakdowns

    -- {x} elm-optimize as a testbed

        -- {x} Running the testcases

    -- {x} Contributing

        -- {x} Contributing actual benchmarks

            -- {x} via exposed elm-benchmark

        -- {_} Open Problems

            -- {_} Hoisting Constants

            -- {_} Analyzing What Functions are Optimized or Deoptimized for a Run

            -- {_} Shape analysis for elm functions

                -- {_} Specifically do the constructors for each variant actually return the same shape?

-- {_} Improve testcase run output

    -- {x} Report what is happening at each step, colorized

    -- {_} Instrument each transformation with metrics

        -- {_} What optimizations were applied and how many instances

        -- {_} What situations were detected that were unoptimizable.

        -- {_} Can we return this as data for a report?

-- {x} Report project for each benchmark

    -- {x} markdown report

-- {x} Report benchmark tags for each benchmark

    -- {x} json

    -- {x} markdown report

-- {x} Parse actual project for types

-- {_} Blissfully Test case

    -- {_} Add note to transformations about arrowizing functions being important for initial parse time.

    -- {_} Compare terser vs our own version
|> H3
    Table to Generate




Project →


-- Tags, Name

-- Base | transformed (transformations applied)

-- Browser

-- Ops/Second

-- Percent Change
"""



{- DOCUMENT DEFINITION -}


document =
    Mark.documentWith
        -- (\meta body ->
        --     { metadata = meta
        --     , body =
        --         Html.h1 []
        --             [ Html.text meta.icon
        --             , Html.span [ Attr.style "margin-left" "10px" ] meta.title
        --             ]
        --             :: body
        --     }
        -- )
        -- We have some required metadata that starts our document.
        { id = \_ -> "document-id"
        , metadata = metadata
        , blocks =
            [ header
            , h2
            , h3
            , image
            , list
            , code
            , Mark.map (always (Html.text "record")) (Mark.toBlock metadata)
            , Mark.withId
                (\id els ->
                    Html.p
                        [ Attr.id (Mark.idToString id)
                        , Attr.class "editor-box"
                        ]
                        els
                )
                text
            ]
        }



{- Handle Text -}


text =
    Mark.textWith
        { view =
            \styles string ->
                viewText styles string
        , replacements = Mark.commonReplacements
        , inlines =
            [ link
            , unlinked
            , externalLink
            , droppedCapital
            , Mark.record "x"
                (Html.span [] [ Html.text "[x]" ])
            , Mark.record "_"
                (Html.span [] [ Html.text "[ ]" ])
            ]
        }


viewText styles string =
    if styles.bold || styles.italic || styles.strike then
        Html.span
            [ Attr.classList
                [ ( "bold", styles.bold )
                , ( "italic", styles.italic )
                , ( "strike", styles.strike )
                ]
            ]
            [ Html.text string ]

    else
        Html.text string


externalLink =
    Mark.annotation "link"
        (\id texts url ->
            Html.a [ Attr.href url ]
                (List.map (applyTuple viewText) texts)
        )
        |> Mark.field "url" Mark.string


link =
    Mark.annotation "to"
        (\identifier texts maybeId ->
            case maybeId of
                Nothing ->
                    Html.span []
                        (List.map (applyTuple viewText) texts)

                Just id ->
                    Html.span
                        [ Attr.style "cursor" "pointer"
                        ]
                        (List.map (applyTuple viewText) texts)
        )
        |> Mark.field "id" blockId


unlinked =
    Mark.annotation "unlinked"
        (\id texts ->
            Html.span []
                (List.map (applyTuple viewText) texts)
        )


blockId =
    Mark.string
        |> Mark.verify
            (\str ->
                Ok (Just str)
            )
        |> Mark.onError Nothing


illFormattedId =
    { title = "Incorrect id format"
    , message =
        [ "I was trying to parse a block id, but this format looks off.\n\n"
        ]
    }


applyTuple fn ( one, two ) =
    fn one two


droppedCapital =
    Mark.verbatim "drop"
        (\id str ->
            let
                drop : String
                drop =
                    String.left 1 str

                lede : String
                lede =
                    String.dropLeft 1 str
            in
            Html.span []
                [ Html.span [ Attr.class "drop-capital" ]
                    [ Html.text drop ]
                , Html.span [ Attr.class "lede" ]
                    [ Html.text lede ]
                ]
        )



{- Metadata -}


metadata =
    Mark.record "Page"
        (\author description title icon ->
            { author = author
            , description = description
            , title = title
            , icon = icon
            }
        )
        |> Mark.field "author" Mark.string
        |> Mark.field "description" text
        |> Mark.field "title" text
        |> Mark.field "icon" Mark.string



{- Common Blocks -}


header =
    Mark.withId
        (\id els ->
            Html.h1
                [ Attr.id (Mark.idToString id)

                -- , Attr.class "editor-box"
                ]
                els
        )
        (Mark.block "H1"
            identity
            text
        )


h2 =
    Mark.withId
        (\id els ->
            Html.h2
                [ Attr.id (Mark.idToString id)

                -- , Attr.class "editor-box"
                ]
                els
        )
        (Mark.block "H2"
            identity
            text
        )


h3 =
    Mark.withId
        (\id els ->
            Html.h3
                [ Attr.id (Mark.idToString id)

                -- , Attr.class "editor-box"
                ]
                els
        )
        (Mark.block "H3"
            identity
            text
        )


image =
    Mark.record "Image"
        (\src description ->
            Html.img
                [ Attr.src src
                , Attr.alt description
                , Attr.style "float" "left"
                , Attr.style "margin-right" "48px"
                , Attr.style "width" "200px"
                , Attr.style "height" "300px"
                ]
                []
        )
        |> Mark.field "src" Mark.string
        |> Mark.field "description" Mark.string
        |> Mark.toBlock


code =
    Mark.block "Code"
        (\str ->
            Html.pre
                [ Attr.style "padding" "12px"
                , Attr.style "background-color" "#eee"
                ]
                [ Html.text str ]
        )
        Mark.string



{- Handling bulleted and numbered lists -}


list : Mark.Block (Html msg)
list =
    Mark.tree renderList (Mark.map (Html.div []) text)


{-| Note: we have to define this as a separate function because
`Enumerated` and `Item` are a pair of mutually recursive data structures.
It's easiest to render them using two separate functions: renderList and renderItem
-}
renderList : Mark.Enumerated (Html msg) -> Html msg
renderList (Mark.Enumerated enum) =
    let
        group : List (Html.Attribute msg) -> List (Html msg) -> Html msg
        group =
            case enum.icon of
                Mark.Bullet ->
                    Html.ul

                Mark.Number ->
                    Html.ol
    in
    group []
        (List.map renderItem enum.items)


renderItem : Mark.Item (Html msg) -> Html msg
renderItem (Mark.Item item) =
    Html.li []
        [ Html.div [] item.content
        , renderList item.children
        ]



{- END DOCUMENT DEFINITION -}
