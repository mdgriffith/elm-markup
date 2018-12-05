module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mark.Custom
import Test exposing (..)


withMetaData =
    Mark.Custom.document
        identity
        (Mark.Custom.startsWith Tuple.pair
            (Mark.Custom.record2 "Meta"
                (\one two -> { one = one, two = two })
                (Mark.Custom.field "one" Mark.Custom.string)
                (Mark.Custom.field "two" Mark.Custom.string)
            )
            (Mark.Custom.manyOf
                [ Mark.Custom.text
                ]
            )
        )


toplevelText =
    Mark.Custom.document
        identity
        Mark.Custom.text


textDoc =
    Mark.Custom.document
        identity
        (Mark.Custom.block "Test"
            identity
            Mark.Custom.text
        )


recordDoc =
    Mark.Custom.document
        identity
        (Mark.Custom.record3 "Test"
            (\one two three -> { one = one, two = two, three = three })
            (Mark.Custom.field "one" Mark.Custom.string)
            (Mark.Custom.field "two" Mark.Custom.string)
            (Mark.Custom.field "three" Mark.Custom.string)
        )


recordManyTextDoc =
    Mark.Custom.document
        identity
        (Mark.Custom.record3 "Test"
            (\one two three -> { one = one, two = two, three = three })
            (Mark.Custom.field "one" Mark.Custom.string)
            (Mark.Custom.field "two" Mark.Custom.string)
            (Mark.Custom.field "three" (Mark.Custom.manyOf [ Mark.Custom.text ]))
        )


floatDoc =
    Mark.Custom.document
        identity
        (Mark.Custom.record3 "Test"
            (\one two three -> { one = one, two = two, three = three })
            (Mark.Custom.field "one" Mark.Custom.float)
            (Mark.Custom.field "two" Mark.Custom.float)
            (Mark.Custom.field "three" Mark.Custom.float)
        )


intDoc =
    Mark.Custom.document
        identity
        (Mark.Custom.record3 "Test"
            (\one two three -> { one = one, two = two, three = three })
            (Mark.Custom.field "one" Mark.Custom.int)
            (Mark.Custom.field "two" Mark.Custom.int)
            (Mark.Custom.field "three" Mark.Custom.int)
        )


sectionDoc =
    Mark.Custom.document
        identity
        (Mark.Custom.manyOf
            [ Mark.Custom.text
                |> Mark.Custom.map (always "text")
            , Mark.Custom.record3 "Test"
                (\one two three -> "record:one,two,three")
                (Mark.Custom.field "one" Mark.Custom.string)
                (Mark.Custom.field "two" Mark.Custom.string)
                (Mark.Custom.field "three" Mark.Custom.string)
            , Mark.Custom.block "Section"
                (\x -> "embedded:" ++ String.join "," x)
                (Mark.Custom.manyOf
                    [ Mark.Custom.block "Embedded"
                        (always "block")
                        Mark.Custom.text
                    , Mark.Custom.text
                        |> Mark.Custom.map (always "text")
                    ]
                )
            ]
        )


sectionWithRecordDoc =
    Mark.Custom.document
        identity
        (Mark.Custom.manyOf
            [ Mark.Custom.text
                |> Mark.Custom.map (always "text")
            , Mark.Custom.record3 "Test"
                (\one two three -> "record:one,two,three")
                (Mark.Custom.field "one" Mark.Custom.string)
                (Mark.Custom.field "two" Mark.Custom.string)
                (Mark.Custom.field "three" Mark.Custom.string)
            , Mark.Custom.block "Section"
                (\x -> "embedded:" ++ String.join "," x)
                (Mark.Custom.manyOf
                    [ Mark.Custom.block "Embedded"
                        (always "block")
                        Mark.Custom.text
                    , Mark.Custom.text
                        |> Mark.Custom.map (always "text")
                    ]
                )
            ]
        )


suite : Test
suite =
    describe "Mark.Custom"
        [ describe "Text"
            [ test "Starts with Space" <|
                \_ ->
                    let
                        doc1 =
                            """ one too many spaces..."""

                        expectedProblem =
                            Mark.Custom.CantStartTextWithSpace
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.Custom.parse toplevelText doc1))
                        (Err [ expectedProblem ])
            ]
        , describe "Blocks"
            [ test "Misspelled Block" <|
                \_ ->
                    let
                        doc1 =
                            """| Turst
    one = hello
    two = world
                        """

                        expectedProblem =
                            Mark.Custom.ExpectingBlockName "Test"
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.Custom.parse textDoc doc1))
                        (Err [ expectedProblem ])
            , test "Extra line between name and value" <|
                \_ ->
                    let
                        doc1 =
                            """| Test

    Here's my extra line

"""

                        result =
                            Ok [ { link = Nothing, style = Mark.Custom.NoFormatting "Here's my extra line" } ]
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.Custom.parse textDoc doc1))
                        result
            , test "Incorrect Indentation" <|
                \_ ->
                    let
                        doc1 =
                            """| Test
  Only two spaces(should be four)
                        """

                        expectedProblem =
                            Mark.Custom.ExpectingIndent 4
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.Custom.parse textDoc doc1))
                        (Err [ expectedProblem ])
            , test "Start with Metadata" <|
                \_ ->
                    let
                        doc1 =
                            """| Meta
    one = Test data
    two = other data

Then a bunch of

paragraphs.

Each with their own /styling/.
"""

                        result =
                            Ok
                                ( { one = "Test data", two = "other data" }
                                , [ [ { link = Nothing
                                      , style = Mark.Custom.NoFormatting "Then a bunch of"
                                      }
                                    ]
                                  , [ { link = Nothing, style = Mark.Custom.NoFormatting "paragraphs." } ]
                                  , [ { link = Nothing, style = Mark.Custom.NoFormatting "Each with their own " }
                                    , { link = Nothing, style = Mark.Custom.Styles [ Mark.Custom.Italic ] "styling" }
                                    , { link = Nothing, style = Mark.Custom.Styles [] "." }
                                    ]
                                  ]
                                )
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.Custom.parse withMetaData doc1))
                        result
            , test "Extra Newline to Start" <|
                \_ ->
                    let
                        doc1 =
                            """
| Meta
    one = Test data
    two = other data

Then a bunch of

paragraphs.

Each with their own /styling/.
"""

                        result =
                            Err [ Mark.Custom.ExpectingBlockName "Meta" ]
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.Custom.parse withMetaData doc1))
                        result
            , test "Nested section blocks" <|
                \_ ->
                    let
                        doc1 =
                            """
| Test
    one = Test data
    two = other data
    three = other test data

Then a bunch of

paragraphs.

Each with their own /styling/.

| Section
    Then we have embedded stuff

    and we can add other blocks like

    | Embedded
        This is embedded

    and others

Finally, a sentence
"""

                        result =
                            Ok [ "record:one,two,three", "text", "text", "text", "embedded:text,text,block,text", "text" ]
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.Custom.parse sectionDoc doc1))
                        result
            ]
        , describe "Records"
            [ test "Missing fields should error" <|
                \_ ->
                    let
                        doc1 =
                            """| Test
    one = hello
    two = world
                        """

                        expectedProblem =
                            Mark.Custom.RecordField
                                (Mark.Custom.NonMatchingFields
                                    { expecting = [ "one", "two", "three" ]
                                    , found = [ "two", "one" ]
                                    }
                                )
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.Custom.parse recordDoc doc1))
                        (Err [ expectedProblem ])
            , test "Extra lines between fields" <|
                \_ ->
                    let
                        doc1 =
                            """| Test
    one = hello

    two = world
    
    three = !
"""
                    in
                    Expect.equal
                        (Result.mapError (List.map .problem) (Mark.Custom.parse recordDoc doc1))
                        (Ok { one = "hello", three = "!", two = "world" })
            , test "Extra line between two fields" <|
                \_ ->
                    let
                        doc1 =
                            """| Test

    one = hello
    two = world

    three = !
"""
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.Custom.parse recordDoc doc1))
                        (Ok { one = "hello", three = "!", two = "world" })
            , test "Records with many text as a field" <|
                \_ ->
                    let
                        doc1 =
                            """| Test
    one = hello
    two = world
    three =
        Here is a bunch of text

"""
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.Custom.parse recordManyTextDoc doc1))
                        (Ok { one = "hello", three = [ [ { link = Nothing, style = Mark.Custom.NoFormatting "Here is a bunch of text" } ] ], two = "world" })
            , test "Records with many text as a field (starting on same line)" <|
                \_ ->
                    let
                        doc1 =
                            """| Test
    one = hello
    two = world
    three = Here is a bunch of text
"""
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.Custom.parse recordManyTextDoc doc1))
                        (Ok
                            { one = "hello"
                            , three =
                                [ [ { link = Nothing, style = Mark.Custom.NoFormatting "Here is a bunch of text" } ] ]
                            , two = "world"
                            }
                        )
            , test "Records with multiple lines in field" <|
                \_ ->
                    let
                        doc1 =
                            """| Test
    one = hello
    two = world
    three =
        Here is a bunch of text

        And some more on another line

"""
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.Custom.parse recordManyTextDoc doc1))
                        (Ok
                            { one = "hello"
                            , three =
                                [ [ { link = Nothing, style = Mark.Custom.NoFormatting "Here is a bunch of text" } ]
                                , [ { link = Nothing, style = Mark.Custom.NoFormatting "And some more on another line" } ]
                                ]
                            , two = "world"
                            }
                        )
            , test "Incorrect Indentation" <|
                \_ ->
                    let
                        doc1 =
                            """| Test
  one = hello
  two = world
                        """

                        expectedProblem =
                            Mark.Custom.ExpectingIndent 4
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.Custom.parse recordDoc doc1))
                        (Err [ expectedProblem ])
            , test "Incorrect Indentation v2" <|
                \_ ->
                    let
                        doc1 =
                            """| Test
    one = hello
  two = world
                        """

                        expectedProblem =
                            Mark.Custom.ExpectingIndent 4
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.Custom.parse recordDoc doc1))
                        (Err [ expectedProblem ])
            , test "Additional fields should error" <|
                \_ ->
                    let
                        doc1 =
                            """| Test
    one = hello
    two = world
    three = Yo
    four = huh?
                        """

                        expectedProblem =
                            Mark.Custom.UnexpectedField
                                { options = [ "one", "two", "three" ]
                                , found = "four"
                                , recordName = "Test"
                                }
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.Custom.parse recordDoc doc1))
                        (Err [ expectedProblem ])
            , test "Order of fields in source shouldn't matter" <|
                \_ ->
                    let
                        doc1 =
                            """| Test
    one = hello
    two = world
    three = !
                        """

                        doc2 =
                            """| Test
    two = world
    one = hello
    three = !
                        """

                        parsed =
                            ( Mark.Custom.parse recordDoc doc1
                            , Mark.Custom.parse recordDoc doc2
                            )
                    in
                    Expect.all
                        [ \( one, two ) ->
                            Expect.equal one two
                        , \( one, two ) ->
                            Expect.true "Record parsing success"
                                (Result.toMaybe one /= Nothing)
                        ]
                        parsed
            , test "Floats are parsed as expected" <|
                \_ ->
                    let
                        doc1 =
                            """| Test
    one = 15.25
    two = -1.0
    three = 2
                        """
                    in
                    Expect.equal (Mark.Custom.parse floatDoc doc1)
                        (Ok { one = 15.25, two = -1, three = 2 })
            , test "Ints are parsed as expected" <|
                \_ ->
                    let
                        doc1 =
                            """| Test
    one = 15
    two = -1
    three = 2
                        """
                    in
                    Expect.equal (Mark.Custom.parse intDoc doc1)
                        (Ok { one = 15, two = -1, three = 2 })
            ]
        ]
