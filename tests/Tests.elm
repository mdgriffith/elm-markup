module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mark.Custom
import Test exposing (..)


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
