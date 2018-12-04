module Records exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mark.Custom
import Test exposing (..)


recordDoc =
    Mark.Custom.document
        identity
        (Mark.Custom.record3 "Test"
            (\one two three -> { one = one, two = two, three = three })
            (Mark.Custom.field "one" Mark.Custom.string)
            (Mark.Custom.field "two" Mark.Custom.string)
            (Mark.Custom.field "three" Mark.Custom.string)
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
        [ describe "Records"
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
