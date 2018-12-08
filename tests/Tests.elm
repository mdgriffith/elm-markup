module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mark
import Test exposing (..)


text =
    Mark.text
        { view = identity
        , replacements = []
        , inlines = []
        }


inlines =
    Mark.document
        identity
        (Mark.text
            { view = List.singleton
            , replacements = []
            , inlines =
                [ Mark.inline "Highlight"
                    identity
                    |> Mark.inlineText
                ]
            }
        )


withMetaData =
    Mark.document
        identity
        (Mark.startWith Tuple.pair
            (Mark.record2 "Meta"
                (\one two -> { one = one, two = two })
                (Mark.field "one" Mark.string)
                (Mark.field "two" Mark.string)
            )
            (Mark.manyOf
                [ text
                ]
            )
        )


toplevelText =
    Mark.document
        identity
        text


textDoc =
    Mark.document
        identity
        (Mark.block "Test"
            identity
            text
        )


recordDoc =
    Mark.document
        identity
        (Mark.record3 "Test"
            (\one two three -> { one = one, two = two, three = three })
            (Mark.field "one" Mark.string)
            (Mark.field "two" Mark.string)
            (Mark.field "three" Mark.string)
        )


recordManyTextDoc =
    Mark.document
        identity
        (Mark.record3 "Test"
            (\one two three -> { one = one, two = two, three = three })
            (Mark.field "one" Mark.string)
            (Mark.field "two" Mark.string)
            (Mark.field "three" (Mark.manyOf [ text ]))
        )


floatDoc =
    Mark.document
        identity
        (Mark.record3 "Test"
            (\one two three -> { one = one, two = two, three = three })
            (Mark.field "one" Mark.float)
            (Mark.field "two" Mark.float)
            (Mark.field "three" Mark.float)
        )


intDoc =
    Mark.document
        identity
        (Mark.record3 "Test"
            (\one two three -> { one = one, two = two, three = three })
            (Mark.field "one" Mark.int)
            (Mark.field "two" Mark.int)
            (Mark.field "three" Mark.int)
        )


sectionDoc =
    Mark.document
        identity
        (Mark.manyOf
            [ text
                |> Mark.map (always "text")
            , Mark.record3 "Test"
                (\one two three -> "record:one,two,three")
                (Mark.field "one" Mark.string)
                (Mark.field "two" Mark.string)
                (Mark.field "three" Mark.string)
            , Mark.block "Section"
                (\x -> "section:" ++ String.join "," x)
                (Mark.manyOf
                    [ Mark.block "Embedded"
                        (always "embedded")
                        text
                    , text
                        |> Mark.map (always "text")
                    ]
                )
            ]
        )


nested : Mark.Document (List Indexed)
nested =
    Mark.document
        (List.indexedMap (renderIndex []))
        (Mark.block "Nested"
            identity
            (Mark.nested
                { start = Mark.exactly "- " True
                , item = Mark.exactly "*" True
                }
            )
        )


type Indexed
    = Indexed Int (List Indexed)


renderIndex stack i (Mark.Nested node) =
    case node.children of
        [] ->
            Indexed i []

        _ ->
            Indexed i (List.indexedMap (renderIndex (i :: stack)) node.children)


simpleNestedDoc =
    """| Nested
    - *
    - *
    - *
"""


complexNestedDoc =
    """| Nested
    - *
        - *
        - *
        - *
        - *
            - *
    - *
        - *
    - *
"""


dedentingNestedDoc =
    """| Nested
    - *
        - *
        - *
        - *
        - *
            - *
        - *
            - *
        - *
    - *
        - *
    - *
"""


sectionWithRecordDoc =
    Mark.document
        identity
        (Mark.manyOf
            [ text
                |> Mark.map (always "text")
            , Mark.record3 "Test"
                (\one two three -> "record:one,two,three")
                (Mark.field "one" Mark.string)
                (Mark.field "two" Mark.string)
                (Mark.field "three" Mark.string)
            , Mark.block "Section"
                (\x -> "embedded:" ++ String.join "," x)
                (Mark.manyOf
                    [ Mark.block "Embedded"
                        (always "block")
                        text
                    , text
                        |> Mark.map (always "text")
                    ]
                )
            ]
        )


suite : Test
suite =
    describe "Mark"
        [ describe "Text"
            [ test "Starts with Space" <|
                \_ ->
                    Expect.equal
                        (Result.mapError (List.map .problem)
                            (Mark.parse toplevelText " one too many spaces...")
                        )
                        (Err [ Mark.CantStartTextWithSpace ])
            , test "Unclosed Italic" <|
                \_ ->
                    Expect.equal
                        (Result.mapError (List.map .problem)
                            (Mark.parse toplevelText "/Start italics, but don't finish")
                        )
                        (Err
                            [ Mark.Escape
                            , Mark.Expecting "/"
                            , Mark.Expecting "~"
                            , Mark.Expecting "*"
                            , Mark.InlineStart
                            , Mark.UnclosedStyles [ Mark.Italic ]
                            ]
                        )
            , test "Inline elements should maintain their source order." <|
                \_ ->
                    Expect.equal
                        (Result.mapError (List.map .problem)
                            (Mark.parse inlines "{Highlight|my} highlighted {Highlight|sentence} {Highlight|order}")
                        )
                        (Ok
                            [ [ Mark.Text [] "my" ]
                            , [ Mark.Text [] " highlighted " ]
                            , [ Mark.Text [] "sentence" ]
                            , [ Mark.Text [] " " ]
                            , [ Mark.Text [] "order" ]
                            ]
                        )
            , test "Incorrect inline element name" <|
                \_ ->
                    Expect.equal
                        (Result.mapError (List.map .problem)
                            (Mark.parse inlines "{Highlurt|my} highlighted sentence")
                        )
                        (Err [ Mark.ExpectingInlineName "Highlight" ])
            ]
        , describe "Nested"
            [ test "Simple list parsing.  No Nesting." <|
                \_ ->
                    Expect.equal
                        (Result.mapError (List.map .problem)
                            (Mark.parse nested simpleNestedDoc)
                        )
                        (Ok [ Indexed 0 [], Indexed 1 [], Indexed 2 [] ])
            , test "Nested list parsing" <|
                \_ ->
                    Expect.equal
                        (Result.mapError (List.map .problem)
                            (Mark.parse nested complexNestedDoc)
                        )
                        (Ok
                            [ Indexed 0
                                [ Indexed 0 []
                                , Indexed 1 []
                                , Indexed 2 []
                                , Indexed 3 [ Indexed 0 [] ]
                                ]
                            , Indexed 1 [ Indexed 0 [] ]
                            , Indexed 2 []
                            ]
                        )
            , test "Nested list dedenting correctly" <|
                \_ ->
                    Expect.equal
                        (Result.mapError (List.map .problem)
                            (Mark.parse nested dedentingNestedDoc)
                        )
                        (Ok
                            [ Indexed 0
                                [ Indexed 0 []
                                , Indexed 1 []
                                , Indexed 2 []
                                , Indexed 3 [ Indexed 0 [] ]
                                , Indexed 4 [ Indexed 0 [] ]
                                , Indexed 5 []
                                ]
                            , Indexed 1 [ Indexed 0 [] ]
                            , Indexed 2 []
                            ]
                        )
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
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.parse textDoc doc1))
                        (Err [ Mark.ExpectingBlockName "Test" ])
            , test "Extra line between name and value" <|
                \_ ->
                    let
                        doc1 =
                            """| Test

    Here's my extra line

"""

                        result =
                            Ok [ Mark.Text [] "Here's my extra line" ]
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.parse textDoc doc1))
                        result
            , test "Incorrect Indentation" <|
                \_ ->
                    let
                        doc1 =
                            """| Test
  Only two spaces(should be four)
                        """

                        expectedProblem =
                            Mark.ExpectingIndent 4
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.parse textDoc doc1))
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
                                , [ [ Mark.Text [] "Then a bunch of"
                                    ]
                                  , [ Mark.Text [] "paragraphs." ]
                                  , [ Mark.Text [] "Each with their own "
                                    , Mark.Text [ Mark.Italic ] "styling"
                                    , Mark.Text [] "."
                                    ]
                                  ]
                                )
                    in
                    Expect.equal
                        (Result.mapError (List.map .problem) (Mark.parse withMetaData doc1))
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
                            Err [ Mark.ExpectingBlockName "Meta" ]
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.parse withMetaData doc1))
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
                            Ok [ "record:one,two,three", "text", "text", "text", "section:text,text,embedded,text", "text" ]
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.parse sectionDoc doc1))
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
                            Mark.NonMatchingFields
                                { expecting = [ "one", "two", "three" ]
                                , found = [ "two", "one" ]
                                }
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.parse recordDoc doc1))
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
                        (Result.mapError (List.map .problem) (Mark.parse recordDoc doc1))
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
                    Expect.equal (Result.mapError (List.map .problem) (Mark.parse recordDoc doc1))
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
                    Expect.equal (Result.mapError (List.map .problem) (Mark.parse recordManyTextDoc doc1))
                        (Ok { one = "hello", three = [ [ Mark.Text [] "Here is a bunch of text" ] ], two = "world" })
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
                    Expect.equal (Result.mapError (List.map .problem) (Mark.parse recordManyTextDoc doc1))
                        (Ok
                            { one = "hello"
                            , three =
                                [ [ Mark.Text [] "Here is a bunch of text" ] ]
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
                    Expect.equal (Result.mapError (List.map .problem) (Mark.parse recordManyTextDoc doc1))
                        (Ok
                            { one = "hello"
                            , three =
                                [ [ Mark.Text [] "Here is a bunch of text" ]
                                , [ Mark.Text [] "And some more on another line" ]
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
                            Mark.ExpectingIndent 4
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.parse recordDoc doc1))
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
                            Mark.ExpectingIndent 4
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.parse recordDoc doc1))
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
                            Mark.UnexpectedField
                                { options = [ "one", "two", "three" ]
                                , found = "four"
                                , recordName = "Test"
                                }
                    in
                    Expect.equal (Result.mapError (List.map .problem) (Mark.parse recordDoc doc1))
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
                            ( Mark.parse recordDoc doc1
                            , Mark.parse recordDoc doc2
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
                    Expect.equal (Mark.parse floatDoc doc1)
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
                    Expect.equal (Mark.parse intDoc doc1)
                        (Ok { one = 15, two = -1, three = 2 })
            ]
        ]
