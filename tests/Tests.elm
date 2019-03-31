module Tests exposing (suite)

-- import Mark.Error
-- import Mark.Default

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mark
import Mark.Internal.Description
import Mark.Internal.Error as Error
import Mark.Internal.Outcome
import Test exposing (..)


text =
    Mark.text identity


inlines =
    Mark.document
        identity
        (Mark.textWith
            { view = List.singleton
            , replacements =
                [ Mark.replacement "..." "…"
                , Mark.replacement "<>" "\u{00A0}"
                , Mark.replacement "---" "—"
                , Mark.replacement "--" "–"
                , Mark.replacement "//" "/"
                , Mark.replacement "'" "’"
                , Mark.balanced
                    { start = ( "\"", "“" )
                    , end = ( "\"", "”" )
                    }
                ]
            , inlines =
                [ Mark.annotation "highlight" identity
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


codeDoc =
    Mark.document
        identity
        (Mark.block "Monospace"
            identity
            Mark.multiline
        )


codeAndTextDoc =
    Mark.document
        identity
        (Mark.startWith
            (\mono extra ->
                mono ++ ":" ++ String.join "," extra
            )
            (Mark.block "Monospace"
                identity
                Mark.multiline
            )
            (Mark.manyOf
                [ text
                    |> Mark.map (always "text")
                ]
            )
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
        (Mark.tree
            "Nested"
            identity
            (Mark.map (always True) Mark.string)
        )


nestedOrdering : Mark.Document (List Ordered)
nestedOrdering =
    Mark.document
        (List.indexedMap (renderContent []))
        (Mark.tree "Nested"
            identity
            Mark.int
        )


type Indexed
    = Indexed Int (List Indexed)


type Ordered
    = Ordered (List Int) (List Ordered)


renderContent stack i (Mark.Tree node) =
    case node.children of
        [] ->
            Ordered node.content []

        _ ->
            Ordered node.content (List.indexedMap (renderContent (i :: stack)) node.children)


renderIndex stack i (Mark.Tree node) =
    case node.children of
        [] ->
            Indexed i []

        _ ->
            Indexed i (List.indexedMap (renderIndex (i :: stack)) node.children)


simpleNestedOrderedDoc =
    """| Nested
    - 1
        2
    - 3
        4
        5
    - 6
"""


complexNestedOrderedDoc =
    """| Nested
    - 1
        2
    - 3
        4
        - 5
            6
        - 7
            8
    - 9
"""


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


emptyStyles =
    { bold = False
    , italic = False
    , strike = False
    }


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


toResult doc src =
    case Mark.Internal.Description.compile doc src of
        Mark.Internal.Outcome.Success success ->
            Ok success

        Mark.Internal.Outcome.Failure errs ->
            Err (List.map .problem errs)

        Mark.Internal.Outcome.Almost { errors } ->
            Err (List.map .problem errors)


suite : Test
suite =
    describe "Mark"
        [ describe "Text"
            [ test "Starts with Space" <|
                \_ ->
                    Expect.equal
                        (toResult toplevelText " one too many spaces...")
                        (Err [ Error.CantStartTextWithSpace ])

            -- , test "Unclosed Italic" <|
            --     \_ ->
            --         Expect.equal
            --             (toResult toplevelText "/Start italics, but don't finish")
            --             (Err
            --              [ Error.Escape
            --              , Error.Expecting "/"
            --              , Error.Expecting "~"
            --              , Error.Expecting "*"
            --              , Error.InlineStart
            --              , Error.UnclosedStyles [ Mark.Italic ]
            --              ]
            --             )
            , test "Inline elements should maintain their source order." <|
                \_ ->
                    Expect.equal
                        (toResult inlines "{Highlight|my} highlighted {Highlight|sentence} {Highlight|order}")
                        (Ok
                            [ [ Mark.Text emptyStyles "my" ]
                            , [ Mark.Text emptyStyles " highlighted " ]
                            , [ Mark.Text emptyStyles "sentence" ]
                            , [ Mark.Text emptyStyles " " ]
                            , [ Mark.Text emptyStyles "order" ]
                            ]
                        )
            , test "Basic replacement" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "my test//text")
                        (Ok [ [ Mark.Text emptyStyles "my test/text" ] ])
            , test "replace dash" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "my test--text")
                        (Ok [ [ Mark.Text emptyStyles "my test–text" ] ])
            , test "Inline elements should maintain escaped italics" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "[my //]{highlight} highlighted [sentence]{highlight} [order]{highlight}")
                        (Ok
                            [ [ Mark.Text emptyStyles "my /" ]
                            , [ Mark.Text emptyStyles " highlighted " ]
                            , [ Mark.Text emptyStyles "sentence" ]
                            , [ Mark.Text emptyStyles " " ]
                            , [ Mark.Text emptyStyles "order" ]
                            ]
                        )
            , test "Inline elements should maintain multiple replacements" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "[my ////]{highlight} highlighted [sentence]{highlight} [order]{highlight}")
                        (Ok
                            [ [ Mark.Text emptyStyles "my //" ]
                            , [ Mark.Text emptyStyles " highlighted " ]
                            , [ Mark.Text emptyStyles "sentence" ]
                            , [ Mark.Text emptyStyles " " ]
                            , [ Mark.Text emptyStyles "order" ]
                            ]
                        )
            , test "Inline elements should maintain escaped characters" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "[my \\/]{highlight} highlighted [sentence]{highlight} [order]{highlight}")
                        (Ok
                            [ [ Mark.Text emptyStyles "my /" ]
                            , [ Mark.Text emptyStyles " highlighted " ]
                            , [ Mark.Text emptyStyles "sentence" ]
                            , [ Mark.Text emptyStyles " " ]
                            , [ Mark.Text emptyStyles "order" ]
                            ]
                        )
            , test "Incorrect inline element name" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "[my]{highlurt} highlighted sentence")
                        (Err
                            [ Error.UnknownInline [ "Highlight" ]
                            ]
                        )
            ]
        , describe "Multiline"
            [ test "Correctly parse code block" <|
                \_ ->
                    Expect.equal
                        (toResult codeDoc """| Monospace
    Here is my first line.
    Here is my second.
    Here is my third.
    Here is my fourth.
        And my indented line.
""")
                        (Ok "Here is my first line.\nHere is my second.\nHere is my third.\nHere is my fourth.\n    And my indented line.\n")
            , test "Parse code block and then normal text" <|
                \_ ->
                    Expect.equal
                        (toResult codeAndTextDoc """| Monospace
    Here is my first line.
    Here is my second.
    Here is my third.
    Here is my fourth.
        And my indented line.
Then some text.
""")
                        (Ok "Here is my first line.\nHere is my second.\nHere is my third.\nHere is my fourth.\n    And my indented line.\n:text")
            ]

        -- , describe "Nested"
        --     [
        --         test "Simple list parsing.  No Nesting." <|
        --         \_ ->
        --             Expect.equal
        --                 (Result.mapError (List.map .problem)
        --                     (Mark.parse nested simpleNestedDoc)
        --                 )
        --                 (Ok [ Indexed 0 [], Indexed 1 [], Indexed 2 [] ])
        --     , test "Simple list parsing, maintains order" <|
        --         \_ ->
        --             Expect.equal
        --                 (Result.mapError (List.map .problem)
        --                     (Mark.parse nestedOrdering simpleNestedOrderedDoc)
        --                 )
        --                 (Ok
        --                     [ Ordered [ 1, 2 ] []
        --                     , Ordered [ 3, 4, 5 ] []
        --                     , Ordered [ 6 ] []
        --                     ]
        --                 )
        --     , test "Complex list parsing, maintains order" <|
        --         \_ ->
        --             Expect.equal
        --                 (Result.mapError (List.map .problem)
        --                     (Mark.parse nestedOrdering complexNestedOrderedDoc)
        --                 )
        --                 (Ok
        --                     [ Ordered [ 1, 2 ] []
        --                     , Ordered [ 3, 4 ]
        --                         [ Ordered [ 5, 6 ] []
        --                         , Ordered [ 7, 8 ] []
        --                         ]
        --                     , Ordered [ 9 ] []
        --                     ]
        --                 )
        --     , test "Nested list parsing" <|
        --         \_ ->
        --             Expect.equal
        --                 (Result.mapError (List.map .problem)
        --                     (Mark.parse nested complexNestedDoc)
        --                 )
        --                 (Ok
        --                     [ Indexed 0
        --                         [ Indexed 0 []
        --                         , Indexed 1 []
        --                         , Indexed 2 []
        --                         , Indexed 3 [ Indexed 0 [] ]
        --                         ]
        --                     , Indexed 1 [ Indexed 0 [] ]
        --                     , Indexed 2 []
        --                     ]
        --                 )
        --     , test "Nested list dedenting correctly" <|
        --         \_ ->
        --             Expect.equal
        --                 (Result.mapError (List.map .problem)
        --                     (Mark.parse nested dedentingNestedDoc)
        --                 )
        --                 (Ok
        --                     [ Indexed 0
        --                         [ Indexed 0 []
        --                         , Indexed 1 []
        --                         , Indexed 2 []
        --                         , Indexed 3 [ Indexed 0 [] ]
        --                         , Indexed 4 [ Indexed 0 [] ]
        --                         , Indexed 5 []
        --                         ]
        --                     , Indexed 1 [ Indexed 0 [] ]
        --                     , Indexed 2 []
        --                     ]
        --                 )
        --     ]
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
                    Expect.equal (toResult textDoc doc1)
                        (Err [ Error.UnknownBlock [ "Test" ] ])
            , test "Extra line between name and value" <|
                \_ ->
                    let
                        doc1 =
                            """| Test
    Here's my extra line
"""

                        result =
                            Ok [ Mark.Text emptyStyles "Here's my extra line" ]
                    in
                    Expect.equal (toResult textDoc doc1)
                        result
            , test "Incorrect Indentation" <|
                \_ ->
                    let
                        doc1 =
                            """| Test
  Only two spaces(should be four)
                        """
                    in
                    Expect.equal (toResult textDoc doc1)
                        (Err [ Error.ExpectingIndent 4 ])
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
                                , [ [ Mark.Text emptyStyles "Then a bunch of"
                                    ]
                                  , [ Mark.Text emptyStyles "paragraphs." ]
                                  , [ Mark.Text emptyStyles "Each with their own "
                                    , Mark.Text { bold = False, italic = True, strike = False } "styling"
                                    , Mark.Text emptyStyles "."
                                    ]
                                  ]
                                )
                    in
                    Expect.equal
                        (toResult withMetaData doc1)
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
                    in
                    Expect.equal (toResult withMetaData doc1)
                        (Err [ Error.UnknownBlock [ "Meta" ] ])
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
                    Expect.equal (toResult sectionDoc doc1)
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
                            Error.NonMatchingFields
                                { expecting = [ "one", "two", "three" ]
                                , found = [ "two", "one" ]
                                }
                    in
                    Expect.equal (toResult recordDoc doc1)
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
                        (toResult recordDoc doc1)
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
                    Expect.equal (toResult recordDoc doc1)
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
                    Expect.equal (toResult recordManyTextDoc doc1)
                        (Ok { one = "hello", three = [ [ Mark.Text emptyStyles "Here is a bunch of text" ] ], two = "world" })
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
                    Expect.equal (toResult recordManyTextDoc doc1)
                        (Ok
                            { one = "hello"
                            , three =
                                [ [ Mark.Text emptyStyles "Here is a bunch of text" ] ]
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
                    Expect.equal (toResult recordManyTextDoc doc1)
                        (Ok
                            { one = "hello"
                            , three =
                                [ [ Mark.Text emptyStyles "Here is a bunch of text" ]
                                , [ Mark.Text emptyStyles "And some more on another line" ]
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
                    in
                    Expect.equal (toResult recordDoc doc1)
                        (Err [ Error.ExpectingIndent 4 ])
            , test "Incorrect Indentation v2" <|
                \_ ->
                    let
                        doc1 =
                            """| Test
    one = hello
  two = world
                        """
                    in
                    Expect.equal (toResult recordDoc doc1)
                        (Err [ Error.ExpectingIndent 4 ])
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
                    in
                    Expect.equal (toResult recordDoc doc1)
                        (Err
                            [ Error.UnexpectedField
                                { options = [ "one", "two", "three" ]
                                , found = "four"
                                , recordName = "Test"
                                }
                            ]
                        )
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
                            ( Mark.compile recordDoc doc1
                            , Mark.compile recordDoc doc2
                            )
                    in
                    Expect.all
                        [ \( one, two ) ->
                            Expect.equal one two
                        , \( one, two ) ->
                            Expect.true "Record parsing success"
                                (case one of
                                    Mark.Success _ ->
                                        True

                                    _ ->
                                        False
                                )
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
                    Expect.equal (Mark.compile floatDoc doc1)
                        (Mark.Success { one = 15.25, two = -1, three = 2 })
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
                    Expect.equal (Mark.compile intDoc doc1)
                        (Mark.Success { one = 15, two = -1, three = 2 })
            ]
        ]
