module Tests exposing (suite, textModification)

-- import Mark.Error
-- import Mark.Default

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mark
import Mark.Edit
import Mark.Internal.Description
import Mark.Internal.Error as Error
import Mark.Internal.Outcome
import Mark.New
import Test exposing (..)


text =
    Mark.text Tuple.pair


inlines =
    Mark.document
        identity
        (Mark.textWith
            { view =
                \style content ->
                    [ ( style, content ) ]
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
                , Mark.verbatim "verb" (List.singleton << Tuple.pair emptyStyles)
                ]
            }
        )


inlineMultipleVerbatim =
    Mark.document
        identity
        (Mark.textWith
            { view =
                \styling str ->
                    [ str ]
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
                [ Mark.annotation "highlight" (always [ "high" ])
                , Mark.verbatim "verb"
                    (\str ->
                        [ str ]
                    )
                , Mark.verbatim "verb2"
                    (\str one two ->
                        [ str, one, two ]
                    )
                    |> Mark.attrString "one"
                    |> Mark.attrString "two"
                ]
            }
        )


inlineOrder =
    Mark.document
        identity
        (Mark.textWith
            { view = \_ _ -> Nothing
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
                [ Mark.annotation "order"
                    (\txt one two ->
                        Just ( one, two )
                    )
                    |> Mark.attrString "one"
                    |> Mark.attrString "two"
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


topLevelText =
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


singleOneOf =
    Mark.document
        identity
        (Mark.oneOf
            [ Mark.block "Monospace"
                identity
                Mark.multiline
            ]
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
            [ Mark.map (always "text") text
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


renderContent stack i (Mark.Edit.Tree node) =
    case node.children of
        [] ->
            Ordered node.content []

        _ ->
            Ordered node.content (List.indexedMap (renderContent (i :: stack)) node.children)


renderIndex stack i (Mark.Edit.Tree node) =
    case node.children of
        [] ->
            Indexed i []

        _ ->
            Indexed i (List.indexedMap (renderIndex (i :: stack)) node.children)


simpleNestedOrderedDoc =
    """|> Nested
    - 1
        2
    - 3
        4
        5
    - 6
"""


complexNestedOrderedDoc =
    """|> Nested
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
    """|> Nested
    - *
    - *
    - *
"""


complexNestedDoc =
    """|> Nested
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
    """|> Nested
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


getProblem renderedError =
    case renderedError of
        Error.Rendered details ->
            details.problem

        Error.Global details ->
            details.problem


flattenErrors result =
    case result of
        Ok ( parsed, outcome ) ->
            outcome

        Err outcome ->
            outcome


toResult doc src =
    case flattenErrors (Mark.Internal.Description.compile doc src) of
        Mark.Internal.Outcome.Success success ->
            Ok success

        Mark.Internal.Outcome.Failure errs ->
            Err (List.map getProblem errs)

        Mark.Internal.Outcome.Almost { errors } ->
            Err (List.map getProblem errors)


suite : Test
suite =
    describe "Mark"
        [ describe "Text"
            [ -- test "Starts with Space" <|
              -- \_ ->
              --     Expect.equal
              --         (toResult toplevelText " one too many spaces...")
              --         (Err [ Error.CantStartTextWithSpace ])
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
              test "Single newlines are allowed in paragraphs" <|
                \_ ->
                    Expect.equal
                        (toResult
                            (Mark.document identity text)
                            "Some text\nand some more text\n\n"
                        )
                        (Ok
                            [ ( emptyStyles, "Some text\nand some more text" )
                            ]
                        )
            , test "Double newlines signify a new paragraph" <|
                \_ ->
                    -- This is an error because we're expecting one paragraph, not two.
                    Expect.err
                        (toResult
                            (Mark.document identity text)
                            "Some text\n\nand some moretext"
                        )
            , test "Inline elements should maintain their source order." <|
                \_ ->
                    Expect.equal
                        (toResult inlines "[my]{highlight} highlighted [sentence]{highlight} [order]{highlight}")
                        (Ok
                            [ [ ( emptyStyles, "my" ) ]
                            , [ ( emptyStyles, " highlighted " ) ]
                            , [ ( emptyStyles, "sentence" ) ]
                            , [ ( emptyStyles, " " ) ]
                            , [ ( emptyStyles, "order" ) ]
                            ]
                        )
            , test "Basic Verbatim Element." <|
                \_ ->
                    Expect.equal
                        (toResult inlines "my `verbatim string`{verb}")
                        (Ok
                            [ [ ( emptyStyles, "my " ) ]
                            , [ ( emptyStyles, "verbatim string" ) ]
                            ]
                        )
            , test "Verbatim element without attributes" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "my `verbatim string`")
                        (Ok
                            [ [ Tuple.pair emptyStyles "my " ]
                            , [ Tuple.pair emptyStyles "verbatim string" ]
                            ]
                        )
            , test "Verbatim element with attributes" <|
                \_ ->
                    Expect.equal
                        (toResult inlineMultipleVerbatim
                            "my `verbatim string`  `verbatim 2`{verb2|one=one, two=two}"
                        )
                        (Ok [ [ "my " ], [ "verbatim string" ], [ "  " ], [ "verbatim 2", "one", "two" ] ])
            , test "Inline attribute order(source order)" <|
                \_ ->
                    Expect.equal
                        (toResult inlineOrder "[test]{order|one = one, two = two}")
                        (Ok
                            [ Just ( "one", "two" )
                            ]
                        )
            , test "Inline attribute order(not source order)" <|
                \_ ->
                    Expect.equal
                        (toResult inlineOrder "[test]{order|two = two, one = one}")
                        (Ok
                            [ Just ( "one", "two" )
                            ]
                        )
            , test "Basic replacement" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "my test//text")
                        (Ok [ [ Tuple.pair emptyStyles "my test/text" ] ])
            , test "replace dash" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "my test--text")
                        (Ok [ [ Tuple.pair emptyStyles "my test–text" ] ])
            , test "Inline elements should maintain escaped italics" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "[my //]{highlight} highlighted [sentence]{highlight} [order]{highlight}")
                        (Ok
                            [ [ Tuple.pair emptyStyles "my /" ]
                            , [ Tuple.pair emptyStyles " highlighted " ]
                            , [ Tuple.pair emptyStyles "sentence" ]
                            , [ Tuple.pair emptyStyles " " ]
                            , [ Tuple.pair emptyStyles "order" ]
                            ]
                        )
            , test "Inline elements should maintain multiple replacements" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "[my ////]{highlight} highlighted [sentence]{highlight} [order]{highlight}")
                        (Ok
                            [ [ Tuple.pair emptyStyles "my //" ]
                            , [ Tuple.pair emptyStyles " highlighted " ]
                            , [ Tuple.pair emptyStyles "sentence" ]
                            , [ Tuple.pair emptyStyles " " ]
                            , [ Tuple.pair emptyStyles "order" ]
                            ]
                        )
            , test "Inline elements should maintain escaped characters" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "[my \\/]{highlight} highlighted [sentence]{highlight} [order]{highlight}")
                        (Ok
                            [ [ Tuple.pair emptyStyles "my /" ]
                            , [ Tuple.pair emptyStyles " highlighted " ]
                            , [ Tuple.pair emptyStyles "sentence" ]
                            , [ Tuple.pair emptyStyles " " ]
                            , [ Tuple.pair emptyStyles "order" ]
                            ]
                        )
            , test "Incorrect inline element name" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "[my]{highlurt} highlighted sentence")
                        (Err
                            [ Error.UnknownInline [ "[some styled text]{highlight}", "`some styled text`" ]
                            ]
                        )
            ]
        , describe "Multiline"
            [ test "Correctly parse code block" <|
                \_ ->
                    Expect.equal
                        (toResult codeDoc """|> Monospace
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
                        (toResult codeAndTextDoc """|> Monospace
    Here is my first line.
    Here is my second.
    Here is my third.
    Here is my fourth.
        And my indented line.
Then some text.
""")
                        (Ok "Here is my first line.\nHere is my second.\nHere is my third.\nHere is my fourth.\n    And my indented line.\n:text")
            ]
        , describe "Nested"
            [ test "Simple list parsing.  No Nesting." <|
                \_ ->
                    Expect.equal
                        (toResult nested simpleNestedDoc)
                        (Ok [ Indexed 0 [], Indexed 1 [], Indexed 2 [] ])
            , test "Simple list parsing, maintains order" <|
                \_ ->
                    Expect.equal
                        (toResult nestedOrdering simpleNestedOrderedDoc)
                        (Ok
                            [ Ordered [ 1, 2 ] []
                            , Ordered [ 3, 4, 5 ] []
                            , Ordered [ 6 ] []
                            ]
                        )
            , test "Complex list parsing, maintains order" <|
                \_ ->
                    Expect.equal
                        (toResult nestedOrdering complexNestedOrderedDoc)
                        (Ok
                            [ Ordered [ 1, 2 ] []
                            , Ordered [ 3, 4 ]
                                [ Ordered [ 5, 6 ] []
                                , Ordered [ 7, 8 ] []
                                ]
                            , Ordered [ 9 ] []
                            ]
                        )
            , test "Nested list parsing" <|
                \_ ->
                    Expect.equal
                        (toResult nested complexNestedDoc)
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
                        (toResult nested dedentingNestedDoc)
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
                            """|> Turst
    Here's some text content
                        """
                    in
                    Expect.equal (toResult textDoc doc1)
                        (Err [ Error.UnknownBlock [ "Test" ] ])
            , test "Extra line between name and value" <|
                \_ ->
                    let
                        doc1 =
                            """|> Test
    Here's my extra line
"""

                        result =
                            Ok [ Tuple.pair emptyStyles "Here’s my extra line" ]
                    in
                    Expect.equal (toResult textDoc doc1)
                        result
            , test "Paragraph with single newline" <|
                \_ ->
                    let
                        doc1 =
                            """|> Test
    Here's my extra line
    And some additional stuff.
"""

                        result =
                            Ok [ Tuple.pair emptyStyles "Here’s my extra line\nAnd some additional stuff." ]
                    in
                    Expect.equal (toResult textDoc doc1)
                        result
            , test "Single OneOf" <|
                \_ ->
                    let
                        doc1 =
                            """|> Monospace
    Here's my extra line
"""
                    in
                    Expect.equal (toResult singleOneOf doc1)
                        (Ok "Here's my extra line\n")
            , test "Mulitline Text" <|
                \_ ->
                    let
                        doc1 =
                            """|> Monospace
    Here's my extra line
"""
                    in
                    Expect.equal (toResult codeDoc doc1)
                        (Ok "Here's my extra line\n")
            , test "Incorrect Indentation" <|
                \_ ->
                    let
                        doc1 =
                            """|> Test
  Only two spaces(should be four)

"""
                    in
                    Expect.equal (toResult textDoc doc1)
                        (Err [ Error.ExpectingIndent 4 ])
            , test "Start with Metadata" <|
                \_ ->
                    let
                        doc1 =
                            """|> Meta
    one = Test data
    two = other data

Then a bunch of

paragraphs.

Each with their own /styling/.
"""

                        result =
                            Ok
                                ( { one = "Test data", two = "other data" }
                                , [ [ Tuple.pair emptyStyles "Then a bunch of"
                                    ]
                                  , [ Tuple.pair emptyStyles "paragraphs." ]
                                  , [ Tuple.pair emptyStyles "Each with their own "
                                    , Tuple.pair { bold = False, italic = True, strike = False } "styling"
                                    , Tuple.pair emptyStyles "."
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
|> Meta
    one = Test data
    two = other data

Then a bunch of

paragraphs.

Each with their own /styling/.
"""
                    in
                    Expect.ok (toResult withMetaData doc1)
            , test "Nested section blocks" <|
                \_ ->
                    let
                        doc1 =
                            """
|> Test
    one = Test data
    two = other data
    three = other test data

Then a bunch of

paragraphs.

Each with their own /styling/.

|> Section

    Then we have embedded stuff

    and we can add other blocks like

    |> Embedded
        This is embedded

    and others
Finally, a sentence
"""
                    in
                    Expect.equal (toResult sectionDoc doc1)
                        (Ok
                            [ "record:one,two,three"
                            , "text"
                            , "text"
                            , "text"
                            , "section:text,text,embedded,text"
                            , "text"
                            ]
                        )
            ]
        , describe "Error Correction"
            [ test "Verify an int" <|
                \_ ->
                    Expect.equal
                        (toResult (Mark.document identity Mark.int) "5")
                        (Ok 5)
            , test "Verify a range on an int" <|
                \_ ->
                    Expect.equal
                        (toResult
                            (Mark.document identity
                                (Mark.int
                                    |> Mark.verify
                                        (\x ->
                                            Ok x
                                        )
                                )
                            )
                            "5"
                        )
                        (Ok 5)
            , test "Fail a range on an int" <|
                \_ ->
                    Expect.equal
                        (toResult
                            (Mark.document identity
                                (Mark.int
                                    |> Mark.verify
                                        (\x ->
                                            Err
                                                { title = "Out of range"
                                                , message =
                                                    []
                                                }
                                        )
                                )
                            )
                            "5"
                        )
                        (Err [ Error.Custom { message = [], title = "Out of range" } ])
            ]
        , describe "Records"
            [ test "Missing fields should error" <|
                \_ ->
                    let
                        doc1 =
                            """|> Test
    one = hello
    two = world
"""

                        expectedProblem =
                            Error.MissingFields [ "three" ]
                    in
                    Expect.equal (toResult recordDoc doc1)
                        (Err [ expectedProblem ])
            , test "Extra lines between fields" <|
                \_ ->
                    let
                        doc1 =
                            """|> Test
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
                            """|> Test
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
                            """|> Test
    one = hello
    two = world
    three =
        Here is a bunch of text
"""
                    in
                    Expect.equal (toResult recordManyTextDoc doc1)
                        (Ok { one = "hello", three = [ [ Tuple.pair emptyStyles "Here is a bunch of text" ] ], two = "world" })
            , test "Records with many text as a field (starting on same line)" <|
                \_ ->
                    let
                        doc1 =
                            """|> Test
    one = hello
    two = world
    three = Here is a bunch of text
"""
                    in
                    Expect.equal
                        (toResult recordManyTextDoc doc1)
                        (Ok
                            { one = "hello"
                            , three =
                                [ [ Tuple.pair emptyStyles "Here is a bunch of text" ] ]
                            , two = "world"
                            }
                        )
            , test "Records with multiple lines in field" <|
                \_ ->
                    let
                        doc1 =
                            """|> Test
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
                                [ [ Tuple.pair emptyStyles "Here is a bunch of text" ]
                                , [ Tuple.pair emptyStyles "And some more on another line" ]
                                ]
                            , two = "world"
                            }
                        )
            , test "Incorrect Indentation" <|
                \_ ->
                    let
                        doc1 =
                            """|> Test
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
                            """|> Test
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
                            """|> Test
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
                                , found = "four = huh?"
                                , recordName = "Test"
                                }
                            ]
                        )
            , test "Order of fields in source shouldn't matter" <|
                \_ ->
                    let
                        doc1 =
                            """|> Test
    one = hello
    two = world
    three = !
                        """

                        doc2 =
                            """|> Test
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
                            """|> Test
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
                            """|> Test
    one = 15
    two = -1
    three = 2
                        """
                    in
                    Expect.equal (Mark.compile intDoc doc1)
                        (Mark.Success { one = 15, two = -1, three = 2 })
            ]
        ]


textModification =
    describe "New Text"
        [ test "insert token" <|
            \_ ->
                Expect.equal
                    [ Mark.New.unstyled "Hel"
                    , Mark.Internal.Description.ExpectToken "link" []
                    , Mark.New.unstyled "lo World"
                    ]
                    (Mark.New.insertToken 3
                        "link"
                        []
                        [ Mark.New.unstyled "Hello "
                        , Mark.New.unstyled "World"
                        ]
                    )
        , test "restyle unstyled text" <|
            \_ ->
                Expect.equal
                    [ Mark.New.unstyled "Hel"
                    , Mark.New.styled Mark.New.bold "lo Wo"
                    , Mark.New.unstyled "rld"
                    ]
                    (Mark.New.restyle (Mark.New.select 3 8)
                        Mark.New.bold
                        [ Mark.New.unstyled "Hello "
                        , Mark.New.unstyled "World"
                        ]
                    )
        ]
