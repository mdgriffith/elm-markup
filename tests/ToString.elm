module ToString exposing (edits, suite)

{-| -}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mark
import Mark.Edit
import Mark.Internal.Description as Description
import Mark.Internal.Id as Id
import Mark.New
import Test exposing (..)


startingPoint =
    { offset = 0
    , line = 1
    , column = 1
    }


bold =
    { italic = False
    , bold = True
    , strike = False
    }


create exp =
    .desc <|
        Description.create
            { indent = 0
            , base = startingPoint
            , expectation = exp
            , seed = Id.initialSeed
            }


createIndented exp =
    .desc <|
        Description.create
            { indent = 1
            , base = startingPoint
            , expectation = exp
            , seed = Id.initialSeed
            }


record =
    Description.ExpectRecord "Circle"
        [ ( "x"
          , Description.ExpectInteger 5
          )
        , ( "y"
          , Description.ExpectInteger 5
          )
        , ( "radius"
          , Description.ExpectInteger 5
          )
        , ( "color"
          , Description.ExpectOneOf
                [ Description.ExpectString "Red"
                , Description.ExpectString "Black"
                ]
          )
        ]


recordOfRecord =
    Description.ExpectRecord "Record"
        [ ( "record"
          , Description.ExpectRecord "Record"
                [ ( "value"
                  , Description.ExpectInteger 5
                  )
                ]
          )
        ]


recordOfRecordString =
    """|> Record
    record =
        |> Record
            value = 5"""


recordString =
    """|> Circle
    x = 5
    y = 5
    radius = 5
    color = Red"""


indentedRecordString =
    """|> Circle
        x = 5
        y = 5
        radius = 5
        color = Red"""


manyHellos () =
    create
        (Description.ExpectManyOf
            [ Description.ExpectString "hello"
            , Description.ExpectString "hello"
            , Description.ExpectString "hello"
            ]
        )


manyIndentedHellos () =
    create
        (Description.ExpectBlock "Indented" <|
            Description.ExpectManyOf
                [ Description.ExpectString "hello"
                , Description.ExpectString "hello"
                , Description.ExpectString "hello"
                ]
        )


suite : Test
suite =
    describe "Description"
        [ describe "toString"
            [ test "Integer" <|
                \_ ->
                    Expect.equal
                        (Description.descriptionToString
                            (create (Description.ExpectInteger 5))
                        )
                        "5"
            , test "String" <|
                \_ ->
                    Expect.equal
                        (Description.descriptionToString
                            (create (Description.ExpectString "hello"))
                        )
                        "hello"
            , test "Exact" <|
                \_ ->
                    Expect.equal
                        (Description.descriptionToString
                            (create (Description.ExpectString "hello"))
                        )
                        "hello"
            , test "Record" <|
                \_ ->
                    Expect.equal
                        (Description.descriptionToString
                            (create record)
                        )
                        recordString
            , test "RecordOfRecord" <|
                \_ ->
                    Expect.equal
                        (Description.descriptionToString
                            (create recordOfRecord)
                        )
                        recordOfRecordString
            , test "Many Strings" <|
                \_ ->
                    Expect.equal
                        (Description.descriptionToString
                            (manyHellos ())
                        )
                        "hello\n\nhello\n\nhello"
            , test "Many Indented Strings" <|
                \_ ->
                    Expect.equal
                        (Description.descriptionToString
                            (manyIndentedHellos ())
                        )
                        """|> Indented
    hello

    hello

    hello"""
            ]
        , describe "Indented - toString"
            [ test "Integer" <|
                \_ ->
                    Expect.equal
                        (Description.descriptionToString
                            (create (Description.ExpectInteger 5))
                        )
                        "5"
            , test "String" <|
                \_ ->
                    Expect.equal
                        (Description.descriptionToString
                            (create (Description.ExpectString "hello"))
                        )
                        "hello"
            , test "Exact" <|
                \_ ->
                    Expect.equal
                        (Description.descriptionToString
                            (create (Description.ExpectString "hello"))
                        )
                        "hello"
            , test "Record" <|
                \_ ->
                    Expect.equal
                        (Description.descriptionToString
                            (createIndented record)
                        )
                        indentedRecordString
            ]
        ]


manyIndentedHellosId =
    Id.Id [ 0 ]


stringText =
    Mark.text
        (\styles str ->
            str
        )


manyHelloDoc =
    Mark.document
        identity
        (Mark.block "Indented"
            identity
            (Mark.manyOf
                [ Mark.string
                ]
            )
        )


manyTextDocNoBlock =
    Mark.document
        identity
        (Mark.manyOf
            [ Mark.string
            ]
        )


styledText =
    Mark.document
        identity
        stringText


threeHellos =
    Description.Parsed
        { errors = []
        , initialSeed = Id.initialSeed
        , currentSeed = Id.initialSeed
        , found =
            Description.Found
                { start = startingPoint
                , end = startingPoint
                }
                (manyHellos ())
        , expected =
            Description.ExpectManyOf
                [ Description.ExpectString "hello"
                , Description.ExpectString "hello"
                , Description.ExpectString "hello"
                ]
        }


edits =
    describe "Edit"
        [ describe "Inserts + Deletes"
            [ test "Indented - Insert at 2" <|
                \_ ->
                    let
                        new =
                            Mark.Edit.update
                                manyHelloDoc
                                (Mark.Edit.insertAt
                                    (Id.Id [ 0 ])
                                    2
                                    (Mark.New.string "world")
                                )
                                (Description.Parsed
                                    { errors = []
                                    , initialSeed = Id.initialSeed
                                    , currentSeed = Id.initialSeed
                                    , found =
                                        Description.Found
                                            { start = startingPoint
                                            , end =
                                                { column = 1, line = 50, offset = 200 }
                                            }
                                            (manyIndentedHellos ())
                                    , expected =
                                        Description.ExpectBlock "Indented" <|
                                            Description.ExpectManyOf
                                                [ Description.ExpectString "hello"
                                                , Description.ExpectString "hello"
                                                , Description.ExpectString "hello"
                                                ]
                                    }
                                )
                    in
                    Expect.equal (Result.map Description.toString new)
                        (Ok """|> Indented
    hello

    hello

    world

    hello""")
            , test "Insert at 2" <|
                \_ ->
                    let
                        new =
                            Mark.Edit.update
                                manyTextDocNoBlock
                                (Mark.Edit.insertAt
                                    (Id.Id [ 0 ])
                                    2
                                    (Mark.New.string "world")
                                )
                                threeHellos
                    in
                    Expect.equal (Result.map Description.toString new)
                        (Ok "hello\n\nhello\n\nworld\n\nhello")
            , test "Insert at 1" <|
                \_ ->
                    let
                        new =
                            Mark.Edit.update
                                manyTextDocNoBlock
                                (Mark.Edit.insertAt
                                    (Id.Id [ 0 ])
                                    1
                                    (Mark.New.string "world")
                                )
                                threeHellos
                    in
                    Expect.equal (Result.map Description.toString new)
                        (Ok "hello\n\nworld\n\nhello\n\nhello")
            , test "Insert at 0" <|
                \_ ->
                    let
                        new =
                            Mark.Edit.update
                                manyTextDocNoBlock
                                (Mark.Edit.insertAt
                                    (Id.Id [ 0 ])
                                    0
                                    (Mark.New.string "world")
                                )
                                threeHellos
                    in
                    Expect.equal (Result.map Description.toString new)
                        (Ok "world\n\nhello\n\nhello\n\nhello")
            , test "Insert at 3" <|
                \_ ->
                    let
                        new =
                            Mark.Edit.update
                                manyTextDocNoBlock
                                (Mark.Edit.insertAt
                                    (Id.Id [ 0 ])
                                    3
                                    (Mark.New.string "world")
                                )
                                threeHellos
                    in
                    Expect.equal (Result.map Description.toString new)
                        (Ok "hello\n\nhello\n\nhello\n\nworld")
            , test "Delete at 0" <|
                \_ ->
                    let
                        new =
                            Mark.Edit.update
                                manyTextDocNoBlock
                                (Mark.Edit.delete (Id.Id [ 0 ]) 0)
                                threeHellos
                    in
                    Expect.equal (Result.map Description.toString new)
                        (Ok "hello\n\nhello")
            , test "Delete at 1" <|
                \_ ->
                    let
                        new =
                            Mark.Edit.update
                                manyTextDocNoBlock
                                (Mark.Edit.delete (Id.Id [ 0 ]) 1)
                                threeHellos
                    in
                    Expect.equal (Result.map Description.toString new)
                        (Ok "hello\n\nhello")
            , test "Delete at 2" <|
                \_ ->
                    let
                        new =
                            Mark.Edit.update
                                manyTextDocNoBlock
                                (Mark.Edit.delete (Id.Id [ 0 ]) 2)
                                threeHellos
                    in
                    Expect.equal (Result.map Description.toString new)
                        (Ok "hello\n\nhello")
            ]
        , describe "Text Edits"
            [ test "Add Bold" <|
                \_ ->
                    let
                        parseOutcome =
                            Mark.parse styledText "Hello World"

                        new =
                            case parseOutcome of
                                Mark.Success parsed ->
                                    Mark.Edit.update
                                        styledText
                                        (Mark.Edit.restyle
                                            (Id.Id [ 0 ])
                                            3
                                            8
                                            bold
                                        )
                                        parsed

                                _ ->
                                    Err []
                    in
                    Expect.equal (Result.map Description.toString new)
                        (Ok "Hel*lo Wo*rld")
            , test "Clear Bold" <|
                \_ ->
                    let
                        parseOutcome =
                            Mark.parse styledText "Hello *World*"

                        new =
                            case parseOutcome of
                                Mark.Success parsed ->
                                    Mark.Edit.update
                                        styledText
                                        (Mark.Edit.restyle
                                            (Id.Id [ 0 ])
                                            3
                                            11
                                            { bold = False
                                            , italic = False
                                            , strike = False
                                            }
                                        )
                                        parsed

                                _ ->
                                    Err []
                    in
                    Expect.equal (Result.map Description.toString new)
                        (Ok "Hello World")
            , test "Clearing styles with over-sized ranges still works as you'd expect" <|
                \_ ->
                    let
                        parseOutcome =
                            Mark.parse styledText "Hello *World*"

                        new =
                            case parseOutcome of
                                Mark.Success parsed ->
                                    Mark.Edit.update
                                        styledText
                                        (Mark.Edit.restyle
                                            (Id.Id [ 0 ])
                                            3
                                            13
                                            { bold = False
                                            , italic = False
                                            , strike = False
                                            }
                                        )
                                        parsed

                                _ ->
                                    Err []
                    in
                    Expect.equal (Result.map Description.toString new)
                        (Ok "Hello World")
            , test "Add all styles" <|
                \_ ->
                    let
                        parseOutcome =
                            Mark.parse styledText "Hello World"

                        new =
                            case parseOutcome of
                                Mark.Success parsed ->
                                    Mark.Edit.update
                                        styledText
                                        (Mark.Edit.restyle
                                            (Id.Id [ 0 ])
                                            3
                                            8
                                            { bold = True
                                            , italic = True
                                            , strike = True
                                            }
                                        )
                                        parsed

                                _ ->
                                    Err []
                    in
                    Expect.equal (Result.map Description.toString new)
                        (Ok "Hel~/*lo Wo*/~rld")
            ]
        ]
