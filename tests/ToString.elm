module ToString exposing (suite)

{-| -}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mark
import Mark.Edit
import Mark.Internal.Description as Description
import Mark.Internal.Id as Id
import Test exposing (..)


create exp =
    .desc <|
        Description.create
            { indent = 0
            , base = Description.startingPoint
            , expectation = exp
            , seed = Id.initialSeed
            }


createIndented exp =
    .desc <|
        Description.create
            { indent = 1
            , base = Description.startingPoint
            , expectation = exp
            , seed = Id.initialSeed
            }


record =
    Description.ExpectRecord "Circle"
        [ ( "x"
          , Description.ExpectIntBetween
                { default = 5
                , min = 10
                , max = 550
                }
          )
        , ( "y"
          , Description.ExpectIntBetween
                { default = 5
                , min = 50
                , max = 550
                }
          )
        , ( "radius"
          , Description.ExpectIntBetween
                { default = 5
                , min = 3
                , max = 10
                }
          )
        , ( "color"
          , Description.ExpectOneOf
                [ Description.ExpectStringExactly "Red"
                , Description.ExpectStringExactly "Black"
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
                            (create (Description.ExpectStringExactly "hello"))
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
                        "hello\nhello\nhello"
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
                            (create (Description.ExpectStringExactly "hello"))
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


edits =
    describe "Edit"
        [ test "Indented - Insert at 2" <|
            \_ ->
                let
                    new =
                        Mark.Edit.update
                            (Mark.Edit.insertAt 2
                                (Id.Choice
                                    manyIndentedHellosId
                                    (Description.ExpectString "world")
                                )
                            )
                            (Description.Parsed
                                { errors = []
                                , initialSeed = Id.initialSeed
                                , currentSeed = Id.initialSeed
                                , found =
                                    Description.Found
                                        { start = Description.startingPoint
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
                Expect.equal (Description.toString new)
                    """|> Indented
    hello
    hello
    world
    hello"""
        , test
            "Insert at 2"
          <|
            \_ ->
                let
                    new =
                        Mark.Edit.update
                            (Mark.Edit.insertAt 2
                                (Id.Choice
                                    (Id.Id [ 0 ])
                                    (Description.ExpectString "world")
                                )
                            )
                            (Description.Parsed
                                { errors = []
                                , initialSeed = Id.initialSeed
                                , currentSeed = Id.initialSeed
                                , found =
                                    Description.Found
                                        { start = Description.startingPoint
                                        , end = Description.startingPoint
                                        }
                                        (manyHellos ())
                                , expected =
                                    Description.ExpectManyOf
                                        [ Description.ExpectString "hello"
                                        , Description.ExpectString "hello"
                                        , Description.ExpectString "hello"
                                        ]
                                }
                            )
                in
                Expect.equal (Description.toString new) "hello\n\nhello\n\nworld\n\nhello"
        , test "Insert at 1" <|
            \_ ->
                let
                    new =
                        Mark.Edit.update
                            (Mark.Edit.insertAt 1
                                (Id.Choice
                                    (Id.Id [ 0 ])
                                    (Description.ExpectString "world")
                                )
                            )
                            (Description.Parsed
                                { errors = []
                                , initialSeed = Id.initialSeed
                                , currentSeed = Id.initialSeed
                                , found =
                                    Description.Found
                                        { start = Description.startingPoint
                                        , end = Description.startingPoint
                                        }
                                        (manyHellos ())
                                , expected =
                                    Description.ExpectManyOf
                                        [ Description.ExpectString "hello"
                                        , Description.ExpectString "hello"
                                        , Description.ExpectString "hello"
                                        ]
                                }
                            )
                in
                Expect.equal (Description.toString new) "hello\n\nworld\n\nhello\n\nhello"
        , test "Insert at 0" <|
            \_ ->
                let
                    new =
                        Mark.Edit.update
                            (Mark.Edit.insertAt 0
                                (Id.Choice
                                    (Id.Id [ 0 ])
                                    (Description.ExpectString "world")
                                )
                            )
                            (Description.Parsed
                                { errors = []
                                , initialSeed = Id.initialSeed
                                , currentSeed = Id.initialSeed
                                , found =
                                    Description.Found
                                        { start = Description.startingPoint
                                        , end = Description.startingPoint
                                        }
                                        (manyHellos ())
                                , expected =
                                    Description.ExpectManyOf
                                        [ Description.ExpectString "hello"
                                        , Description.ExpectString "hello"
                                        , Description.ExpectString "hello"
                                        ]
                                }
                            )
                in
                Expect.equal (Description.toString new) "world\n\nhello\n\nhello\n\nhello"
        , test "Insert at 3" <|
            \_ ->
                let
                    new =
                        Mark.Edit.update
                            (Mark.Edit.insertAt 3
                                (Id.Choice
                                    (Id.Id [ 0 ])
                                    (Description.ExpectString "world")
                                )
                            )
                            (Description.Parsed
                                { errors = []
                                , initialSeed = Id.initialSeed
                                , currentSeed = Id.initialSeed
                                , found =
                                    Description.Found
                                        { start = Description.startingPoint
                                        , end = Description.startingPoint
                                        }
                                        (manyHellos ())
                                , expected =
                                    Description.ExpectManyOf
                                        [ Description.ExpectString "hello"
                                        , Description.ExpectString "hello"
                                        , Description.ExpectString "hello"
                                        ]
                                }
                            )
                in
                Expect.equal (Description.toString new) "hello\n\nhello\n\nhello\n\nworld"
        ]
