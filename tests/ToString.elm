module ToString exposing (edits, suite)

{-| -}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mark
import Mark.Description as Description
import Mark.Internal.Id as Id
import Test exposing (..)


create exp =
    Tuple.second <|
        Description.create
            0
            Description.startingPoint
            exp


createIndented exp =
    Tuple.second <|
        Description.create
            1
            Description.startingPoint
            exp


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
    """| Record
    record =
        | Record
            value = 5"""


recordString =
    """| Circle
    x = 5
    y = 5
    radius = 5
    color = Red"""


indentedRecordString =
    """| Circle
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
                            (Debug.log "record" (create record))
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
                        """| Indented
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
    Id.Id
        { end = { column = 5, line = 2, offset = 5 }
        , start = { column = 5, line = 2, offset = 5 }
        }


edits =
    describe "Edit"
        [ test "Indented - Insert at 2" <|
            \_ ->
                let
                    new =
                        Description.update
                            (Description.insertAt 2
                                (Id.Choice
                                    manyIndentedHellosId
                                    (Description.ExpectString "world")
                                )
                            )
                            (Description.Parsed
                                { errors = []
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
                                , focus = Nothing
                                }
                            )
                in
                Expect.equal (Description.toString new)
                    """| Indented
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
                        Description.update
                            (Description.insertAt 2
                                (Id.Choice
                                    (Id.Id
                                        { start = Description.startingPoint
                                        , end = Description.startingPoint
                                        }
                                    )
                                    (Description.ExpectString "world")
                                )
                            )
                            (Description.Parsed
                                { errors = []
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
                                , focus = Nothing
                                }
                            )
                in
                Expect.equal (Description.toString new) "hello\n\nhello\n\nworld\n\nhello"
        , test "Insert at 1" <|
            \_ ->
                let
                    new =
                        Description.update
                            (Description.insertAt 1
                                (Id.Choice
                                    (Id.Id
                                        { start = Description.startingPoint
                                        , end = Description.startingPoint
                                        }
                                    )
                                    (Description.ExpectString "world")
                                )
                            )
                            (Description.Parsed
                                { errors = []
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
                                , focus = Nothing
                                }
                            )
                in
                Expect.equal (Description.toString new) "hello\n\nworld\n\nhello\n\nhello"
        , test "Insert at 0" <|
            \_ ->
                let
                    new =
                        Description.update
                            (Description.insertAt 0
                                (Id.Choice
                                    (Id.Id
                                        { start = Description.startingPoint
                                        , end = Description.startingPoint
                                        }
                                    )
                                    (Description.ExpectString "world")
                                )
                            )
                            (Description.Parsed
                                { errors = []
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
                                , focus = Nothing
                                }
                            )
                in
                Expect.equal (Description.toString new) "world\n\nhello\n\nhello\n\nhello"
        , test "Insert at 3" <|
            \_ ->
                let
                    new =
                        Description.update
                            (Description.insertAt 3
                                (Id.Choice
                                    (Id.Id
                                        { start = Description.startingPoint
                                        , end = Description.startingPoint
                                        }
                                    )
                                    (Description.ExpectString "world")
                                )
                            )
                            (Description.Parsed
                                { errors = []
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
                                , focus = Nothing
                                }
                            )
                in
                Expect.equal (Description.toString new) "hello\n\nhello\n\nhello\n\nworld"
        ]
