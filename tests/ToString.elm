module ToString exposing (edits, suite)

{-| -}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mark
import Mark.Edit
import Mark.Error
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


create new =
    .desc <|
        Description.create (Id.initialSeed "none") new


createIndented new =
    .desc <|
        Description.create (Id.initialSeed "none") new


record =
    Description.NewRecord "Circle"
        [ ( "x"
          , Description.NewInteger 5
          )
        , ( "y"
          , Description.NewInteger 5
          )
        , ( "radius"
          , Description.NewInteger 5
          )
        , ( "color"
          , Description.NewString "Red"
          )
        ]


recordOfRecord =
    Description.NewRecord "Record"
        [ ( "record"
          , Description.NewRecord "Record"
                [ ( "value"
                  , Description.NewInteger 5
                  )
                ]
          )
        ]


recordString =
    """|> Circle
    x = 5
    y = 5
    radius = 5
    color = Red
"""


manyHellos () =
    create
        (Description.NewGroup
            [ Description.NewString "hello"
            , Description.NewString "hello"
            , Description.NewString "hello"
            ]
        )


manyIndentedHellos () =
    create
        (Description.NewBlock "Indented" <|
            Description.NewGroup
                [ Description.NewString "hello"
                , Description.NewString "hello"
                , Description.NewString "hello"
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
                            (create (Description.NewInteger 5))
                        )
                        "5"
            , test "String" <|
                \_ ->
                    Expect.equal
                        (Description.descriptionToString
                            (create (Description.NewString "hello"))
                        )
                        "hello"
            , test "Exact" <|
                \_ ->
                    Expect.equal
                        (Description.descriptionToString
                            (create (Description.NewString "hello"))
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
                        """|> Record
    record =
        |> Record
            value = 5
"""
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
        ]


manyIndentedHellosId =
    Id.Id "none" [ 0 ]


stringText =
    Mark.text
        (\styles str ->
            str
        )


manyHelloDoc =
    Mark.document
        [ Mark.block "Indented"
            identity
            (Mark.manyOf
                [ Mark.string
                ]
            )
        ]


manyTextDocNoBlock =
    Mark.document
        [ Mark.string
        ]


styledText =
    Mark.document
        [ stringText ]


threeHellos =
    Description.Parsed
        { errors = []
        , initialSeed = Id.initialSeed "none"
        , currentSeed = Id.initialSeed "none"
        , found =
            manyHellos ()
        , expected =
            Description.ExpectManyOf
                [ Description.ExpectString "hello"
                , Description.ExpectString "hello"
                , Description.ExpectString "hello"
                ]
        , attributes = []
        }


edits =
    describe "Edit"
        [ describe "Inserts + Deletes"
            [ test "Indented - Insert at 2" <|
                \_ ->
                    let
                        new : Result (List Mark.Error.Error) Description.Parsed
                        new =
                            Mark.Edit.update
                                manyHelloDoc
                                (Mark.Edit.insertAfter
                                    (Id.Id "none" [ 0, 2 ])
                                    [ Mark.New.string "world" ]
                                )
                                (Description.Parsed
                                    { errors = []
                                    , initialSeed = Id.initialSeed "none"
                                    , currentSeed = Id.initialSeed "none"
                                    , found =
                                        manyIndentedHellos ()
                                    , expected =
                                        Description.ExpectBlock "Indented" <|
                                            Description.ExpectManyOf
                                                [ Description.ExpectString "hello"
                                                , Description.ExpectString "hello"
                                                , Description.ExpectString "hello"
                                                ]
                                    , attributes = []
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
                        new : Result (List Mark.Error.Error) Description.Parsed
                        new =
                            Mark.Edit.update
                                manyTextDocNoBlock
                                (Mark.Edit.insertAfter
                                    (Id.Id "none" [ 1, 1 ])
                                    [ Mark.New.string "world" ]
                                )
                                threeHellos
                    in
                    Expect.equal (Result.map Description.toString new)
                        (Ok "hello\n\nhello\n\nworld\n\nhello")
            , test "Insert at 1" <|
                \_ ->
                    let
                        new : Result (List Mark.Error.Error) Description.Parsed
                        new =
                            Mark.Edit.update
                                manyTextDocNoBlock
                                (Mark.Edit.insertAfter
                                    (Id.Id "none" [ 0, 1 ])
                                    [ Mark.New.string "world" ]
                                )
                                threeHellos
                    in
                    Expect.equal (Result.map Description.toString new)
                        (Ok "hello\n\nworld\n\nhello\n\nhello")
            , test "Insert at 3" <|
                \_ ->
                    let
                        new : Result (List Mark.Error.Error) Description.Parsed
                        new =
                            Mark.Edit.update
                                manyTextDocNoBlock
                                (Mark.Edit.insertAfter
                                    (Id.Id "none" [ 2, 1 ])
                                    [ Mark.New.string "world" ]
                                )
                                threeHellos
                    in
                    Expect.equal (Result.map Description.toString new)
                        (Ok "hello\n\nhello\n\nhello\n\nworld")
            , test "Delete at 0" <|
                \_ ->
                    let
                        new : Result (List Mark.Error.Error) Description.Parsed
                        new =
                            Mark.Edit.update
                                manyTextDocNoBlock
                                (Mark.Edit.delete [ Id.Id "none" [ 0, 1 ] ])
                                threeHellos
                    in
                    Expect.equal (Result.map Description.toString new)
                        (Ok "hello\n\nhello")
            , test "Delete at 1" <|
                \_ ->
                    let
                        new : Result (List Mark.Error.Error) Description.Parsed
                        new =
                            Mark.Edit.update
                                manyTextDocNoBlock
                                (Mark.Edit.delete [ Id.Id "none" [ 1, 1 ] ])
                                threeHellos
                    in
                    Expect.equal (Result.map Description.toString new)
                        (Ok "hello\n\nhello")
            , test "Delete at 2" <|
                \_ ->
                    let
                        new : Result (List Mark.Error.Error) Description.Parsed
                        new =
                            Mark.Edit.update
                                manyTextDocNoBlock
                                (Mark.Edit.delete [ Id.Id "none" [ 2, 1 ] ])
                                threeHellos
                    in
                    Expect.equal (Result.map Description.toString new)
                        (Ok "hello\n\nhello")
            ]
        , describe "Text Edits"
            [ test "Add Bold" <|
                \_ ->
                    let
                        parseOutcome : Mark.Outcome (List Mark.Error.Error) (Mark.Partial Mark.Parsed) Mark.Parsed
                        parseOutcome =
                            Mark.parse styledText "Hello World"

                        new : Result (List Mark.Error.Error) Description.Parsed
                        new =
                            case parseOutcome of
                                Mark.Success parsed ->
                                    Mark.Edit.update
                                        styledText
                                        (Mark.Edit.restyle
                                            (Id.Id "none" [ 0, 0 ])
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
                        parseOutcome : Mark.Outcome (List Mark.Error.Error) (Mark.Partial Mark.Parsed) Mark.Parsed
                        parseOutcome =
                            Mark.parse styledText "Hello *World*"

                        new : Result (List Mark.Error.Error) Description.Parsed
                        new =
                            case parseOutcome of
                                Mark.Success parsed ->
                                    Mark.Edit.update
                                        styledText
                                        (Mark.Edit.restyle
                                            (Id.Id "none" [ 0, 0 ])
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
                        parseOutcome : Mark.Outcome (List Mark.Error.Error) (Mark.Partial Mark.Parsed) Mark.Parsed
                        parseOutcome =
                            Mark.parse styledText "Hello *World*"

                        new : Result (List Mark.Error.Error) Description.Parsed
                        new =
                            case parseOutcome of
                                Mark.Success parsed ->
                                    Mark.Edit.update
                                        styledText
                                        (Mark.Edit.restyle
                                            (Id.Id "none" [ 0, 0 ])
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
                        parseOutcome : Mark.Outcome (List Mark.Error.Error) (Mark.Partial Mark.Parsed) Mark.Parsed
                        parseOutcome =
                            Mark.parse styledText "Hello World"

                        new : Result (List Mark.Error.Error) Description.Parsed
                        new =
                            case parseOutcome of
                                Mark.Success parsed ->
                                    Mark.Edit.update
                                        styledText
                                        (Mark.Edit.restyle
                                            (Id.Id "none" [ 0, 0 ])
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
