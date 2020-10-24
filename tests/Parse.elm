module Parse exposing (text)

{-| -}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mark
import Mark.Internal.Description as Description
import Mark.Internal.Id as Id
import Mark.Internal.Parser
import Parser.Advanced as Parser
import Test exposing (..)


emptyStyles =
    { italic = False, bold = False, strike = False }


italic =
    { emptyStyles | italic = True }


bold =
    { emptyStyles | bold = True }


dummyRange =
    { end = { column = -1, line = 1, offset = -1 }
    , start = { column = -1, line = 1, offset = -1 }
    }


styleParser =
    Mark.Internal.Parser.styledText
        { inlines =
            [--Mark.annotation
             -- Description.ExpectAnnotation "test"
             -- [ Description.ExpectAttrString "attr" "placeholder"
             -- ]
             -- []
            ]
        , replacements = []
        }
        Description.ParseInline
        (Id.initialSeed "none")
        { column = 1, line = 1, offset = 0 }
        emptyStyles


text =
    describe "styled text and inlines"
        [ test "basic" <|
            \_ ->
                Expect.equal
                    (Parser.run
                        (Mark.Internal.Parser.styledText
                            { inlines = []
                            , replacements = []
                            }
                            Description.ParseInline
                            (Id.initialSeed "none")
                            { column = 1, line = 1, offset = 0 }
                            emptyStyles
                        )
                        "Here is my /styled/ *text*."
                    )
                    (Ok
                        (Description.DescribeText
                            { id = Id.Id "none" [ 0 ]
                            , text =
                                [ Description.Styled
                                    (Description.Text emptyStyles "Here is my ")
                                , Description.Styled
                                    (Description.Text italic "styled")
                                , Description.Styled
                                    (Description.Text emptyStyles " ")
                                , Description.Styled
                                    (Description.Text bold "text")
                                , Description.Styled
                                    (Description.Text emptyStyles ".")
                                ]
                            }
                        )
                    )
        , test "spacing required for control chars" <|
            \_ ->
                Expect.equal
                    (Parser.run
                        (Mark.Internal.Parser.styledText
                            { inlines = []
                            , replacements = []
                            }
                            Description.ParseInline
                            (Id.initialSeed "none")
                            { column = 1, line = 1, offset = 0 }
                            emptyStyles
                        )
                        "Here is my /styled / *text*."
                    )
                    (Ok
                        (Description.DescribeText
                            { id = Id.Id "none" [ 0 ]
                            , text =
                                [ Description.Styled
                                    (Description.Text { bold = False, italic = False, strike = False } "Here is my ")
                                , Description.Styled
                                    (Description.Text { bold = False, italic = True, strike = False } "styled / ")
                                , Description.Styled
                                    (Description.Text { bold = True, italic = True, strike = False } "text")
                                , Description.Styled
                                    (Description.Text { bold = False, italic = True, strike = False } ".")
                                ]
                            }
                        )
                    )
        , test "style that ends at the end" <|
            \_ ->
                Expect.equal
                    (Parser.run
                        (Mark.Internal.Parser.styledText
                            { inlines = []
                            , replacements = []
                            }
                            Description.ParseInline
                            (Id.initialSeed "none")
                            { column = 1, line = 1, offset = 0 }
                            emptyStyles
                        )
                        "Hello *World*"
                    )
                    (Ok
                        (Description.DescribeText
                            { id = Id.Id "none" [ 0 ]
                            , text =
                                [ Description.Styled
                                    (Description.Text
                                        { bold = False
                                        , italic = False
                                        , strike = False
                                        }
                                        "Hello "
                                    )
                                , Description.Styled
                                    (Description.Text
                                        { bold = True
                                        , italic = False
                                        , strike = False
                                        }
                                        "World"
                                    )
                                ]
                            }
                        )
                    )
        , test "stackedstyles" <|
            \_ ->
                Expect.equal
                    (Parser.run
                        (Mark.Internal.Parser.styledText
                            { inlines = []
                            , replacements = []
                            }
                            Description.ParseInline
                            (Id.initialSeed "none")
                            { column = 1, line = 1, offset = 0 }
                            emptyStyles
                        )
                        "Hello /*World*/"
                    )
                    (Ok
                        (Description.DescribeText
                            { id = Id.Id "none" [ 0 ]
                            , text =
                                [ Description.Styled
                                    (Description.Text
                                        { bold = False
                                        , italic = False
                                        , strike = False
                                        }
                                        "Hello "
                                    )
                                , Description.Styled
                                    (Description.Text
                                        { bold = True
                                        , italic = True
                                        , strike = False
                                        }
                                        "World"
                                    )
                                ]
                            }
                        )
                    )

        -- , test "basic w/ inline token" <|
        --     \_ ->
        --         Expect.equal
        --             (Parser.run
        --                 (Mark.Internal.Parser.styledText
        --                     { inlines =
        --                         [-- Description.ExpectToken "test"
        --                          -- []
        --                         ]
        --                     , replacements = []
        --                     }
        --                     (Id.initialSeed "none")
        --                     { column = 1, line = 1, offset = 0 }
        --                     emptyStyles
        --                     []
        --                 )
        --                 "Here is my /styled/ *text*.  And a {test}."
        --             )
        --             (Ok
        --                 (Description.DescribeText
        --                     { id =
        --                         Id.Id "none" [ 0 ]
        --                     , range = { end = { column = 43, line = 1, offset = 42 }, start = { column = 1, line = 1, offset = 0 } }
        --                     , text =
        --                         [ Description.Styled
        --                             { end = { column = 12, line = 1, offset = 11 }
        --                             , start = { column = 1, line = 1, offset = 0 }
        --                             }
        --                             (Description.Text emptyStyles "Here is my ")
        --                         , Description.Styled
        --                             { end = { column = 18, line = 1, offset = 17 }
        --                             , start = { column = 12, line = 1, offset = 11 }
        --                             }
        --                             (Description.Text italic "styled")
        --                         , Description.Styled
        --                             { end = { column = 19, line = 1, offset = 18 }
        --                             , start = { column = 18, line = 1, offset = 17 }
        --                             }
        --                             (Description.Text emptyStyles " ")
        --                         , Description.Styled
        --                             { end = { column = 23, line = 1, offset = 22 }
        --                             , start = { column = 19, line = 1, offset = 18 }
        --                             }
        --                             (Description.Text bold "text")
        --                         , Description.Styled
        --                             { end = { column = 32, line = 1, offset = 31 }
        --                             , start = { column = 23, line = 1, offset = 22 }
        --                             }
        --                             (Description.Text emptyStyles ".  And a ")
        --                         -- , Description.InlineToken
        --                         --     { attributes = []
        --                         --     , name = "test"
        --                         --     , range =
        --                         --         { end = { column = 42, line = 1, offset = 41 }
        --                         --         , start = { column = 36, line = 1, offset = 35 }
        --                         --         }
        --                         --     }
        --                         , Description.Styled
        --                             { end = { column = 43, line = 1, offset = 42 }
        --                             , start = { column = 42, line = 1, offset = 41 }
        --                             }
        --                             (Description.Text emptyStyles ".")
        --                         ]
        --                     }
        --                 )
        --             )
        -- , test "basic w/ inline token w/ string attr" <|
        --     \_ ->
        --         Expect.equal
        --             (Parser.run
        --                 (Mark.Internal.Parser.styledText
        --                     { inlines =
        --                         [-- Description.ExpectToken "test"
        --                          -- [ Description.ExpectAttrString "attr" "placeholder"
        --                          -- ]
        --                         ]
        --                     , replacements = []
        --                     }
        --                     (Id.initialSeed "none")
        --                     { column = 1, line = 1, offset = 0 }
        --                     emptyStyles
        --                     []
        --                 )
        --                 "Here is my /styled/ *text*.  And a {test|attr = my string}."
        --             )
        --             (Ok
        --                 (Description.DescribeText
        --                     { id = Id.Id "none" [ 0 ]
        --                     , range = { end = { column = 60, line = 1, offset = 59 }, start = { column = 1, line = 1, offset = 0 } }
        --                     , text =
        --                         [ Description.Styled
        --                             { end = { column = 12, line = 1, offset = 11 }
        --                             , start = { column = 1, line = 1, offset = 0 }
        --                             }
        --                             (Description.Text emptyStyles "Here is my ")
        --                         , Description.Styled
        --                             { end = { column = 18, line = 1, offset = 17 }
        --                             , start = { column = 12, line = 1, offset = 11 }
        --                             }
        --                             (Description.Text italic "styled")
        --                         , Description.Styled
        --                             { end = { column = 19, line = 1, offset = 18 }
        --                             , start = { column = 18, line = 1, offset = 17 }
        --                             }
        --                             (Description.Text emptyStyles " ")
        --                         , Description.Styled
        --                             { end = { column = 23, line = 1, offset = 22 }
        --                             , start = { column = 19, line = 1, offset = 18 }
        --                             }
        --                             (Description.Text bold "text")
        --                         , Description.Styled
        --                             { end = { column = 32, line = 1, offset = 31 }
        --                             , start = { column = 23, line = 1, offset = 22 }
        --                             }
        --                             (Description.Text emptyStyles ".  And a ")
        --                         -- , Description.InlineToken
        --                         --     { attributes =
        --                         --         [ Description.AttrString
        --                         --             { name = "attr"
        --                         --             , range =
        --                         --                 { end =
        --                         --                     { column = 58
        --                         --                     , line = 1
        --                         --                     , offset = 57
        --                         --                     }
        --                         --                 , start = { column = 42, line = 1, offset = 41 }
        --                         --                 }
        --                         --             , value = "my string"
        --                         --             }
        --                         --         ]
        --                         --     , name = "test"
        --                         --     , range =
        --                         --         { end = { column = 59, line = 1, offset = 58 }
        --                         --         , start = { column = 36, line = 1, offset = 35 }
        --                         --         }
        --                         --     }
        --                         , Description.Styled
        --                             { end = { column = 60, line = 1, offset = 59 }
        --                             , start = { column = 59, line = 1, offset = 58 }
        --                             }
        --                             (Description.Text emptyStyles ".")
        --                         ]
        --                     }
        --                 )
        --             )
        -- , test "basic w/ inline annotation" <|
        --     \_ ->
        --         Expect.equal
        --             (Parser.run
        --                 styleParser
        --                 "Here is my /styled/ *text*.  And a [some text]{test|attr = my string}."
        --             )
        --             basicWithInlineResult
        -- -- Stack explosion  :/
        -- , test "basic w/ inline annotation, style transfer" <|
        --     \_ ->
        --         Expect.ok
        --             (Parser.run styleParser "Here is my /styled/ *text*.  /And a [some text/]{test|attr = my string}.")
        ]


basicWithInlineResult =
    Ok
        (Description.DescribeText
            { id =
                Id.Id "none" [ 0 ]
            , text =
                [ Description.Styled
                    (Description.Text emptyStyles "Here is my ")
                , Description.Styled
                    (Description.Text italic "styled")
                , Description.Styled
                    (Description.Text emptyStyles " ")
                , Description.Styled
                    (Description.Text bold "text")
                , Description.Styled
                    (Description.Text emptyStyles ".  And a ")

                -- , Description.InlineAnnotation
                --     { name = "test"
                --     , attributes =
                --         [ Description.AttrString
                --             { name = "attr"
                --             , range =
                --                 { end = { column = 69, line = 1, offset = 68 }
                --                 , start = { column = 53, line = 1, offset = 52 }
                --                 }
                --             , value = "my string"
                --             }
                --         ]
                --     , range =
                --         { end = { column = 70, line = 1, offset = 69 }
                --         , start = { column = 36, line = 1, offset = 35 }
                --         }
                --     , text = [ Description.Text emptyStyles "some text" ]
                --     }
                , Description.Styled
                    (Description.Text emptyStyles ".")
                ]
            }
        )


styleTransfer =
    Description.DescribeText
        { id =
            Id.Id "none" [ 0 ]
        , text =
            [ Description.Styled
                (Description.Text emptyStyles "Here is my ")
            , Description.Styled
                (Description.Text italic "styled")
            , Description.Styled
                (Description.Text emptyStyles " ")
            , Description.Styled
                (Description.Text bold "text")
            , Description.Styled
                (Description.Text emptyStyles ".  ")
            , Description.Styled
                (Description.Text italic "And a ")

            -- , Description.InlineAnnotation
            --     { name = "test"
            --     , attributes =
            --         [ Description.AttrString
            --             { name = "attr"
            --             , range =
            --                 { end = { column = 71, line = 1, offset = 70 }
            --                 , start = { column = 55, line = 1, offset = 54 }
            --                 }
            --             , value = " my string"
            --             }
            --         ]
            --     , range =
            --         { end = { column = 72, line = 1, offset = 71 }
            --         , start = { column = 37, line = 1, offset = 36 }
            --         }
            --     , text = [ Description.Text italic "some text" ]
            --     }
            , Description.Styled
                (Description.Text emptyStyles ".")
            ]
        }
