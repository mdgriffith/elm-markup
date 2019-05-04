module Mark.Record exposing (Record, record, field, toBlock)

{-|

@docs Record, record, field, toBlock

-}

import Mark.Internal.Description as Desc
import Mark.Internal.Error as Error exposing (AstError(..), Context(..), Problem(..))
import Mark.Internal.Id as Id
import Mark.Internal.Outcome as Outcome
import Mark.Internal.Parser as Parse
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


{-| -}
type alias Record a =
    Desc.Record a


{-| -}
type alias Block data =
    Desc.Block data


{-| Parse a record with any number of fields.

    Mark.record "Image"
        (\src description ->
            Html.img
                [ Html.Attributes.src src
                , Html.Attributes.alt description
                ]
                []
        )
        |> Mark.field "src" Mark.string
        |> Mark.field "description" Mark.string
        |> Mark.toBlock

would parse the following markup:

```markup
|> Image
    src = http://placekitten/200/500
    description = What a cutie.
```

Fields can be in any order in the markup. Also, by convention field names should be `camelCase`. This might be enforced in the future.

-}
record : String -> data -> Record data
record name view =
    Desc.ProtoRecord
        { name = name
        , expectations = []
        , fieldConverter =
            \desc ->
                case desc of
                    Desc.Record details ->
                        if details.name == name then
                            case details.found of
                                Desc.Found pos fieldDescriptions ->
                                    Outcome.Success ( pos, fieldDescriptions, view )

                                Desc.Unexpected unexpected ->
                                    Desc.uncertain unexpected

                        else
                            Outcome.Failure NoMatch

                    _ ->
                        Outcome.Failure NoMatch
        , fields = []
        }


{-| -}
field : String -> Block value -> Record (value -> result) -> Record result
field name value (Desc.ProtoRecord details) =
    let
        newField =
            Field name value
    in
    Desc.ProtoRecord
        { name = details.name
        , expectations = fieldExpectation newField :: details.expectations
        , fieldConverter =
            \desc ->
                case details.fieldConverter desc of
                    Outcome.Success ( pos, fieldDescriptions, rendered ) ->
                        case getField newField fieldDescriptions of
                            Ok (Desc.Found rng myField) ->
                                Outcome.Success
                                    ( pos
                                    , fieldDescriptions
                                    , rendered myField
                                    )

                            Ok (Desc.Unexpected deets) ->
                                Desc.uncertain deets

                            Err prob ->
                                Desc.uncertain
                                    { problem = prob
                                    , range = pos
                                    }

                    Outcome.Failure fail ->
                        Outcome.Failure fail

                    Outcome.Almost (Desc.Uncertain e) ->
                        Outcome.Almost (Desc.Uncertain e)

                    Outcome.Almost (Desc.Recovered e ( pos, fieldDescriptions, rendered )) ->
                        case getField newField fieldDescriptions of
                            Ok (Desc.Found rng myField) ->
                                Outcome.Almost
                                    (Desc.Recovered e
                                        ( pos
                                        , fieldDescriptions
                                        , rendered myField
                                        )
                                    )

                            Ok (Desc.Unexpected deets) ->
                                Desc.uncertain deets

                            Err prob ->
                                Desc.uncertain
                                    { problem = prob
                                    , range = pos
                                    }
        , fields =
            fieldParser newField :: details.fields
        }


{-| Convert a `Record` to a `Block`.
-}
toBlock : Record a -> Block a
toBlock (Desc.ProtoRecord details) =
    let
        expectations =
            Desc.ExpectRecord details.name
                details.expectations
    in
    Desc.Block
        { kind = Desc.Named details.name
        , expect = expectations
        , converter =
            \desc ->
                case details.fieldConverter desc of
                    Outcome.Success ( pos, fieldDescriptions, rendered ) ->
                        Outcome.Success rendered

                    Outcome.Failure fail ->
                        Outcome.Failure fail

                    Outcome.Almost (Desc.Uncertain e) ->
                        Outcome.Almost (Desc.Uncertain e)

                    Outcome.Almost (Desc.Recovered e ( pos, fieldDescriptions, rendered )) ->
                        Outcome.Almost (Desc.Recovered e rendered)
        , parser =
            \seed ->
                let
                    ( parentId, parentSeed ) =
                        Id.step seed

                    ( newSeed, fields ) =
                        Id.thread parentSeed (List.reverse details.fields)
                in
                ( newSeed
                , parseRecord parentId
                    details.name
                    expectations
                    fields
                )
        }


{-| -}
type Field value
    = Field String (Block value)


{-| -}
fieldOld : String -> Block value -> Field value
fieldOld name child =
    Field name child


fieldParser : Field value -> Id.Seed -> ( Id.Seed, ( String, Parser Context Problem ( String, Desc.Found Desc.Description ) ) )
fieldParser (Field name myBlock) seed =
    let
        ( newSeed, blockParser ) =
            Desc.getParser seed myBlock
    in
    ( newSeed
    , ( name
      , withFieldName
            name
            blockParser
      )
    )


fieldName : Field v -> String
fieldName (Field name _) =
    name


fieldExpectation (Field name fieldBlock) =
    ( name, Desc.getBlockExpectation fieldBlock )



{- RECORD PARSER HELPERS -}


backtrackCharacters chars range =
    { start =
        { offset = range.start.offset - chars
        , line = range.start.line
        , column = range.start.column - chars
        }
    , end = range.end
    }


parseRecord :
    Id.Id
    -> String
    -> Desc.Expectation
    -> List ( String, Parser Context Problem ( String, Desc.Found Desc.Description ) )
    -> Parser Context Problem Desc.Description
parseRecord id recordName expectations fields =
    Parser.succeed
        (\result ->
            case result of
                Ok details ->
                    Desc.Record
                        { expected = expectations
                        , id = id
                        , name = recordName
                        , found =
                            Desc.Found (backtrackCharacters 2 details.range) details.value
                        }

                -- Err ( maybePosition, prob ) ->
                Err err ->
                    Desc.Record
                        { expected = expectations
                        , id = id
                        , name = recordName
                        , found =
                            Desc.Unexpected
                                { range = Maybe.withDefault (backtrackCharacters 2 err.range) (Tuple.first err.error)
                                , problem = Tuple.second err.error
                                }
                        }
        )
        |= Parse.withRangeResult
            (Parse.withIndent
                (\indentation ->
                    Parser.succeed identity
                        |. Parser.keyword (Parser.Token recordName (ExpectingBlockName recordName))
                        |. Parser.chompWhile (\c -> c == ' ')
                        |. Parser.chompIf (\c -> c == '\n') Newline
                        |= Parser.withIndent (indentation + 4)
                            (Parser.loop
                                { remaining = fields
                                , found = Ok []
                                }
                                (parseFields recordName (List.map Tuple.first fields))
                            )
                )
            )


withFieldName : String -> Parser Error.Context Error.Problem Desc.Description -> Parser Error.Context Error.Problem ( String, Desc.Found Desc.Description )
withFieldName name parser =
    Parse.withIndent
        (\indentation ->
            Parser.map
                (\( pos, description ) ->
                    ( name, Desc.Found pos description )
                )
            <|
                Parse.withRange
                    (Parser.succeed identity
                        |. Parser.keyword (Parser.Token name (ExpectingFieldName name))
                        |. Parser.chompWhile (\c -> c == ' ')
                        |. Parser.chompIf (\c -> c == '=') (Expecting "=")
                        |. Parser.chompWhile (\c -> c == ' ')
                        |= Parser.oneOf
                            [ Parser.withIndent (indentation + 4) (Parser.inContext (InRecordField name) parser)
                            , Parser.succeed identity
                                |. Parser.chompWhile (\c -> c == '\n')
                                |. Parser.token (Parser.Token (String.repeat (indentation + 4) " ") (ExpectingIndentation indentation))
                                |= Parser.withIndent (indentation + 4) (Parser.inContext (InRecordField name) parser)
                            ]
                    )
        )


unexpectedField recordName options =
    Parse.withIndent
        (\indentation ->
            Parser.map
                (\{ range, value } ->
                    ( value
                    , Desc.Unexpected
                        { range = range
                        , problem =
                            Error.UnexpectedField
                                { found = value
                                , options = options
                                , recordName = recordName
                                }
                        }
                    )
                )
                (Parse.getRangeAndSource
                    (Parser.succeed identity
                        |= Parser.getChompedString (Parser.chompWhile Char.isAlphaNum)
                        |. Parser.chompWhile (\c -> c == ' ')
                        |. Parser.chompIf (\c -> c == '=') (Expecting "=")
                        |. Parser.chompWhile (\c -> c == ' ')
                        -- TODO: parse multiline string
                        |. Parser.withIndent (indentation + 4) (Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n')))
                     -- |. Parse.newline
                     -- |. Parser.map (Debug.log "unexpected capture") (Parser.loop "" (raggedIndentedStringAbove (indent - 4)))
                    )
                )
        )


renderRecordResult pos result =
    case result of
        Ok parsedCorrectly ->
            case parsedCorrectly of
                Ok rendered ->
                    Outcome.Success rendered

                Err unexpected ->
                    Desc.uncertain unexpected

        Err prob ->
            Desc.uncertain
                { problem = prob
                , range = pos
                }


type alias RecordFields =
    { remaining :
        List ( String, Parser Context Problem ( String, Desc.Found Desc.Description ) )
    , found :
        Result ( Maybe Desc.Range, Error.Error ) (List ( String, Desc.Found Desc.Description ))
    }


type Indented thing
    = Indented thing
    | WeirdIndent Int
    | EmptyLine


{-| Either:

    1. Parses indent ++ parser ++ Parse.newline
        -> Outcome.Success!
    2. Parses many spaces ++ Parse.newline
        -> Ignore completely
    3. Parses some number of spaces ++ some not Parse.newlines ++ Parse.newline
        -> Is improperly indented

-}
indentOrSkip :
    Int
    -> Parser Context Problem (Parser.Step RecordFields a)
    -> Parser Context Problem (Indented (Parser.Step RecordFields a))
indentOrSkip indentation successParser =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.token (Parser.Token (String.repeat indentation " ") (ExpectingIndentation indentation))
            |= Parser.oneOf
                [ Parser.map (always EmptyLine) Parse.newline
                , Parser.succeed
                    (\foundIndent content ->
                        if content /= "" then
                            WeirdIndent (String.length foundIndent)

                        else
                            EmptyLine
                    )
                    |. Parser.chompIf (\c -> c == ' ') Space
                    |= Parser.getChompedString (Parser.chompWhile (\c -> c == ' '))
                    |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n'))
                    |. Parse.newlineWith "indentOrSkip one"

                -- parse field
                , Parser.succeed Indented
                    |= successParser

                -- |. Parse.newlineWith "indentOrSkip two"
                ]

        -- We're here because there is less than the desired indent.
        , Parser.succeed
            (\foundIndent hasContent ->
                if hasContent then
                    WeirdIndent (String.length foundIndent)

                else
                    EmptyLine
            )
            |= Parser.getChompedString (Parser.chompWhile (\c -> c == ' '))
            |= Parser.oneOf
                [ Parser.map (always False) Parse.newline
                , Parser.succeed True
                    |. Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n'))
                    |. Parse.newline
                ]
        ]


{-| -}
parseFields :
    String
    -> List String
    -> RecordFields
    -> Parser Context Problem (Parser.Step RecordFields (Result ( Maybe Desc.Range, Error.Error ) (List ( String, Desc.Found Desc.Description ))))
parseFields recordName fieldNames fields =
    case fields.remaining of
        [] ->
            Parse.withIndent
                (\indentation ->
                    Parser.succeed
                        (\remaining ->
                            if String.trim remaining == "" then
                                Parser.Done fields.found

                            else
                                Parser.Done
                                    (Err
                                        ( Nothing
                                        , Error.UnexpectedField
                                            { options = fieldNames
                                            , found = String.trim remaining
                                            , recordName = recordName
                                            }
                                        )
                                    )
                        )
                        |= Parser.oneOf
                            [ Parser.succeed identity
                                |. Parser.token
                                    (Parser.Token (String.repeat indentation " ")
                                        (ExpectingIndentation indentation)
                                    )
                                |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n'))
                            , Parser.succeed ""
                            ]
                )

        _ ->
            case fields.found of
                Ok found ->
                    Parse.withIndent
                        (\indentation ->
                            Parser.oneOf
                                [ indentOrSkip indentation (captureField found recordName fields fieldNames)
                                    |> Parser.map
                                        (\indentedField ->
                                            case indentedField of
                                                Indented thing ->
                                                    thing

                                                EmptyLine ->
                                                    Parser.Loop fields

                                                WeirdIndent i ->
                                                    Parser.Loop
                                                        { found =
                                                            Err ( Nothing, Error.ExpectingIndent indentation )
                                                        , remaining =
                                                            fields.remaining
                                                        }
                                        )

                                -- We've reached here because:
                                -- 1. We still have expected fields, but we didn't parse them.
                                -- 2. No other errors occurred.
                                -- 3. We did not find the correct indentation
                                -- 4. And This is not a blank line
                                -- So, the only thing left is that we have some fields that we didn't parse
                                , Parser.succeed
                                    (Parser.Done
                                        (Err
                                            ( Nothing, Error.MissingFields (List.map Tuple.first fields.remaining) )
                                        )
                                    )
                                ]
                        )

                Err unexpected ->
                    -- We've encountered an error, but we still need to parse
                    -- the entire indented block.  so that the parser can continue.
                    Parse.withIndent
                        (\indentation ->
                            Parser.succeed (Parser.Done fields.found)
                                |. Parser.loop "" (Parse.raggedIndentedStringAbove (indentation - 4))
                        )


captureField :
    List ( String, Desc.Found Desc.Description )
    -> String
    -> RecordFields
    -> List String
    -> Parser Context Problem (Parser.Step RecordFields a)
captureField found recordName fields fieldNames =
    Parser.map
        (\maybeField ->
            case maybeField of
                Nothing ->
                    Parser.Loop fields

                Just ( foundFieldname, fieldValue ) ->
                    case fieldValue of
                        Desc.Found _ _ ->
                            Parser.Loop
                                { found = Ok (( foundFieldname, fieldValue ) :: found)
                                , remaining =
                                    List.filter
                                        (\( fieldParserName, _ ) -> fieldParserName /= foundFieldname)
                                        fields.remaining
                                }

                        Desc.Unexpected unexpected ->
                            Parser.Loop
                                { found = Err ( Just unexpected.range, unexpected.problem )
                                , remaining =
                                    List.filter
                                        (\( fieldParserName, _ ) -> fieldParserName /= foundFieldname)
                                        fields.remaining
                                }
        )
        (Parser.oneOf
            (List.map (Parser.map Just << Tuple.second) fields.remaining
                ++ [ Parser.map Just (unexpectedField recordName fieldNames)
                   ]
            )
        )


getField :
    Field value
    -> List ( String, Desc.Found Desc.Description )
    -> Result Error.Error (Desc.Found value)
getField (Field name fieldBlock) fields =
    List.foldl (matchField name fieldBlock) (Err (Error.MissingFields [ name ])) fields


matchField :
    String
    -> Block value
    -> ( String, Desc.Found Desc.Description )
    -> Result Error.Error (Desc.Found value)
    -> Result Error.Error (Desc.Found value)
matchField targetName targetBlock ( name, foundDescription ) existing =
    case existing of
        Ok _ ->
            existing

        Err err ->
            if name == targetName then
                case foundDescription of
                    Desc.Found rng description ->
                        case Desc.renderBlock targetBlock description of
                            Outcome.Success rendered ->
                                Ok (Desc.Found rng rendered)

                            Outcome.Almost invalidAst ->
                                Err err

                            Outcome.Failure _ ->
                                Err err

                    Desc.Unexpected unexpected ->
                        Ok (Desc.Unexpected unexpected)

            else
                existing
