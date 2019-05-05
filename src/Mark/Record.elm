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
        , blockKind = Desc.Named name
        , expectations = []
        , fieldConverter =
            \desc ann ->
                case desc of
                    Desc.Record details ->
                        if details.name == name && ann == Desc.EmptyAnnotation then
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
        , blockKind = details.blockKind
        , expectations = fieldExpectation newField :: details.expectations
        , fieldConverter =
            \desc ann ->
                case details.fieldConverter desc ann of
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
            fieldParser (Desc.blockKindToContext details.blockKind) newField :: details.fields
        }


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
        { kind = details.blockKind
        , expect = expectations
        , converter =
            \desc ->
                case details.fieldConverter desc Desc.EmptyAnnotation of
                    Outcome.Success ( pos, fieldDescriptions, rendered ) ->
                        Outcome.Success rendered

                    Outcome.Failure fail ->
                        Outcome.Failure fail

                    Outcome.Almost (Desc.Uncertain e) ->
                        Outcome.Almost (Desc.Uncertain e)

                    Outcome.Almost (Desc.Recovered e ( pos, fieldDescriptions, rendered )) ->
                        Outcome.Almost (Desc.Recovered e rendered)
        , parser =
            \context seed ->
                let
                    ( parentId, parentSeed ) =
                        Id.step seed

                    ( newSeed, fields ) =
                        Id.thread parentSeed (List.reverse details.fields)
                in
                ( newSeed
                , Parse.record Parse.BlockRecord
                    parentId
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


fieldParser : Desc.ParseContext -> Field value -> Id.Seed -> ( Id.Seed, ( String, Parser Context Problem ( String, Desc.Found Desc.Description ) ) )
fieldParser context (Field name myBlock) seed =
    let
        ( newSeed, blockParser ) =
            Desc.getParser context seed myBlock
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
