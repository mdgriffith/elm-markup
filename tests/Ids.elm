module Ids exposing (..)

{-| -}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mark
import Mark.Edit
import Mark.Internal.Description as Description
import Mark.Internal.Error as Error
import Mark.Internal.Id as Id
import Mark.New
import Test exposing (..)


sectionDoc =
    Mark.document
        [ Mark.withId (\i b -> [ Tuple.pair i b ]) <|
            Mark.map (String.join ":" << List.map Tuple.second) text
        , idRecord
        , Mark.withId
            (\x y ->
                ( x, "section block" ) :: y
            )
          <|
            Mark.block "Section"
                List.concat
                (Mark.manyOf
                    [ Mark.withId
                        (\x y ->
                            ( x, "embedded block" ) :: y
                        )
                      <|
                        Mark.block "Embedded"
                            identity
                            (Mark.withId (\i b -> [ Tuple.pair i ("!!" ++ b) ]) <|
                                Mark.map (String.join ":" << List.map Tuple.second) text
                            )
                    , Mark.withId (\i b -> [ Tuple.pair i b ]) <|
                        Mark.map (String.join ":" << List.map Tuple.second) text
                    ]
                )
        , Mark.withId
            (\x y -> ( x, "list block" ) :: y)
            list
        ]


idRecord =
    Mark.withId
        (\i b -> ( i, "record main!" ) :: b)
        (Mark.record "Test"
            (\one two three ->
                [ one
                , two
                , three
                ]
            )
            |> Mark.field "one" (Mark.withId Tuple.pair Mark.string)
            |> Mark.field "two" (Mark.withId Tuple.pair Mark.string)
            |> Mark.field "three" (Mark.withId Tuple.pair Mark.string)
            |> Mark.toBlock
        )


list : Mark.Block (List ( Id.Id, String ))
list =
    Mark.block "List"
        identity
        (Mark.tree
            renderList
            (Mark.withId (\i b -> [ Tuple.pair i b ]) <|
                Mark.map (String.join "--" << List.map Tuple.second) text
            )
        )


{-| Note: we have to define this as a separate function because
`Enumerated` and `Item` are a pair of mutually recursive data structures.
It's easiest to render them using two separate functions: renderList and renderItem
-}
renderList : Mark.Enumerated (List ( Id.Id, String )) -> List ( Id.Id, String )
renderList (Mark.Enumerated enum) =
    List.concatMap renderItem enum.items


renderItem : Mark.Item (List ( Id.Id, String )) -> List ( Id.Id, String )
renderItem (Mark.Item item) =
    case item.children of
        Mark.Enumerated enum ->
            case enum.items of
                [] ->
                    List.concat item.content

                _ ->
                    List.concat item.content ++ renderList item.children


text =
    Mark.text Tuple.pair


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


|> List
    1.  This is definitely the first thing.
        Add all together now
        With some Content

    -- Another thing.

        1. sublist

        -- more sublist

            -- indented


        -- other sublist

            -- subthing

            -- other subthing

    -- and yet, another
        --  and another one
            With some content



Each with their own /styling/.

|> Section

    Then we have embedded stuff

    and we can add other blocks like

    |> Embedded
        This is embedded

    and others
Finally, a sentence

"""


getProblem renderedError =
    case renderedError of
        Error.Rendered details ->
            details.problem

        Error.Global details ->
            details.problem


toResult doc src =
    case Mark.compile doc src of
        Mark.Success ( _, success ) ->
            Ok success

        Mark.Failure errs ->
            Err (List.map getProblem errs)

        Mark.Almost { errors } ->
            Err (List.map getProblem errors)


suite =
    describe "ID handling"
        [ test "IDs are not duplicated" <|
            \_ ->
                case toResult sectionDoc doc1 of
                    Err errs ->
                        let
                            _ =
                                Debug.log "error" errs
                        in
                        Expect.fail "Document failed to parse for tests"

                    Ok data ->
                        let
                            stringIds =
                                List.concatMap (List.map (Mark.idToString << Tuple.first)) data

                            -- _ =
                            --     List.map (Debug.log "ids") data
                        in
                        Expect.true "All IDs found are unique"
                            (stringIds
                                |> List.all
                                    (\str ->
                                        List.filter (\s -> s == str) stringIds
                                            |> List.length
                                            |> (\len -> len == 1)
                                    )
                            )
        ]
