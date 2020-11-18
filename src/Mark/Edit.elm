module Mark.Edit exposing
    ( update, Id, Edit
    , Offset
    , insertString, insertText, deleteText
    , Styles, restyle, addStyles, removeStyles
    , annotate, verbatim
    , replace, delete, insertAfter, insertFirst
    , copy, copyText
    )

{-| This module allows you to make **edits** to `Parsed`, that intermediate data structure we talked about in [`Mark`](Mark).

This means you can build an editor for your document.

In order to make edits to your document you need an [`Id`](#Id) and an [`Edit`](#Edit).

Once you have those you can [`update`](#update) your document, which can succeed or fail depending on if the edit was valid.


# Updating `Parsed`

@docs update, Id, Edit


# Text Edits

Here are edits you can make against [`Mark.text`](Mark#text) and [`Mark.textWith`](Mark#textWith) blocks.

**Note** These edits don't apply to [`Mark.string`](Mark#string). If you want to modify a `Mark.string`, use [`Mark.Edit.replace`](Mark-Edit#replace).

@docs Offset

@docs insertString, insertText, insertString, deleteText

@docs Styles, restyle, addStyles, removeStyles

@docs annotate, verbatim


# General Edits

@docs replace, delete, insertAfter, insertFirst


# Reading

@docs copy, copyText

-}

import Mark.Error
import Mark.Internal.Description as Desc exposing (..)
import Mark.Internal.Error as Error
import Mark.Internal.Format as Format
import Mark.Internal.Id as Id exposing (..)
import Mark.Internal.Index as Index
import Mark.Internal.Outcome as Outcome
import Mark.Internal.Parser as Parse
import Mark.New
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


{-| Every block has an `Id`. You can retrieve the `Id` for a block using [`Mark.withId`](Mark#withId)
-}
type alias Id =
    Id.Id


{-| -}
type alias Position =
    { offset : Int
    , line : Int
    , column : Int
    }


{-| -}
type alias Range =
    { start : Position
    , end : Position
    }


{-| -}
type Edit
    = Replace Id Mark.New.Block
      -- Create an element in a ManyOf
      -- Indexes overflow, so if it's too large, it just puts it at the end.
      -- Indexes that are below 0 and clamped to 0
    | InsertAt Id Int Mark.New.Block
    | InsertAfter Id (List Mark.New.Block)
    | Delete (List Id)
      -- Text Editing
    | StyleText Id Offset Offset Restyle
    | Annotate Id Offset Offset Annotation
    | ReplaceString Id Offset Offset String
    | ReplaceSelection Id Offset Offset (List Mark.New.Text)


type Annotation
    = Annotation String (List ( String, Mark.New.Block ))
    | Verbatim String (List ( String, Mark.New.Block ))


{-| -}
deleteText : Id -> Offset -> Offset -> Edit
deleteText id start end =
    ReplaceSelection id
        start
        end
        []


{-| This will work on both styled text and string datatypes.
-}
insertString : Id -> Offset -> String -> Edit
insertString id at str =
    ReplaceString id at at str


{-| -}
insertText : Id -> Offset -> List Mark.New.Text -> Edit
insertText id at els =
    ReplaceSelection id at at els


{-| The `Block` mentioned here is actually a `Mark.New.Block`. Use [`Mark.New`](/Mark-New) to create the new block you'd like.
-}
replace : Id -> Mark.New.Block -> Edit
replace =
    Replace


{-| Delete blocks.

This may fail if the block cannot be deleted(such as a field in a record).

-}
delete : List Id -> Edit
delete =
    Delete



{-
   I don't think we need insertAt anymore.
   The only situation is possibly needing it for inserting something in the very first position.
   Though when does that come up?
   I think

   {-| Insert a block at an index within a `Mark.manyOf`.
   -}
   insertAt : Id -> Int -> Mark.New.Block -> Edit
   insertAt =
       InsertAt

-}


{-| Insert a block at the first position.

The id needed here is of the _parent_.

-}
insertFirst : Id -> Mark.New.Block -> Edit
insertFirst id new =
    InsertAt id 0 new


{-| Insert a block at an index within a `Mark.manyOf`.
-}
insertAfter : Id -> List Mark.New.Block -> Edit
insertAfter =
    InsertAfter


{-| Put the current text selection within an `annotation` with some attributes.

Here's an example that would turn the selection into a `link`.

    Mark.Edit.annotate id
        start
        end
        "link"
        [ ( "url", Mark.New.string "https://guide.elm-lang.org/" ) ]

**Note** existing annotations within the selection are removed.

-}
annotate : Id -> Offset -> Offset -> String -> List ( String, Mark.New.Block ) -> Edit
annotate id start end name attrs =
    Annotate id start end (Annotation name attrs)


{-| Same as `annotate`, but creates a `verbatim` instead.

**Note** existing annotations within the selection are removed.

-}
verbatim : Id -> Offset -> Offset -> String -> List ( String, Mark.New.Block ) -> Edit
verbatim id start end name attrs =
    Annotate id start end (Verbatim name attrs)


{-| Remove all styling on the text selection and replace with the given styling.
-}
restyle : Id -> Offset -> Offset -> Styles -> Edit
restyle id start end styles =
    StyleText id start end (Restyle styles)


{-| Remove the given styles if they are present in the text selection.

**Note:** Other styling is unaffected.

-}
removeStyles : Id -> Offset -> Offset -> Styles -> Edit
removeStyles id start end styles =
    StyleText id start end (RemoveStyle styles)


{-| Add the given styles if they are not present in the text selection.

**Note:** Other styling is unaffected.

-}
addStyles : Id -> Offset -> Offset -> Styles -> Edit
addStyles id start end styles =
    StyleText id start end (AddStyle styles)


editAtId id fn desc =
    if Desc.getId desc == id then
        fn desc

    else
        NoIdFound


{-| Set the content of the current block
-}
replaceOption id original new desc =
    let
        created : { desc : Description, seed : Seed }
        created =
            create original.currentSeed new
    in
    EditMade
        (Just created.seed)
        created.desc


sizeToPush size =
    if size.offset == 0 && size.line == 0 then
        Nothing

    else
        Just size


deleteBlocks : List Id -> Description -> EditOutcome Description
deleteBlocks ids desc =
    case desc of
        Group many ->
            let
                newChildren =
                    List.filter
                        (\child ->
                            not (List.member (Desc.getId child) ids)
                        )
                        many.children
            in
            if List.length newChildren == List.length many.children then
                NoIdFound

            else
                EditMade
                    Nothing
                    (Group
                        { many
                            | children = newChildren
                        }
                    )

        StartsWith details ->
            NoIdFound

        _ ->
            NoIdFound


{-| -}
update : Document meta data -> Edit -> Parsed -> Result (List Mark.Error.Error) Parsed
update doc edit (Parsed original) =
    let
        editFn =
            case edit of
                Replace id new ->
                    editAtId id <|
                        \desc ->
                            -- if Desc.match desc new then
                            replaceOption id original new desc

                InsertAfter id new ->
                    editAtId id <|
                        \desc ->
                            let
                                created =
                                    createMany original.currentSeed new
                            in
                            AddNext (Just created.seed) created.new

                InsertAt id index new ->
                    editAtId id <|
                        \desc ->
                            case desc of
                                Group many ->
                                    let
                                        inserted : { updated : List Description, seed : Seed }
                                        inserted =
                                            makeInsertAt
                                                original.currentSeed
                                                index
                                                many
                                                new
                                    in
                                    EditMade
                                        (Just inserted.seed)
                                        (Group
                                            { many
                                                | children =
                                                    inserted.updated
                                            }
                                        )

                                _ ->
                                    -- inserts= by index only works for `manyOf`
                                    ErrorMakingEdit Error.InvalidInsert

                Delete ids ->
                    deleteBlocks ids

                StyleText id start end restyleAction ->
                    editAtId id
                        (\desc ->
                            case desc of
                                DescribeText details ->
                                    let
                                        realStart =
                                            min start end

                                        realEnd =
                                            max start end

                                        newTexts =
                                            doTextEdit realStart
                                                realEnd
                                                (List.map (applyStyles restyleAction))
                                                details.text
                                                emptySelectionEdit
                                                |> List.foldl mergeStyles []
                                    in
                                    EditMade
                                        Nothing
                                        (DescribeText
                                            { details | text = newTexts }
                                        )

                                _ ->
                                    ErrorMakingEdit Error.InvalidTextEdit
                        )

                Annotate id start end wrapper ->
                    editAtId id
                        (\desc ->
                            case desc of
                                DescribeText details ->
                                    let
                                        realStart =
                                            min start end

                                        realEnd =
                                            max start end

                                        newTexts =
                                            doTextEdit realStart
                                                realEnd
                                                (\els ->
                                                    let
                                                        wrapped : NewInline
                                                        wrapped =
                                                            case wrapper of
                                                                Annotation name attrs ->
                                                                    NewInlineBlock
                                                                        { name = name
                                                                        , kind =
                                                                            SelectText
                                                                                (List.concatMap onlyText els)
                                                                        , fields = attrs
                                                                        }

                                                                Verbatim name attrs ->
                                                                    NewInlineBlock
                                                                        { name = name
                                                                        , kind =
                                                                            SelectString
                                                                                (List.concatMap onlyText els
                                                                                    |> List.map textString
                                                                                    |> String.join ""
                                                                                )
                                                                        , fields = attrs
                                                                        }

                                                        newText : List TextDescription
                                                        newText =
                                                            createInline
                                                                [ wrapped ]
                                                    in
                                                    newText
                                                )
                                                details.text
                                                emptySelectionEdit
                                                |> List.foldl mergeStyles []
                                    in
                                    EditMade
                                        Nothing
                                        (DescribeText { details | text = newTexts })

                                _ ->
                                    ErrorMakingEdit Error.InvalidTextEdit
                        )

                ReplaceSelection id start end newTextEls ->
                    editAtId id
                        (\desc ->
                            let
                                realStart =
                                    min start end

                                realEnd =
                                    max start end
                            in
                            case desc of
                                DescribeText details ->
                                    let
                                        makeNewText selectedEls =
                                            createInline newTextEls

                                        newTexts =
                                            doTextEdit
                                                realStart
                                                realEnd
                                                makeNewText
                                                details.text
                                                emptySelectionEdit
                                                |> List.foldl mergeStyles []
                                    in
                                    EditMade
                                        Nothing
                                        (DescribeText { details | text = newTexts })

                                DescribeString strId str ->
                                    case newTextEls of
                                        [] ->
                                            -- deletes work on strings too
                                            let
                                                new =
                                                    String.left realStart str ++ String.dropLeft realEnd str
                                            in
                                            EditMade
                                                Nothing
                                                (DescribeString strId new)

                                        _ ->
                                            ErrorMakingEdit Error.InvalidTextEdit

                                _ ->
                                    ErrorMakingEdit Error.InvalidTextEdit
                        )

                ReplaceString id start end newString ->
                    editAtId id
                        (\desc ->
                            let
                                realStart =
                                    min start end

                                realEnd =
                                    max start end
                            in
                            case desc of
                                DescribeText details ->
                                    let
                                        makeNewText selectedEls =
                                            createInline [ NewText (Text emptyStyles newString) ]

                                        newTexts =
                                            doTextEdit
                                                realStart
                                                realEnd
                                                makeNewText
                                                details.text
                                                emptySelectionEdit
                                                |> List.foldl mergeStyles []
                                    in
                                    EditMade
                                        Nothing
                                        (DescribeText { details | text = newTexts })

                                DescribeString strId str ->
                                    let
                                        new =
                                            String.left realStart str
                                                ++ newString
                                                ++ String.dropLeft realEnd str
                                    in
                                    EditMade
                                        Nothing
                                        (DescribeString strId new)

                                _ ->
                                    ErrorMakingEdit Error.InvalidTextEdit
                        )
    in
    original.found
        |> makeEdit
            { makeEdit = editFn
            , indentation = 0
            }
        |> prepareResults doc original


prepareResults :
    Document meta data
    -> ParsedDetails
    -> EditOutcome Description
    -> Result (List Error.Rendered) Parsed
prepareResults doc original edited =
    case edited of
        NoIdFound ->
            Err [ Error.idNotFound ]

        ErrorMakingEdit err ->
            Err [ Error.renderEditError err ]

        AddNext maybeSeed new ->
            Err [ Error.renderEditError Error.InvalidInsert ]

        EditMade maybeSeed newDescription ->
            let
                newParsed : Parsed
                newParsed =
                    Parsed
                        { original
                            | found = newDescription
                            , currentSeed =
                                case maybeSeed of
                                    Nothing ->
                                        original.currentSeed

                                    Just seed ->
                                        seed
                        }
            in
            case Desc.render doc newParsed of
                Outcome.Success _ ->
                    Ok newParsed

                Outcome.Almost details ->
                    Err details.errors

                Outcome.Failure errs ->
                    Err errs


textString : Text -> String
textString (Text _ str) =
    str


type alias EditCursor =
    -- take the current item and return an edit
    -- pass in previous id if there is one.
    { makeEdit : Description -> EditOutcome Description
    , indentation : Int
    }


type alias Push =
    Maybe Size


type alias Size =
    { offset : Int
    , line : Int
    }


type EditOutcome desc
    = ErrorMakingEdit Error.EditErr
    | EditMade (Maybe Id.Seed) desc
      -- AddNext only needs to be handled in editMany
      -- if it shows up in other places, it's an error
    | AddNext (Maybe Id.Seed) (List desc)
    | NoIdFound


{-| -}
makeEdit : EditCursor -> Description -> EditOutcome Description
makeEdit cursor desc =
    case desc of
        DescribeBlock details ->
            case cursor.makeEdit desc of
                NoIdFound ->
                    -- dive further
                    case makeEdit (increaseIndent cursor) details.found of
                        EditMade maybeSeed newFound ->
                            EditMade maybeSeed
                                (DescribeBlock
                                    { details
                                        | found = newFound
                                    }
                                )

                        AddNext maybeSeed new ->
                            ErrorMakingEdit Error.InvalidInsert

                        NoIdFound ->
                            NoIdFound

                        ErrorMakingEdit err ->
                            ErrorMakingEdit err

                otherwise ->
                    otherwise

        Record details ->
            case cursor.makeEdit desc of
                NoIdFound ->
                    case editFields cursor details.found of
                        EditMade maybeSeed updatedFields ->
                            EditMade maybeSeed
                                (Record
                                    { details
                                        | found =
                                            updatedFields
                                    }
                                )

                        NoIdFound ->
                            NoIdFound

                        AddNext maybeSeed new ->
                            ErrorMakingEdit Error.InvalidInsert

                        ErrorMakingEdit err ->
                            ErrorMakingEdit err

                otherwise ->
                    otherwise

        Group many ->
            case cursor.makeEdit desc of
                NoIdFound ->
                    -- dive further
                    case editMany makeEdit cursor many.children of
                        EditMade maybeSeed updatedChildren ->
                            EditMade maybeSeed
                                (Group
                                    { many
                                        | children =
                                            updatedChildren
                                    }
                                )

                        NoIdFound ->
                            NoIdFound

                        AddNext maybeSeed new ->
                            ErrorMakingEdit Error.InvalidInsert

                        ErrorMakingEdit err ->
                            ErrorMakingEdit err

                otherwise ->
                    otherwise

        StartsWith details ->
            case makeEdit cursor details.first of
                NoIdFound ->
                    case makeEdit cursor details.second of
                        EditMade maybeSeed secondUpdated ->
                            EditMade maybeSeed
                                (StartsWith
                                    { id = details.id
                                    , first = details.first
                                    , second =
                                        secondUpdated
                                    }
                                )

                        otherwise ->
                            otherwise

                EditMade maybeSeed firstUpdated ->
                    EditMade maybeSeed
                        (StartsWith
                            { id = details.id
                            , second =
                                details.second
                            , first =
                                firstUpdated
                            }
                        )

                otherwise ->
                    otherwise

        DescribeItem details ->
            case cursor.makeEdit desc of
                NoIdFound ->
                    case editMany makeEdit cursor details.content of
                        EditMade maybeSeed newContent ->
                            EditMade maybeSeed
                                (DescribeItem
                                    { details
                                        | content = newContent
                                    }
                                )

                        AddNext maybeSeed new ->
                            ErrorMakingEdit Error.InvalidInsert

                        NoIdFound ->
                            case editMany makeEdit cursor details.children of
                                EditMade maybeSeed newChildren ->
                                    EditMade maybeSeed
                                        (DescribeItem
                                            { details
                                                | children = newChildren
                                            }
                                        )

                                AddNext maybeSeed new ->
                                    ErrorMakingEdit Error.InvalidInsert

                                NoIdFound ->
                                    NoIdFound

                                ErrorMakingEdit err ->
                                    ErrorMakingEdit err

                        ErrorMakingEdit err ->
                            ErrorMakingEdit err

                otherwise ->
                    otherwise

        -- Primitives
        DescribeBoolean details ->
            cursor.makeEdit desc

        DescribeInteger found ->
            cursor.makeEdit desc

        DescribeFloat found ->
            cursor.makeEdit desc

        DescribeText txt ->
            cursor.makeEdit desc

        DescribeString id str ->
            cursor.makeEdit desc

        DescribeUnexpected id details ->
            cursor.makeEdit desc


editFields : EditCursor -> List ( String, Description ) -> EditOutcome (List ( String, Description ))
editFields cursor fields =
    let
        makeFieldEdit (( fieldName, foundField ) as field) ( editMade, pastFields ) =
            case editMade of
                EditMade maybeSeed ls ->
                    ( EditMade maybeSeed ls
                    , ( fieldName
                      , foundField
                      )
                        :: pastFields
                    )

                AddNext maybeSeed new ->
                    ( ErrorMakingEdit Error.InvalidInsert
                    , field :: pastFields
                    )

                NoIdFound ->
                    case makeEdit cursor foundField of
                        NoIdFound ->
                            ( NoIdFound
                            , field :: pastFields
                            )

                        AddNext maybeSeed new ->
                            ( ErrorMakingEdit Error.InvalidInsert
                            , field :: pastFields
                            )

                        ErrorMakingEdit err ->
                            ( ErrorMakingEdit err
                            , field :: pastFields
                            )

                        EditMade maybeSeed newField ->
                            ( EditMade maybeSeed []
                            , ( fieldName, newField ) :: pastFields
                            )

                ErrorMakingEdit err ->
                    ( ErrorMakingEdit err
                    , field :: pastFields
                    )
    in
    fields
        |> List.foldl makeFieldEdit ( NoIdFound, [] )
        |> (\( editMade, updatedList ) ->
                case editMade of
                    EditMade maybeSeed _ ->
                        EditMade maybeSeed (List.reverse updatedList)

                    otherwise ->
                        otherwise
           )


editMany :
    (EditCursor -> Description -> EditOutcome Description)
    -> EditCursor
    -> List Description
    -> EditOutcome (List Description)
editMany fn cursor manyItems =
    manyItems
        |> List.foldl
            (\node ( editMade, pastChildren ) ->
                case editMade of
                    EditMade maybeSeed _ ->
                        ( editMade
                        , node :: pastChildren
                        )

                    AddNext maybeSeed new ->
                        -- This branch should never take place because the below case statement
                        -- with the fn is the one that actually handles it.
                        ( EditMade maybeSeed []
                        , node :: pastChildren
                        )

                    ErrorMakingEdit err ->
                        ( ErrorMakingEdit err
                        , node :: pastChildren
                        )

                    NoIdFound ->
                        case fn (increaseIndent (increaseIndent cursor)) node of
                            EditMade maybeSeed newChild ->
                                ( EditMade maybeSeed []
                                , newChild :: pastChildren
                                )

                            AddNext maybeSeed new ->
                                ( EditMade maybeSeed []
                                , new ++ node :: pastChildren
                                )

                            NoIdFound ->
                                ( NoIdFound
                                , node :: pastChildren
                                )

                            ErrorMakingEdit err ->
                                ( ErrorMakingEdit err
                                , node :: pastChildren
                                )
            )
            ( NoIdFound, [] )
        |> (\( editMade, updatedList ) ->
                case editMade of
                    EditMade maybeSeed _ ->
                        EditMade maybeSeed (List.reverse updatedList)

                    otherwise ->
                        otherwise
           )


increaseIndent x =
    { x | indentation = x.indentation + 1 }


addSizes one two =
    { line = one.line + two.line
    , offset = one.offset + two.offset
    }


invertSize size =
    { line = -1 * size.line
    , offset = -1 * size.offset
    }


{-| -}
expandRange : Push -> Range -> Range
expandRange maybePush range =
    case maybePush of
        Nothing ->
            range

        Just to ->
            { range
                | end =
                    { offset = range.end.offset + to.offset
                    , line = range.end.line + to.line
                    , column = range.end.column
                    }
            }


pushRange to range =
    { start = pushPosition to range.start
    , end = pushPosition to range.end
    }


pushPosition to pos =
    { offset = pos.offset + to.offset
    , line = pos.line + to.line
    , column = pos.column
    }


addNewline pos =
    { offset = pos.offset + 1
    , line = pos.line + 1
    }


makeInsertAt :
    Id.Seed
    -> Int
    ->
        { children : List Description
        , id : Id
        }
    -> New
    ->
        { updated : List Description
        , seed : Id.Seed
        }
makeInsertAt seed index many new =
    many.children
        |> List.foldl (insertHelper seed index new)
            { index = 0
            , seed = seed
            , inserted = False
            , list = []
            }
        |> (\found ->
                if found.inserted then
                    { updated = List.reverse found.list
                    , seed = found.seed
                    }

                else
                    let
                        created : { desc : Description, seed : Seed }
                        created =
                            create seed new
                    in
                    { updated =
                        List.reverse
                            (created.desc
                                :: found.list
                            )
                    , seed = created.seed
                    }
           )


insertHelper seed index new item found =
    if found.index == index then
        let
            created : { desc : Description, seed : Seed }
            created =
                create seed new
        in
        { index = found.index + 1
        , seed = created.seed
        , inserted = True
        , list =
            item
                :: created.desc
                :: found.list
        }

    else
        { index = found.index + 1
        , seed = found.seed
        , inserted = found.inserted
        , list = item :: found.list
        }



{- EDITING TEXT -}


{-| -}
type alias Replacement =
    Parse.Replacement


{-| -}
type alias Styles =
    { bold : Bool
    , italic : Bool
    , strike : Bool
    }


{-| -}
type alias Selection =
    { anchor : Offset
    , focus : Offset
    }


{-| The index of the rendered `String`. Let's say you have this string in your markup source.

```markup
Here is *my string*.
```

When you're rendering this, it's broken into three segments.

  - `Here is`
  - `my string` (which is bold)
  - `.`

The `Offset` is character position where those three strings are considered as one big one.

Here are some lookups:

  - `0  -> H`
  - `8  -> m`
  - `17 -> .`

We're not counting control characters from our markup source.

-}
type alias Offset =
    Int



{- TEXT EDITING -}


type Restyle
    = Restyle Styles
    | AddStyle Styles
    | RemoveStyle Styles



{- TEXT EDITING HELP -}


onlyText : TextDescription -> List Text
onlyText txt =
    case txt of
        InlineBlock details ->
            case details.kind of
                EmptyAnnotation ->
                    []

                SelectText ts ->
                    ts

                SelectString str ->
                    [ Text emptyStyles str ]

        Styled t ->
            [ t ]


{-| Folds over a list of styles and merges them if they're compatible
-}
mergeStyles : TextDescription -> List TextDescription -> List TextDescription
mergeStyles inlineEl gathered =
    case gathered of
        [] ->
            [ inlineEl ]

        prev :: tail ->
            case attemptMerge inlineEl prev of
                Nothing ->
                    inlineEl :: prev :: tail

                Just merged ->
                    merged :: tail


attemptMerge : TextDescription -> TextDescription -> Maybe TextDescription
attemptMerge first second =
    case ( first, second ) of
        ( Styled (Text stylingOne strOne), Styled (Text stylingTwo strTwo) ) ->
            if stylingOne == stylingTwo then
                Just (Styled (Text stylingOne (strOne ++ strTwo)))

            else
                Nothing

        ( InlineBlock one, InlineBlock two ) ->
            let
                matchingAttributes attr1 attr2 =
                    List.map Tuple.first attr1
                        == List.map Tuple.first attr2

                mergeMatchingRecords r1 r2 newKind =
                    case ( r1, r2 ) of
                        ( Record rec1, Record rec2 ) ->
                            if
                                rec1.name
                                    == rec2.name
                                    && matchingAttributes rec1.found rec2.found
                            then
                                Just
                                    (InlineBlock
                                        { kind = newKind
                                        , record = one.record
                                        }
                                    )

                            else
                                Nothing

                        _ ->
                            Nothing
            in
            -- Same == same type, same attribute list, same attribute values
            case ( one.kind, two.kind ) of
                ( EmptyAnnotation, EmptyAnnotation ) ->
                    mergeMatchingRecords one.record two.record EmptyAnnotation

                ( SelectText txt1, SelectText txt2 ) ->
                    mergeMatchingRecords one.record two.record (SelectText (txt1 ++ txt2))

                ( SelectString str1, SelectString str2 ) ->
                    mergeMatchingRecords one.record two.record (SelectString (str1 ++ str2))

                _ ->
                    Nothing

        _ ->
            Nothing


mergeRanges one two =
    { start = one.start
    , end = two.end
    }


emptySelectionEdit =
    { offset = 0
    , elements = []
    , selection = Nothing
    }


{-| -}
doTextEdit :
    Offset
    -> Offset
    -> (List TextDescription -> List TextDescription)
    -> List TextDescription
    ->
        { elements : List TextDescription
        , offset : Int
        , selection : Maybe (List TextDescription)
        }
    -> List TextDescription
doTextEdit anchor focus editFn textDescs cursor =
    case textDescs of
        [] ->
            case cursor.selection of
                Nothing ->
                    cursor.elements

                Just selection ->
                    editFn selection ++ cursor.elements

        current :: remain ->
            let
                start =
                    min anchor focus

                end =
                    max anchor focus

                len : Int
                len =
                    length current
            in
            case cursor.selection of
                Nothing ->
                    if cursor.offset <= start && cursor.offset + len >= start then
                        {- Start Selection -}
                        if cursor.offset + len >= end then
                            {- We finish the selection in this element -}
                            let
                                ( before, afterLarge ) =
                                    splitAt (start - cursor.offset) current

                                ( selected, after ) =
                                    splitAt (end - start) afterLarge
                            in
                            doTextEdit anchor
                                focus
                                editFn
                                remain
                                { offset = cursor.offset + len
                                , elements =
                                    after :: editFn [ selected ] ++ (before :: cursor.elements)
                                , selection =
                                    Nothing
                                }

                        else
                            let
                                ( before, after ) =
                                    splitAt (start - cursor.offset) current
                            in
                            doTextEdit anchor
                                focus
                                editFn
                                remain
                                { offset = cursor.offset + len
                                , elements =
                                    before :: cursor.elements
                                , selection =
                                    Just [ after ]
                                }

                    else
                        doTextEdit anchor
                            focus
                            editFn
                            remain
                            { offset = cursor.offset + len
                            , elements = current :: cursor.elements
                            , selection = cursor.selection
                            }

                Just selection ->
                    if cursor.offset + len >= end then
                        let
                            ( before, after ) =
                                splitAt (end - cursor.offset) current

                            fullSelection =
                                before :: selection
                        in
                        doTextEdit anchor
                            focus
                            editFn
                            remain
                            { offset = cursor.offset + len
                            , elements =
                                if cursor.offset + len == end then
                                    editFn fullSelection ++ cursor.elements

                                else
                                    after :: editFn fullSelection ++ cursor.elements
                            , selection = Nothing
                            }

                    else
                        doTextEdit anchor
                            focus
                            editFn
                            remain
                            { offset = cursor.offset + len
                            , elements = cursor.elements
                            , selection = Just (current :: selection)
                            }


applyStyles : Restyle -> TextDescription -> TextDescription
applyStyles styling inlineEl =
    case inlineEl of
        Styled txt ->
            Styled (applyStylesToText styling txt)

        InlineBlock details ->
            case details.kind of
                SelectText txts ->
                    InlineBlock
                        { details
                            | kind = SelectText (List.map (applyStylesToText styling) txts)
                        }

                x ->
                    inlineEl


applyStylesToText styling (Text styles str) =
    case styling of
        Restyle newStyle ->
            Text newStyle str

        RemoveStyle toRemove ->
            Text
                { bold = styles.bold && not toRemove.bold
                , italic = styles.italic && not toRemove.italic
                , strike = styles.strike && not toRemove.strike
                }
                str

        AddStyle toAdd ->
            Text
                { bold = styles.bold || toAdd.bold
                , italic = styles.italic || toAdd.italic
                , strike = styles.strike || toAdd.strike
                }
                str


{-| Return the text descriptions between the two offsets, splittin if necessary.
-}
slice : Offset -> Offset -> List TextDescription -> List TextDescription
slice start end texts =
    sliceHelper 0 start end texts []
        |> List.foldl mergeStyles []


within : Offset -> Offset -> Int -> Int -> Bool
within start end current len =
    current >= start && current + len <= end


isSplitting : Offset -> Int -> Int -> Bool
isSplitting offset current len =
    current <= offset && current + len >= offset


sliceHelper : Int -> Offset -> Offset -> List TextDescription -> List TextDescription -> List TextDescription
sliceHelper index start end toSplit captured =
    case toSplit of
        [] ->
            captured

        top :: remaining ->
            let
                len =
                    length top
            in
            if within start end index len then
                -- add to captured
                sliceHelper (index + len) start end remaining (top :: captured)

            else
                let
                    containsStart =
                        isSplitting start index len

                    containsEnd =
                        isSplitting end index len
                in
                -- we add 1 to the end because we want the slice to be inclusive.
                -- e.g. if we want to copy the letter 'a', which starts at the 4 offset,
                -- then the offsets 4 4 seem reasonable.
                -- 4 5 seems like it should copy two characters
                -- this is basically the difference between offsets referring
                -- to literal charcters vs the space between characters.
                -- space between characters seems slightly harder to think about.
                if containsStart && containsEnd then
                    -- split and add right
                    let
                        ( _, right ) =
                            splitAt start top

                        ( focus, _ ) =
                            splitAt (end + 1 - start) right
                    in
                    sliceHelper (index + len) start end remaining (focus :: captured)

                else if containsStart then
                    -- split and add right
                    let
                        ( _, right ) =
                            splitAt start top
                    in
                    sliceHelper (index + len) start end remaining (right :: captured)

                else if containsEnd then
                    -- split and add left
                    let
                        ( left, _ ) =
                            splitAt (end + 1) top
                    in
                    sliceHelper (index + len) start end remaining (left :: captured)

                else
                    case captured of
                        [] ->
                            sliceHelper (index + len) start end remaining captured

                        _ ->
                            captured


{-| Splits the current element based on an index.

This function should only be called when the offset is definitely contained within the element provided, not on the edges.

_Reminder_ Indexes are based on the size of the rendered text.

-}
splitAt : Offset -> TextDescription -> ( TextDescription, TextDescription )
splitAt offset inlineEl =
    case inlineEl of
        Styled txt ->
            let
                ( leftText, rightText ) =
                    splitText offset txt
            in
            ( Styled leftText
            , Styled rightText
            )

        InlineBlock details ->
            case details.kind of
                EmptyAnnotation ->
                    -- This shoudn't happen because we're expecting the offset
                    -- to be within the range, and a token has a length of 0
                    ( Styled (Text emptyStyles "")
                    , InlineBlock details
                    )

                SelectString str ->
                    let
                        leftString : String
                        leftString =
                            String.slice 0 offset str

                        rightString : String
                        rightString =
                            String.slice offset -1 str
                    in
                    ( InlineBlock
                        { details
                            | kind = SelectString leftString
                        }
                    , InlineBlock
                        { details
                            | kind = SelectString rightString
                        }
                    )

                SelectText txts ->
                    let
                        { left, right } =
                            List.foldl (splitTextElements offset)
                                { offset = 0
                                , left = []
                                , right = []
                                }
                                txts

                        splitTextElements off (Text styling txt) cursor =
                            if off >= cursor.offset && off <= cursor.offset + String.length txt then
                                { offset = cursor.offset + String.length txt
                                , left = Text styling (String.left (offset - cursor.offset) txt) :: cursor.left
                                , right = Text styling (String.dropLeft (offset - cursor.offset) txt) :: cursor.right
                                }

                            else if off < cursor.offset then
                                { offset = cursor.offset + String.length txt
                                , left = cursor.left
                                , right = Text styling txt :: cursor.right
                                }

                            else
                                { offset = cursor.offset + String.length txt
                                , left = Text styling txt :: cursor.left
                                , right = cursor.right
                                }
                    in
                    ( InlineBlock
                        { details
                            | kind = SelectText (List.reverse left)
                        }
                    , InlineBlock
                        { details
                            | kind = SelectText (List.reverse right)
                        }
                    )


splitText offset (Text styling str) =
    ( Text styling (String.left offset str)
    , Text styling (String.dropLeft offset str)
    )


splitRange offset range =
    let
        -- TODO: This stays on the same line
        middle =
            { offset = range.start.offset + offset
            , line = range.start.line
            , column = range.start.column + offset
            }
    in
    ( range
    , range
    )


emptyRange =
    { start =
        { offset = 0
        , line = 1
        , column = 1
        }
    , end =
        { offset = 0
        , line = 1
        , column = 1
        }
    }



{- READING -}


{-| -}
copy : List Id -> Parsed -> List Mark.New.Block
copy ids (Desc.Parsed parsed) =
    let
        blocks =
            Desc.findMany ids parsed.found
    in
    List.map Desc.toNew blocks


{-| -}
copyText : Id -> Offset -> Offset -> Parsed -> List Mark.New.Text
copyText id anchor focus (Desc.Parsed parsed) =
    let
        start =
            min anchor focus

        end =
            max anchor focus
    in
    case Desc.findMany [ id ] parsed.found of
        [] ->
            []

        (Desc.DescribeText details) :: [] ->
            slice start end details.text
                |> List.map Desc.toNewText

        _ ->
            []
