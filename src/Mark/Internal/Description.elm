module Mark.Internal.Description exposing
    ( Found(..), Nested(..), UnexpectedDetails
    , Description(..), TextDescription(..), InlineAttribute(..), Text(..), Style(..)
    , Expectation(..), InlineExpectation(..), AttrExpectation(..)
    , Parsed(..), startingPoint, descriptionToString, toString
    )

{-|

@docs Found, Nested, UnexpectedDetails

@docs Description, TextDescription, InlineAttribute, Text, Style

@docs Expectation, InlineExpectation, AttrExpectation

@docs Parsed, startingPoint, descriptionToString, toString

-}

import Iso8601
import Mark.Format as Format
import Mark.Internal.Error as Error
import Mark.Internal.Id exposing (..)
import Random
import Time


type alias Error =
    { message : List Format.Text
    , region : { start : Position, end : Position }
    , title : String
    }


{-| -}
type Parsed
    = Parsed
        { errors : List Error
        , found : Found Description
        , expected : Expectation
        , focus : Maybe Position
        , initialSeed : Random.Seed
        , currentSeed : Random.Seed
        }


{-| -}
type Found item
    = Found Range item
    | Unexpected
        { range : Range
        , problem : Error.Error
        }


{-| -}
type alias UnexpectedDetails =
    { range : Range
    , problem : Error.Error
    }


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
type Nested item
    = Nested
        { content : item
        , children :
            List (Nested item)
        }


{-| -}
type Description
    = DescribeBlock
        { name : String
        , found : Found Description
        , expected : Expectation
        }
    | Record
        { name : String
        , found : Found (List ( String, Found Description ))
        , expected : Expectation
        }
    | OneOf
        { id : Id Options
        , choices : List (Choice (Id Options) Expectation)
        , child : Found Description
        }
    | ManyOf
        { id : Id ManyOptions
        , choices : List (Choice (Id ManyOptions) Expectation)
        , children : List (Found Description)
        }
    | StartsWith
        Range
        { found : Description
        , expected : Expectation
        }
        { found : Description
        , expected : Expectation
        }
    | DescribeTree
        { found : ( Range, List (Nested ( Description, List Description )) )
        , expected : Expectation
        }
      -- Primitives
    | DescribeStub String (Found String)
    | DescribeBoolean
        { id : Id Bool
        , found : Found Bool
        }
    | DescribeInteger
        { id : Id Int
        , found : Found Int
        }
    | DescribeIntBetween
        { max : Int
        , min : Int
        , found : Found Int
        , id : Id Int
        }
    | DescribeFloat
        { id : Id Float
        , found : Found ( String, Float )
        }
    | DescribeFloatBetween
        { max : Float
        , min : Float
        , found : Found ( String, Float )
        , id : Id Float
        }
    | DescribeText
        { id : Id Text
        , text : List TextDescription
        }
    | DescribeString (Id String) String
    | DescribeMultiline (Id String) String
    | DescribeStringExactly Range String
    | DescribeDate
        { id : Id Time.Posix
        , found : Found ( String, Time.Posix )
        }


{-| -}
type Proved
    = Proved (Id ManyOptions) (List (Found Description))


{-| -}
type TextDescription
    = Styled Range Text
    | InlineAnnotation
        { range : Range
        , text : List Text
        , attributes : List InlineAttribute
        }
    | InlineToken
        { name : String
        , range : Range
        , attributes : List InlineAttribute
        }
    | UnexpectedInline UnexpectedDetails


{-| -}
type InlineAttribute
    = AttrString
        { name : String
        , range : Range
        , value : String
        }



-- | DescribeInlineText Range (List Text)


{-| -}
type Style
    = Bold
    | Italic
    | Strike


{-| A text fragment with some styling.
-}
type Text
    = Text (List Style) String


{-| -}
type Expectation
    = ExpectBlock String Expectation
    | ExpectStub String
    | ExpectRecord String (List ( String, Expectation ))
    | ExpectOneOf (List Expectation)
    | ExpectManyOf (List Expectation)
    | ExpectStartsWith Expectation Expectation
    | ExpectBoolean Bool
    | ExpectInteger Int
    | ExpectFloat Float
    | ExpectFloatBetween
        { min : Float
        , max : Float
        , default : Float
        }
    | ExpectIntBetween
        { min : Int
        , max : Int
        , default : Int
        }
    | ExpectText (List InlineExpectation)
    | ExpectString String
    | ExpectMultiline String
    | ExpectStringExactly String
    | ExpectDate Time.Posix
    | ExpectTree Expectation Expectation


{-| -}
type InlineExpectation
    = ExpectAnnotation String (List AttrExpectation)
    | ExpectToken String (List AttrExpectation)


{-| -}
type AttrExpectation
    = ExpectAttrString String



-- | ExpectAttr
-- | ExpectInlineText


choiceExpectation (Choice id exp) =
    exp


match description exp =
    case description of
        DescribeBlock details ->
            case exp of
                ExpectBlock expectedName expectedChild ->
                    if expectedName == details.name then
                        matchExpected details.expected expectedChild

                    else
                        False

                _ ->
                    False

        DescribeStub name found ->
            case exp of
                ExpectStub expectedName ->
                    name == expectedName

                _ ->
                    False

        Record details ->
            matchExpected details.expected exp

        OneOf one ->
            matchExpected (ExpectOneOf (List.map choiceExpectation one.choices)) exp

        ManyOf many ->
            matchExpected (ExpectManyOf (List.map choiceExpectation many.choices)) exp

        StartsWith range start end ->
            case exp of
                ExpectStartsWith startExp endExp ->
                    match start.found startExp && match end.found endExp

                _ ->
                    False

        DescribeTree tree ->
            matchExpected tree.expected exp

        DescribeBoolean foundBoolean ->
            case exp of
                ExpectBoolean _ ->
                    True

                _ ->
                    False

        DescribeInteger _ ->
            case exp of
                ExpectInteger _ ->
                    True

                _ ->
                    False

        DescribeFloat _ ->
            case exp of
                ExpectFloat _ ->
                    True

                _ ->
                    False

        DescribeFloatBetween _ ->
            case exp of
                ExpectFloatBetween _ ->
                    True

                _ ->
                    False

        DescribeIntBetween _ ->
            case exp of
                ExpectIntBetween _ ->
                    True

                _ ->
                    False

        DescribeText _ ->
            case exp of
                ExpectText _ ->
                    True

                _ ->
                    False

        DescribeString _ _ ->
            case exp of
                ExpectString _ ->
                    True

                _ ->
                    False

        DescribeMultiline _ _ ->
            case exp of
                ExpectMultiline _ ->
                    True

                _ ->
                    False

        DescribeStringExactly _ _ ->
            case exp of
                ExpectStringExactly _ ->
                    True

                _ ->
                    False

        DescribeDate foundPosix ->
            case exp of
                ExpectDate _ ->
                    True

                _ ->
                    False


{-| Is the first expectation a subset of the second?
-}
matchExpected : Expectation -> Expectation -> Bool
matchExpected subExp expected =
    case ( subExp, expected ) of
        ( ExpectBlock oneName oneExp, ExpectBlock twoName twoExp ) ->
            oneName == twoName && matchExpected oneExp twoExp

        ( ExpectStub one, ExpectStub two ) ->
            one == two

        ( ExpectRecord one oneFields, ExpectRecord two twoFields ) ->
            one == two && List.all (matchFields twoFields) oneFields

        ( ExpectOneOf oneOptions, ExpectOneOf twoOptions ) ->
            List.all (matchExpectedOptions twoOptions) oneOptions

        ( ExpectManyOf oneOptions, ExpectManyOf twoOptions ) ->
            List.all (matchExpectedOptions twoOptions) oneOptions

        ( ExpectStartsWith oneStart oneRemain, ExpectStartsWith twoStart twoRemain ) ->
            matchExpected oneStart twoStart
                && matchExpected oneRemain twoRemain

        ( ExpectBoolean _, ExpectBoolean _ ) ->
            True

        ( ExpectInteger _, ExpectInteger _ ) ->
            True

        ( ExpectFloat _, ExpectFloat _ ) ->
            True

        ( ExpectFloatBetween oneDetails, ExpectFloatBetween twoDetails ) ->
            oneDetails.max == twoDetails.max && oneDetails.min == twoDetails.min

        ( ExpectIntBetween oneDetails, ExpectIntBetween twoDetails ) ->
            oneDetails.max == twoDetails.max && oneDetails.min == twoDetails.min

        ( ExpectText oneInline, ExpectText twoInline ) ->
            True

        ( ExpectString _, ExpectString _ ) ->
            True

        ( ExpectMultiline _, ExpectMultiline _ ) ->
            True

        ( ExpectStringExactly oneName, ExpectStringExactly twoName ) ->
            oneName == twoName

        ( ExpectDate _, ExpectDate _ ) ->
            True

        ( ExpectTree oneIcon oneContent, ExpectTree twoIcon twoContent ) ->
            True

        _ ->
            False


matchExpectedOptions : List Expectation -> Expectation -> Bool
matchExpectedOptions opts target =
    List.any (matchExpected target) opts


matchFields : List ( String, Expectation ) -> ( String, Expectation ) -> Bool
matchFields valid ( targetFieldName, targetFieldExpectation ) =
    let
        innerMatch ( validFieldName, validExpectation ) =
            validFieldName
                == targetFieldName
                && matchExpected validExpectation targetFieldExpectation
    in
    List.any innerMatch valid


within rangeOne rangeTwo =
    withinOffsetRange { start = rangeOne.start.offset, end = rangeOne.end.offset } rangeTwo


withinOffsetRange offset range =
    range.start.offset <= offset.start && range.end.offset >= offset.end



{- All the above ids are opaque, so we know they can't be spoofed.

       The editing commands all require one of these opaque values to be constructed.

       An id captures:

           1. The coordinates of a specific point
           2. What operations can be performed at that point
           3. A valid payload

       For ReplaceOneOf

           -> Can we accept an Expectation ++ ID Combo?

           -> Means we can't let the dev create their own Description


   Editing Messages are generated by an Editor that we create.

   Or by an editor fragment that we create.

   The expectation would be inflated with built in defaults


-}
{- EDITING

   A general sketch of Edits.

   If a human is sending updates, then likely these will be single character updates or deletions.



   Simple case, the edit is completely within a leaf node

       -> replace leaf node

   More advanced

       -> get smallest containing block
       -> generate source for that block
       -> replace target range with new string
       -> generate parser for that block
            -> Adjusting correctly for offsets
       -> reparse
       -> replace on AST
            -> Adjust node indexes

   Issues:
       -> Seems like a lot of work.

   Individual Edits

       -> addChar
           -> add space
           -> add newline
       -> deleteChar


-}
-- {-| -}
-- createField : Int -> ( String, Expectation ) -> ( Position, List ( String, Found Description ) ) -> ( Position, List ( String, Found Description ) )
-- createField currentIndent ( name, exp ) ( base, existingFields ) =
--     let
--         -- This is the beginning of the field
--         --    field = x
--         --   ^ right there
--         fieldValueStart =
--             base
--                 |> moveColumn (currentIndent * 4)
--         -- If the field value is more than a line
--         ( end, childField ) =
--             create (currentIndent + 1)
--                 (fieldValueStart
--                     -- Add a few characters to account for `field = `.
--                     |> moveColumn (String.length name + 3)
--                 )
--                 exp
--         height =
--             end.line
--                 - base.line
--         fieldEnd =
--             moveNewline end
--     in
--     if height == 0 then
--         ( fieldEnd
--         , ( name
--           , Found
--                 { start =
--                     fieldValueStart
--                 , end =
--                     fieldEnd
--                 }
--                 childField
--           )
--             :: existingFields
--         )
--     else
--         let
--             -- If the field value is more than a line
--             ( newEnd, newChildField ) =
--                 create (currentIndent + 1)
--                     (fieldValueStart
--                         -- account for just `fieldname =`
--                         |> moveColumn (String.length name + 2)
--                         |> moveNewline
--                         -- Indent one level further
--                         |> moveColumn (1 * 4)
--                     )
--                     exp
--             finalEnd =
--                 newEnd
--                     |> moveNewline
--         in
--         ( finalEnd
--         , ( name
--           , Found
--                 { start =
--                     fieldValueStart
--                 , end =
--                     finalEnd
--                 }
--                 newChildField
--           )
--             :: existingFields
--         )


{-| Given an expectation and a list of choices, verify that the expectation is a valid choice.
-}
make : Expectation -> List (Choice id Expectation) -> Maybe (Choice id Expectation)
make expected options =
    List.filterMap
        (\(Choice id exp) ->
            if matchExpected expected exp then
                Just (Choice id expected)

            else
                Nothing
        )
        options
        |> List.head



-- {-|
--     `Position` is the starting position for a block.
--     The same rules for indentation as they apply everywhere.
--         - Primitives do not handle their indentation.
--         - Block, Record, and Tree elements handle the indentation of their children.
-- -}
-- create : Int -> Position -> Expectation -> ( Position, Description )
-- create currentIndent base expectation =
--     case expectation of
--         ExpectBlock name childExpectation ->
--             let
--                 ( end, childDescription ) =
--                     create (currentIndent + 1)
--                         (base
--                             |> moveNewline
--                             |> moveColumn ((currentIndent + 1) * 4)
--                         )
--                         childExpectation
--             in
--             ( end
--             , DescribeBlock
--                 { name = name
--                 , found =
--                     Found
--                         { start = base
--                         , end = end
--                         }
--                         childDescription
--                 , expected = expectation
--                 }
--             )
--         ExpectStub name ->
--             let
--                 end =
--                     moveColumn (String.length name) base
--             in
--             ( end
--             , DescribeStub name
--                 (Found
--                     { start = base
--                     , end = end
--                     }
--                     name
--                 )
--             )
--         ExpectRecord name fields ->
--             let
--                 ( end, renderedFields ) =
--                     List.foldl (createField (currentIndent + 1)) ( moveNewline base, [] ) fields
--             in
--             ( end
--             , Record
--                 { name = name
--                 , found =
--                     Found
--                         { start = base
--                         , end = moveNewline end
--                         }
--                         renderedFields
--                 , expected = expectation
--                 }
--             )
--         ExpectTree icon content ->
--             let
--                 range =
--                     { start = base, end = base }
--                 items =
--                     []
--             in
--             ( moveNewline base
--             , DescribeTree
--                 { found = ( range, items )
--                 , expected = expectation
--                 }
--             )
--         ExpectOneOf choices ->
--             let
--                 id =
--                     Id
--                         { start = base
--                         , end = end
--                         }
--                 -- TODO: handle case of empty OneOf
--                 ( end, childDescription ) =
--                     create (currentIndent + 1)
--                         base
--                         (Maybe.withDefault (ExpectStub "Unknown") (List.head choices))
--             in
--             ( base
--             , OneOf
--                 { id = id
--                 , choices = List.map (Choice id) choices
--                 , child =
--                     Found { start = base, end = end } childDescription
--                 }
--             )
--         ExpectManyOf choices ->
--             let
--                 id =
--                     Id { start = base, end = base }
--             in
--             ( moveNewline base
--             , ManyOf
--                 { id = id
--                 , choices = List.map (Choice id) choices
--                 , children =
--                     List.foldl
--                         (\choice ( newBase, result ) ->
--                             let
--                                 ( endOfCreated, created ) =
--                                     create currentIndent newBase choice
--                             in
--                             ( endOfCreated
--                                 |> moveNewline
--                                 |> moveNewline
--                                 |> moveColumn (currentIndent * 4)
--                             , Found
--                                 { start = newBase
--                                 , end = endOfCreated
--                                 }
--                                 created
--                                 :: result
--                             )
--                         )
--                         ( base, [] )
--                         choices
--                         |> Tuple.second
--                         |> List.reverse
--                 }
--             )
--         ExpectStartsWith start remaining ->
--             let
--                 ( startEnd, startChildDescription ) =
--                     create currentIndent
--                         base
--                         start
--                 ( remainingEnd, remainingDescription ) =
--                     create currentIndent
--                         (moveNewline startEnd)
--                         remaining
--             in
--             ( remainingEnd
--             , StartsWith
--                 { start = base
--                 , end = remainingEnd
--                 }
--                 { found = startChildDescription
--                 , expected = start
--                 }
--                 { found = remainingDescription
--                 , expected = remaining
--                 }
--             )
--         -- Primitives
--         ExpectBoolean b ->
--             let
--                 boolString =
--                     boolToString b
--                 end =
--                     moveColumn (String.length boolString) base
--                 range =
--                     { start = base
--                     , end = end
--                     }
--             in
--             ( end
--             , DescribeBoolean
--                 { id = Id range
--                 , found =
--                     Found
--                         range
--                         b
--                 }
--             )
--         ExpectInteger i ->
--             let
--                 end =
--                     moveColumn
--                         (String.length (String.fromInt i))
--                         base
--                 pos =
--                     { start = base
--                     , end = end
--                     }
--             in
--             ( end
--             , DescribeInteger
--                 { id = Id pos
--                 , found = Found pos i
--                 }
--             )
--         ExpectFloat f ->
--             let
--                 end =
--                     moveColumn
--                         (String.length (String.fromFloat f))
--                         base
--                 pos =
--                     { start = base
--                     , end =
--                         end
--                     }
--             in
--             ( end
--             , DescribeFloat
--                 { id = Id pos
--                 , found = Found pos ( String.fromFloat f, f )
--                 }
--             )
--         ExpectFloatBetween details ->
--             let
--                 end =
--                     moveColumn
--                         (String.length (String.fromFloat details.default))
--                         base
--                 pos =
--                     { start = base
--                     , end =
--                         end
--                     }
--             in
--             ( end
--             , DescribeFloatBetween
--                 { id = Id pos
--                 , min = details.min
--                 , max = details.max
--                 , found =
--                     Found pos
--                         ( String.fromFloat details.default
--                         , details.default
--                         )
--                 }
--             )
--         ExpectIntBetween details ->
--             let
--                 end =
--                     moveColumn (String.length (String.fromInt details.default)) base
--                 pos =
--                     { start = base
--                     , end = end
--                     }
--             in
--             ( end
--             , DescribeIntBetween
--                 { id = Id pos
--                 , min = details.min
--                 , max = details.max
--                 , found = Found pos details.default
--                 }
--             )
--         ExpectString str ->
--             let
--                 end =
--                     moveColumn (String.length str) base
--                 pos =
--                     { start = base
--                     , end = end
--                     }
--             in
--             ( end
--             , DescribeString
--                 (Id pos)
--                 str
--             )
--         ExpectMultiline str ->
--             let
--                 end =
--                     moveColumn (String.length str) base
--                 -- TODO: This position is not correct!
--                 -- Account for newlines
--                 pos =
--                     { start = base
--                     , end = end
--                     }
--             in
--             ( end
--             , DescribeMultiline (Id pos) str
--             )
--         ExpectStringExactly str ->
--             let
--                 end =
--                     moveColumn (String.length str) base
--                 pos =
--                     { start = base
--                     , end = end
--                     }
--             in
--             ( end
--             , DescribeStringExactly pos str
--             )
--         -- ExpectDate ->
--         _ ->
--             let
--                 end =
--                     moveColumn (String.length "True") base
--                 range =
--                     { start = base
--                     , end = end
--                     }
--             in
--             ( end
--             , DescribeBoolean
--                 { id = Id range
--                 , found =
--                     Found
--                         range
--                         True
--                 }
--             )


boolToString : Bool -> String
boolToString b =
    if b then
        "True"

    else
        "False"


moveColumn : Int -> Position -> Position
moveColumn num pos =
    { offset = pos.offset + num
    , column = pos.column + num
    , line = pos.line
    }


moveNewline : Position -> Position
moveNewline pos =
    { offset = pos.offset + 1
    , column = 1
    , line = pos.line + 1
    }


startingPoint =
    { offset = 0
    , line = 1
    , column = 1
    }



-- push maybePush found =
--     case maybePush of
--         Nothing ->
--             found
--         Just to ->
--             case found of
--                 Found range item ->
--                     Found (pushRange to range) (pushDescription to item)
--                 Unexpected unexpected ->
--                     Unexpected { unexpected | range = pushRange to unexpected.range }
-- pushDescription to desc =
--     case desc of
--         DescribeString id str ->
--             DescribeString (pushId to id) str
--         _ ->
--             desc
-- pushId to (Id range) =
--     Id (pushRange to range)
-- pushRange to range =
--     { start = addPositions to range.start
--     , end = addPositions to range.end
--     }
-- addPositions to pos =
--     { offset = pos.offset + to.offset
--     , line = pos.line + to.line
--     , column = pos.column + to.column
--     }
-- pushFromRange { start, end } =
--     { offset = end.offset - start.offset
--     , line = end.line - start.line
--     , column = end.column - start.column
--     }


minusPosition end start =
    { offset = end.offset - start.offset
    , line = end.line - start.line
    , column = end.column - start.column
    }



-- sizeToRange start delta =
--     { start = start
--     , end =
--         addPositions start delta
--     }


{-| -}
getDescription : Parsed -> Found Description
getDescription (Parsed parsed) =
    parsed.found


{-| -}
getDesc : { start : Int, end : Int } -> Parsed -> List Description
getDesc offset (Parsed parsed) =
    getWithinFound offset parsed.found


{-| -}
getWithinFound : { start : Int, end : Int } -> Found Description -> List Description
getWithinFound offset found =
    case found of
        Found range item ->
            if withinOffsetRange offset range then
                if isPrimitive item then
                    [ item ]

                else
                    [ item ]
                        ++ getContainingDescriptions item offset

            else
                []

        Unexpected unexpected ->
            []


withinFoundLeaf offset found =
    case found of
        Found range item ->
            withinOffsetRange offset range

        Unexpected unexpected ->
            withinOffsetRange offset unexpected.range


isPrimitive : Description -> Bool
isPrimitive description =
    case description of
        DescribeBlock _ ->
            False

        Record _ ->
            False

        OneOf _ ->
            False

        ManyOf _ ->
            False

        StartsWith _ fst snd ->
            False

        DescribeTree details ->
            False

        -- Primitives
        DescribeStub name found ->
            True

        DescribeBoolean found ->
            True

        DescribeInteger found ->
            True

        DescribeFloat found ->
            True

        DescribeFloatBetween _ ->
            True

        DescribeIntBetween _ ->
            True

        DescribeText _ ->
            True

        DescribeString rng str ->
            True

        DescribeMultiline rng str ->
            True

        DescribeStringExactly rng str ->
            True

        DescribeDate found ->
            True


{-| -}
getContainingDescriptions : Description -> { start : Int, end : Int } -> List Description
getContainingDescriptions description offset =
    case description of
        DescribeBlock details ->
            getWithinFound offset details.found

        Record details ->
            case details.found of
                Found range fields ->
                    if withinOffsetRange offset range then
                        List.concatMap (getWithinFound offset << Tuple.second) fields

                    else
                        []

                Unexpected unexpected ->
                    if withinOffsetRange offset unexpected.range then
                        []

                    else
                        []

        OneOf one ->
            getWithinFound offset one.child

        ManyOf many ->
            List.concatMap (getWithinFound offset) many.children

        StartsWith range fst snd ->
            if withinOffsetRange offset range then
                getContainingDescriptions fst.found offset ++ getContainingDescriptions snd.found offset

            else
                []

        DescribeTree details ->
            case details.found of
                ( range, items ) ->
                    if withinOffsetRange offset range then
                        List.concatMap (getWithinNested offset) items

                    else
                        []

        -- Primitives
        DescribeStub name found ->
            if withinFoundLeaf offset found then
                [ description ]

            else
                []

        DescribeBoolean details ->
            if withinFoundLeaf offset details.found then
                [ description ]

            else
                []

        DescribeInteger details ->
            if withinFoundLeaf offset details.found then
                [ description ]

            else
                []

        DescribeFloat details ->
            if withinFoundLeaf offset details.found then
                [ description ]

            else
                []

        DescribeFloatBetween details ->
            if withinFoundLeaf offset details.found then
                [ description ]

            else
                []

        DescribeIntBetween details ->
            if withinFoundLeaf offset details.found then
                [ description ]

            else
                []

        DescribeText txt ->
            if withinOffsetRange offset (getRange txt.id) then
                [ description ]

            else
                []

        DescribeString id str ->
            if withinOffsetRange offset (getRange id) then
                [ description ]

            else
                []

        DescribeMultiline id str ->
            if withinOffsetRange offset (getRange id) then
                [ description ]

            else
                []

        DescribeStringExactly rng str ->
            if withinOffsetRange offset rng then
                [ description ]

            else
                []

        DescribeDate details ->
            if withinFoundLeaf offset details.found then
                [ description ]

            else
                []


getWithinNested offset (Nested nest) =
    case nest.content of
        ( desc, items ) ->
            getContainingDescriptions desc offset
                ++ List.concatMap
                    (\item ->
                        getContainingDescriptions item offset
                    )
                    items



{- DESCRIPTION -> STRING -}


{-| -}
toString : Parsed -> String
toString (Parsed parsed) =
    writeFound writeDescription
        parsed.found
        { indent = 0
        , position = { line = 1, column = 1, offset = 0 }
        , printed = ""
        }
        |> .printed


descriptionToString desc =
    writeDescription
        desc
        { indent = 0
        , position = { line = 1, column = 1, offset = 0 }
        , printed = ""
        }
        |> .printed


getSize desc =
    writeDescription desc
        { indent = 0
        , position = { line = 1, column = 1, offset = 0 }
        , printed = ""
        }
        |> .position
        |> (\pos ->
                { column = pos.column - 1
                , line = pos.line - 1
                , offset = pos.offset
                }
           )


type alias PrintCursor =
    { indent : Int
    , position : Position
    , printed : String
    }


write : String -> PrintCursor -> PrintCursor
write str cursor =
    { cursor
        | printed = cursor.printed ++ str
        , position =
            (\pos ->
                { pos
                    | offset = pos.offset + String.length str
                    , column = pos.column + String.length str
                }
            )
                cursor.position
    }


writeNewline : PrintCursor -> PrintCursor
writeNewline cursor =
    { cursor
        | printed = cursor.printed ++ "\n"
        , position =
            (\pos ->
                { pos
                    | offset = pos.offset + 1
                    , column = 1
                    , line = pos.line + 1
                }
            )
                cursor.position
    }


writeNewlines : Int -> PrintCursor -> PrintCursor
writeNewlines n cursor =
    { cursor
        | printed = cursor.printed ++ String.repeat n "\n"
        , position =
            (\pos ->
                { pos
                    | offset = pos.offset + n
                    , column = 1
                    , line = pos.line + n
                }
            )
                cursor.position
    }


{-| Add spaces and newlines in order to make up the discrepancy between cursor and target
-}
advanceTo : Range -> PrintCursor -> PrintCursor
advanceTo target cursor =
    let
        lineDiff =
            abs (target.start.line - cursor.position.line)
    in
    if target.start == cursor.position then
        cursor

    else if lineDiff == 0 then
        write (String.repeat (target.start.column - cursor.position.column) " ") cursor

    else
        cursor
            |> writeNewlines lineDiff
            |> write (String.repeat (target.start.column - 1) " ")


writeIndent : PrintCursor -> PrintCursor
writeIndent cursor =
    write (String.repeat (cursor.indent * 4) " ") cursor


writeLine line cursor =
    cursor
        |> write line
        |> writeNewline


indent : PrintCursor -> PrintCursor
indent cursor =
    { cursor | indent = cursor.indent + 1 }


dedent : PrintCursor -> PrintCursor
dedent cursor =
    { cursor | indent = max 0 cursor.indent - 1 }


{-| -}
writeDescription : Description -> PrintCursor -> PrintCursor
writeDescription description cursor =
    case description of
        DescribeBlock details ->
            cursor
                |> write ("| " ++ details.name)
                |> indent
                |> writeFound writeDescription details.found
                |> dedent

        DescribeStub name found ->
            cursor
                |> write "|"
                |> writeFound (writeWith identity) found

        Record details ->
            cursor
                |> writeFound
                    (\fields curs ->
                        curs
                            -- TODO: This seems to be necessary for the recordOfRecord test, but
                            -- makes things parsed normally fail...sooo
                            -- |> writeIndent
                            |> Debug.log ("record: " ++ details.name)
                            |> write ("| " ++ details.name)
                            |> indent
                            |> (\c ->
                                    List.foldr writeField c fields
                               )
                    )
                    details.found
                |> dedent

        OneOf one ->
            cursor
                |> writeFound writeDescription one.child

        ManyOf many ->
            List.foldl
                (writeFound writeDescription)
                cursor
                many.children

        StartsWith range start end ->
            cursor
                |> writeDescription start.found
                |> writeDescription end.found

        DescribeBoolean details ->
            writeFound (writeWith boolToString) details.found cursor

        DescribeInteger details ->
            writeFound (writeWith String.fromInt) details.found cursor

        DescribeFloat details ->
            writeFound (writeWith Tuple.first) details.found cursor

        DescribeFloatBetween details ->
            writeFound (writeWith Tuple.first) details.found cursor

        DescribeIntBetween details ->
            writeFound (writeWith String.fromInt) details.found cursor

        DescribeText txt ->
            cursor
                |> advanceTo (getRange txt.id)
                |> (\c -> List.foldl writeTextDescription c txt.text)

        DescribeString id str ->
            cursor
                |> advanceTo (getRange id)
                |> write str

        DescribeMultiline id str ->
            let
                indented =
                    String.lines str
                        |> List.indexedMap
                            (\i s ->
                                if s == "" || i == 0 then
                                    s

                                else
                                    String.repeat (cursor.indent * 4) " " ++ s
                            )

                numLines =
                    List.length indented
            in
            cursor
                |> advanceTo (getRange id)
                |> (\curs ->
                        List.foldl
                            (\line ( i, advancedCurs ) ->
                                if i == numLines then
                                    ( i + 1, write line advancedCurs )

                                else
                                    ( i + 1, writeLine line advancedCurs )
                            )
                            ( 1, curs )
                            indented
                   )
                |> Tuple.second

        DescribeStringExactly range str ->
            cursor
                |> advanceTo range
                |> write str

        DescribeDate details ->
            writeFound (writeWith Tuple.first) details.found cursor

        DescribeTree tree ->
            case tree.found of
                ( range, nestedItems ) ->
                    cursor
                        |> advanceTo range
                        |> (\curs -> List.foldl writeNested curs nestedItems)


writeNested (Nested node) cursor =
    cursor
        |> writeDescription (Tuple.first node.content)
        |> (\curs -> List.foldl writeDescription curs (Tuple.second node.content))
        |> indent
        |> (\curs -> List.foldl writeNested curs node.children)
        |> dedent


textDescriptionToString txt =
    case txt of
        Styled range t ->
            textToString t

        InlineToken details ->
            case details.attributes of
                [] ->
                    "{" ++ details.name ++ "}"

                _ ->
                    "{"
                        ++ details.name
                        ++ " |"
                        ++ String.join ", " (List.map inlineDescToString details.attributes)
                        ++ "}"

        InlineAnnotation details ->
            "["
                ++ String.join "" (List.map textToString details.text)
                ++ "]{"
                ++ String.join ", " (List.map inlineDescToString details.attributes)
                ++ "}"

        UnexpectedInline unexpected ->
            ""


inlineDescToString : InlineAttribute -> String
inlineDescToString inlineDesc =
    case inlineDesc of
        AttrString { name, range, value } ->
            name ++ " = " ++ value



-- DescribeInlineText range txts ->
--     String.join "" (List.map textToString txts)


writeTextDescription desc curs =
    write (textDescriptionToString desc) curs


writeTextNode node curs =
    write (textToString node) curs


textToString : Text -> String
textToString (Text styles txt) =
    txt


writeWith toStr a cursor =
    write (toStr a) cursor


writeFound : (a -> PrintCursor -> PrintCursor) -> Found a -> PrintCursor -> PrintCursor
writeFound fn found cursor =
    case found of
        Found range fnd ->
            cursor
                |> advanceTo range
                |> fn fnd

        Unexpected unexpected ->
            cursor
                |> advanceTo unexpected.range


writeField : ( String, Found Description ) -> PrintCursor -> PrintCursor
writeField ( name, foundVal ) cursor =
    case foundVal of
        Found rng fnd ->
            cursor
                |> advanceTo rng
                |> write (name ++ " =")
                |> writeDescription fnd

        Unexpected unexpected ->
            cursor
                |> advanceTo unexpected.range
