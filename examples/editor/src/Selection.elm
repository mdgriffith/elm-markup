module Selection exposing
    ( Box
    , CharBox
    , CharLayout
    , Offset
    , Selection(..)
    , decode
    , move
    , moveDown
    , moveUp
    , resync
    , select
    , selectMany
    )

{-| -}

import Json.Decode as Decode
import Mark
import Mark.Edit


{-| A Layout should be an ordered list in the source order that the characters appear in the html.
-}
type CharLayout
    = CharLayout (List CharBox)


type alias CharBox =
    { id : Mark.Edit.Id
    , offset : Offset
    , box : Box
    }


type alias Offset =
    Int


{-| x,y are document absolute.
-}
type alias Box =
    { x : Float
    , y : Float
    , height : Float
    , width : Float
    }



{- CharBox operations -}


{-| -}
move : Int -> CharBox -> CharBox
move i charBox =
    { id = charBox.id
    , offset = charBox.offset + i
    , box = charBox.box
    }


{-| Move the cursor up one line
-}
moveUp : CharLayout -> CharBox -> CharBox
moveUp (CharLayout layout) charBox =
    let
        yTarget =
            charBox.box.y - 10

        xTarget =
            charBox.box.x
    in
    List.foldl (find ( xTarget, yTarget )) NoMatch layout
        |> matchToMaybe
        |> Maybe.withDefault charBox


{-| Move the cursor up one line
-}
moveDown : CharLayout -> CharBox -> CharBox
moveDown (CharLayout layout) charBox =
    let
        yTarget =
            charBox.box.y + charBox.box.height + 10

        xTarget =
            charBox.box.x
    in
    List.foldl (find ( xTarget, yTarget )) NoMatch layout
        |> matchToMaybe
        |> Maybe.withDefault charBox


within ( x, y ) box =
    (x >= box.x)
        && (x <= box.x + box.width)
        && (y >= box.y)
        && (y <= box.y + box.height)


closestSide ( x, y ) box =
    if max 0 (x - box.x) <= (box.width / 2) then
        Left

    else
        Right


type Side
    = Left
    | Right


{-| -}
select : ( Float, Float ) -> CharLayout -> Maybe CharBox
select coords (CharLayout layout) =
    List.foldl (find coords) NoMatch layout
        |> matchToMaybe


type Synced x
    = Unsynced x
    | Synced x


{-| Update a bounding boxed based on id and offset match.
-}
resync : CharLayout -> CharBox -> CharBox
resync (CharLayout layout) charBox =
    case List.foldl sync (Unsynced charBox) layout of
        Synced x ->
            x

        Unsynced x ->
            x


sync charBox synced =
    case synced of
        Synced _ ->
            synced

        Unsynced unsynced ->
            if unsynced.id == charBox.id && unsynced.offset == charBox.offset then
                Synced charBox

            else
                synced


type Selection
    = Single CharBox
    | Many CharBox (List CharBox) CharBox


{-| -}
selectMany : ( Float, Float ) -> ( Float, Float ) -> CharLayout -> Maybe Selection
selectMany anchor focus (CharLayout layout) =
    let
        ( start, end ) =
            -- sort
            ( anchor, focus )
    in
    List.foldl (findSegment start end) Nothing layout
        |> Maybe.andThen segmentCursorToMaybeSelection


segmentCursorToMaybeSelection segment =
    case segment of
        Started start ->
            Just (Single start)

        StartNext start ->
            Just (Single start)

        Selected start middle end ->
            Just (Many start middle end)

        Finished start middle end ->
            Just (Many start middle end)

        FinishNext start middle end ->
            Just (Many start middle end)


type SegmentCursor
    = Started CharBox
    | StartNext CharBox
    | Selected CharBox (List CharBox) CharBox
    | FinishNext CharBox (List CharBox) CharBox
    | Finished CharBox (List CharBox) CharBox


findSegment : ( Float, Float ) -> ( Float, Float ) -> CharBox -> Maybe SegmentCursor -> Maybe SegmentCursor
findSegment start end current maybeFound =
    case maybeFound of
        Nothing ->
            if within start current.box then
                if within end current.box then
                    Just (Finished current [] current)

                else
                    case closestSide start current.box of
                        Left ->
                            Just (Started current)

                        Right ->
                            Just (StartNext current)

            else
                Nothing

        Just (StartNext _) ->
            Just (Started current)

        Just (Started started) ->
            if within end current.box then
                Just (Finished started [] current)

            else
                Just (Selected started [] current)

        Just (FinishNext started middle ended) ->
            if within end current.box then
                Just (Finished started (ended :: middle) current)

            else
                Just (Selected started (ended :: middle) current)

        Just (Selected started middle ended) ->
            if within end current.box then
                Just (Finished started (ended :: middle) current)

            else
                Just (Selected started (ended :: middle) current)

        Just (Finished _ _ _) ->
            maybeFound


matchToMaybe match =
    case match of
        NoMatch ->
            Nothing

        MatchNext x ->
            Just x

        Matched x ->
            Just x


type Match thing
    = NoMatch
    | MatchNext thing
    | Matched thing


find coords charBox maybeFound =
    case maybeFound of
        NoMatch ->
            if within coords charBox.box then
                case closestSide coords charBox.box of
                    Left ->
                        Matched charBox

                    Right ->
                        MatchNext charBox

            else
                NoMatch

        MatchNext _ ->
            Matched charBox

        _ ->
            maybeFound



{- Decoding -}


decode =
    Decode.map CharLayout
        (Decode.list decodeCharBox)


decodeCharBox =
    Decode.map3 CharBox
        (Decode.field "id" decodeId)
        (Decode.field "offset" decodeOffset)
        (Decode.field "box" decodeBox)


decodeId : Decode.Decoder Mark.Edit.Id
decodeId =
    Decode.string
        |> Decode.andThen
            (\str ->
                case Mark.stringToId str of
                    Nothing ->
                        Decode.fail "Invalid Id"

                    Just id ->
                        Decode.succeed id
            )


decodeOffset : Decode.Decoder Offset
decodeOffset =
    Decode.int


decodeBox : Decode.Decoder Box
decodeBox =
    Decode.map4 Box
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)
        (Decode.field "height" Decode.float)
        (Decode.field "width" Decode.float)
