module Mark.New exposing
    ( Block(..), from, field
    , bool, float, int, string
    )

{-|

@docs Block, from, field

@docs bool, float, int, string

-}

import Mark.Format as Format
import Mark.Internal.Description exposing (..)
import Mark.Internal.Error as Error
import Mark.Internal.Id as Id exposing (..)
import Mark.Internal.Outcome as Outcome
import Mark.Internal.Parser as Parse
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)



{-
   General Use of Setters.


   makeCircle default =
       New.from default
           |> New.field "label" (New.string "Heres my circle!")
           |> New.field "x" (New.int 10)



-}


{-| -}
type Block
    = Block


{-| -}
type ExpError
    = ExpError


{-| -}
from : Expectation -> Result ExpError Expectation
from exp =
    Ok exp


{-| -}
block :
    (Result ExpError Expectation -> Result ExpError Expectation)
    -> Result ExpError Expectation
    -> Result ExpError Expectation
block ter resExp =
    resExp


{-| -}
int : Int -> Result ExpError Expectation -> Result ExpError Expectation
int i resExp =
    case resExp of
        Err _ ->
            resExp

        Ok exp ->
            case exp of
                ExpectInteger _ ->
                    Ok (ExpectInteger i)

                ExpectIntBetween details ->
                    if i >= details.min && i <= details.max then
                        Ok (ExpectIntBetween { details | default = i })

                    else
                        Err ExpError

                _ ->
                    Err ExpError


{-| -}
string : String -> Result ExpError Expectation -> Result ExpError Expectation
string str resExp =
    case resExp of
        Err _ ->
            resExp

        Ok exp ->
            case exp of
                ExpectString _ ->
                    Ok (ExpectString str)

                ExpectMultiline _ ->
                    Ok (ExpectMultiline str)

                _ ->
                    Err ExpError


{-| -}
float : Float -> Result ExpError Expectation -> Result ExpError Expectation
float f resExp =
    case resExp of
        Err _ ->
            resExp

        Ok exp ->
            case exp of
                ExpectFloat _ ->
                    Ok (ExpectFloat f)

                ExpectFloatBetween details ->
                    if f >= details.min && f <= details.max then
                        Ok (ExpectFloatBetween { details | default = f })

                    else
                        Err ExpError

                _ ->
                    Err ExpError


{-| -}
bool : Bool -> Result ExpError Expectation -> Result ExpError Expectation
bool b resExp =
    case resExp of
        Err _ ->
            resExp

        Ok exp ->
            case exp of
                ExpectBoolean _ ->
                    Ok (ExpectBoolean b)

                _ ->
                    Err ExpError


{-| -}
field :
    String
    -> (Result ExpError Expectation -> Result ExpError Expectation)
    -> Result ExpError Expectation
    -> Result ExpError Expectation
field fieldName fieldSetter resExp =
    case resExp of
        Err _ ->
            resExp

        Ok exp ->
            case exp of
                ExpectRecord recordName fields ->
                    case List.foldl (recordField fieldName fieldSetter) (NotYet []) fields of
                        Failed err ->
                            Err err

                        NotYet _ ->
                            Err ExpError

                        UpdateMade updatedFields ->
                            Ok
                                (ExpectRecord recordName (List.reverse updatedFields))

                _ ->
                    Err ExpError


type Updated x
    = UpdateMade x
    | NotYet x
    | Failed ExpError


recordField :
    String
    ->
        (Result ExpError Expectation
         -> Result ExpError Expectation
        )
    -> ( String, Expectation )
    -> Updated (List ( String, Expectation ))
    -> Updated (List ( String, Expectation ))
recordField targetFieldName fieldSetter ( fieldName, fieldExp ) gathered =
    case gathered of
        Failed x ->
            gathered

        UpdateMade fields ->
            UpdateMade (( fieldName, fieldExp ) :: fields)

        NotYet fields ->
            if fieldName == targetFieldName then
                case fieldSetter (Ok fieldExp) of
                    Err err ->
                        Failed err

                    Ok updated ->
                        UpdateMade (( fieldName, updated ) :: fields)

            else
                NotYet (( fieldName, fieldExp ) :: fields)
