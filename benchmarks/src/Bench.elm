module Main exposing (main, source, suite)

import Benchmark exposing (..)
import Benchmark.LowLevel
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Browser
import Html
import Html.Attributes
import Mark
import Mark.Default
import Task


main : BenchmarkProgram
main =
    program suite


mainManual =
    Browser.element
        { init =
            \() ->
                ( ( 0, "Parse Post" )
                , Task.attempt NewResults (Benchmark.LowLevel.sample 100 (Benchmark.LowLevel.operation parsePost))
                )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


parsePost () =
    Mark.parse Mark.Default.document source


type Msg
    = NewResults (Result Benchmark.LowLevel.Error Float)


update msg (( count, label ) as model) =
    case msg of
        NewResults result ->
            case result of
                Ok i ->
                    let
                        parsed =
                            Debug.log "result" (parsePost ())
                    in
                    ( ( i, label ), Cmd.none )

                Err error ->
                    let
                        _ =
                            Debug.log "error" error
                    in
                    ( model, Cmd.none )


view ( count, label ) =
    Html.text (label ++ ":" ++ String.fromFloat count)


suite : Benchmark
suite =
    describe "Mark"
        [ benchmark "Parse a Simple Blogpost" <|
            \_ ->
                Mark.parse Mark.Default.document source
        ]


source =
    """| Title
    My Article


Lorem Ipsum is simply--- dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's /standard/ dummy text ever since the 1500's, when an "unknown printer" took a galley of type and scrambled it to<>make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was *popularised* in the 1960's with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.

But, for real, here's a kitten.

| Image
    description = What a cute kitten.
    src = http://placekitten.com/g/200/300


"""
