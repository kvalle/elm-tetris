module Main exposing (main)

import Html exposing (..)
import Random exposing (Generator)
import Model exposing (Model, init)
import View exposing (view)
import Messages exposing (Msg(..))
import Types exposing (GameState(..), Direction(..))
import Subscriptions exposing (subscriptions)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartGame ->
            ( { model | state = Running 0 }
            , Cmd.none
            )

        NewPiece piece ->
            ( if Types.legal piece model.board then
                { model | piece = piece }
              else
                { model | state = GameOver <| Types.score model.state }
            , Cmd.none
            )

        Tick ->
            let
                movedPiece =
                    Types.move Down model.piece
            in
                if Types.legal movedPiece model.board then
                    ( { model | piece = movedPiece }
                    , Cmd.none
                    )
                else
                    let
                        newBoard =
                            model.board
                                |> Types.addPiece model.piece
                                |> Types.clearFullRows
                    in
                        ( { model
                            | board = newBoard
                            , piece = Types.emptyPiece
                          }
                        , Random.generate NewPiece Types.randomPiece
                        )

        Move direction ->
            ( { model
                | piece = Types.moveIfPossible direction model.board model.piece
              }
            , Cmd.none
            )

        Rotate ->
            ( { model | piece = Types.rotate model.piece }, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
