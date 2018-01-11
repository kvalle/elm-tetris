module Subscriptions exposing (subscriptions)

import Model exposing (Model)
import Messages exposing (Msg(..))
import Types.Common exposing (Direction(..))
import Types.GameState exposing (GameState(..))
import Keyboard
import Time


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        NotStarted ->
            Keyboard.downs startGameOnSpace

        Running _ ->
            Sub.batch
                [ Time.every (Time.second * 0.5) (always Tick)
                , Keyboard.downs moveBrick
                , Keyboard.downs rotateBrick
                ]

        GameOver _ ->
            Keyboard.downs startGameOnSpace


startGameOnSpace : Keyboard.KeyCode -> Msg
startGameOnSpace keyCode =
    if keyCode == 32 then
        StartGame
    else
        NoOp


moveBrick : Keyboard.KeyCode -> Msg
moveBrick keyCode =
    case keyCode of
        37 ->
            Move Left

        39 ->
            Move Right

        40 ->
            Move Down

        _ ->
            NoOp


rotateBrick : Keyboard.KeyCode -> Msg
rotateBrick keyCode =
    case keyCode of
        38 ->
            Rotate

        _ ->
            NoOp
