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

        Paused _ ->
            Keyboard.downs resumeOnR

        Running _ ->
            Sub.batch
                [ Time.every
                    (Time.millisecond * toFloat (500 - (10 * model.speed)))
                    (always Tick)
                , Keyboard.downs moveBrick
                , Keyboard.downs rotateBrick
                , Keyboard.downs dropPieceOnEnter
                , Keyboard.downs pauseOnP
                ]

        GameOver _ ->
            Keyboard.downs startGameOnSpace


pauseOnP : Keyboard.KeyCode -> Msg
pauseOnP keyCode =
    if keyCode == 80 then
        PauseGame
    else
        NoOp


resumeOnR : Keyboard.KeyCode -> Msg
resumeOnR keyCode =
    if keyCode == 82 then
        ResumeGame
    else
        NoOp


dropPieceOnEnter : Keyboard.KeyCode -> Msg
dropPieceOnEnter keyCode =
    if keyCode == 13 then
        Drop
    else
        NoOp


startGameOnSpace : Keyboard.KeyCode -> Msg
startGameOnSpace keyCode =
    if keyCode == 32 then
        StartNewGame
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
