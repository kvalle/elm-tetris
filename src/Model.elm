module Model exposing (..)

import Messages exposing (Msg(..))
import Types.Board as Board exposing (Board)
import Types.Piece as Piece exposing (Piece)
import Types.Common exposing (GameState(..))


type alias Model =
    { board : Board
    , piece : Piece
    , state : GameState
    }


init : ( Model, Cmd Msg )
init =
    ( { board = Board.empty
      , piece = Piece.empty
      , state = NotStarted
      }
    , Cmd.none
    )


score : Model -> Int
score model =
    case model.state of
        NotStarted ->
            0

        Running score ->
            score

        GameOver score ->
            score


addPoints : Int -> GameState -> GameState
addPoints n state =
    case state of
        NotStarted ->
            NotStarted

        Running score ->
            Running <| score + n

        GameOver score ->
            GameOver <| score + n
