module Model exposing (..)

import Random exposing (Generator)
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
    , Random.generate NewPiece Piece.random
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
