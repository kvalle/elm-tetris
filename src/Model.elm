module Model exposing (..)

import Messages exposing (Msg(..))
import Types.Board as Board exposing (Board)
import Types.Piece as Piece exposing (Piece)
import Types.GameState exposing (GameState(..))


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
