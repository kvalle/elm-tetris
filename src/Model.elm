module Model exposing (..)

import Random exposing (Generator)
import Messages exposing (Msg(..))
import Types exposing (Board, Piece, GameState(..))


type alias Model =
    { board : Board
    , piece : Piece
    , state : GameState
    }


init : ( Model, Cmd Msg )
init =
    ( { board = Types.emptyBoard
      , piece = Types.emptyPiece
      , state = NotStarted
      }
    , Random.generate NewPiece Types.randomPiece
    )
