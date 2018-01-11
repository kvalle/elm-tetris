module Messages exposing (Msg(..))

import Types.Common exposing (Direction)
import Types.Piece exposing (Piece)


type Msg
    = NoOp
    | StartNewGame
    | NewGame Piece Piece
    | NewPiece Piece
    | Tick
    | Move Direction
    | Rotate
