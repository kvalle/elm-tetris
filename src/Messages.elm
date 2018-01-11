module Messages exposing (Msg(..))

import Types.Common exposing (Direction)
import Types.Piece exposing (Piece)


type Msg
    = NoOp
    | StartGame
    | Tick
    | Move Direction
    | NewPiece Piece
    | Rotate
