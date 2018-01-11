module Messages exposing (Msg(..))

import Types exposing (Direction, Piece)


type Msg
    = NoOp
    | StartGame
    | Tick
    | Move Direction
    | NewPiece Piece
    | Rotate
