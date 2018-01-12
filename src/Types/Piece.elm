module Types.Piece exposing (Piece, empty, isEmpty, positions, random, move, rotate)

import Random exposing (Generator)
import List.Nonempty exposing (Nonempty(..))
import Types.Common exposing (Color(..), Direction(..))
import Types.Pos as Pos exposing (Pos)
import Config


type alias Piece =
    { -- cell type for this piece
      color : Color
    , -- center position of piece
      pos : Pos
    , -- relative coordinates of blocks within piece
      blocks : List ( Int, Int )
    }


empty : Piece
empty =
    { color = Blue, pos = Pos 0 0, blocks = [] }


isEmpty : Piece -> Bool
isEmpty piece =
    piece.blocks == []


positions : Piece -> List Pos
positions piece =
    piece.blocks
        |> List.map (Tuple.mapFirst <| (+) piece.pos.row)
        |> List.map (Tuple.mapSecond <| (+) piece.pos.col)
        |> List.map (uncurry Pos)


pieces : Nonempty Piece
pieces =
    let
        theI =
            { color = Red
            , pos = { row = 1, col = Config.width // 2 }
            , blocks = [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]
            }

        theL =
            { color = Yellow
            , pos = { row = 1, col = Config.width // 2 }
            , blocks = [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 1, 1 ) ]
            }

        theJ =
            { color = Purple
            , pos = { row = 1, col = Config.width // 2 }
            , blocks = [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 1, -1 ) ]
            }

        theT =
            { color = Green
            , pos = { row = 1, col = Config.width // 2 }
            , blocks = [ ( -1, 0 ), ( 0, 0 ), ( 0, 1 ), ( 1, 0 ) ]
            }

        theO =
            { color = Blue
            , pos = { row = 1, col = Config.width // 2 }
            , blocks = [ ( -1, 0 ), ( -1, 1 ), ( 0, 0 ), ( 0, 1 ) ]
            }

        theS =
            { color = Orange
            , pos = { row = 1, col = Config.width // 2 }
            , blocks = [ ( -1, 0 ), ( -1, 1 ), ( 0, -1 ), ( 0, 0 ) ]
            }

        theZ =
            { color = Gray
            , pos = { row = 1, col = Config.width // 2 }
            , blocks = [ ( -1, -1 ), ( -1, 0 ), ( 0, 0 ), ( 0, 1 ) ]
            }
    in
        Nonempty theI
            [ theL, theJ, theT, theO, theS, theZ ]


random : Generator Piece
random =
    List.Nonempty.sample pieces


move : Direction -> Piece -> Piece
move direction piece =
    let
        fn =
            case direction of
                Down ->
                    Pos.mapRow <| (+) 1

                Left ->
                    Pos.mapCol <| flip (-) 1

                Right ->
                    Pos.mapCol <| (+) 1
    in
        { piece | pos = fn piece.pos }


rotate : Piece -> Piece
rotate piece =
    { piece
        | blocks = piece.blocks |> List.map (\( row, col ) -> ( -col, row ))
    }
