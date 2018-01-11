module Types.Piece exposing (Piece, empty, positions, random, move, rotate)

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


positions : Piece -> List Pos
positions piece =
    piece.blocks
        |> List.map (Tuple.mapFirst <| (+) piece.pos.row)
        |> List.map (Tuple.mapSecond <| (+) piece.pos.col)
        |> List.map (uncurry Pos)


pieces : Nonempty Piece
pieces =
    let
        theLongStrightOne =
            { color = Red
            , pos = { row = 1, col = Config.width // 2 }
            , blocks = [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]
            }

        theOneThatLooksLikeL =
            { color = Yellow
            , pos = { row = 1, col = Config.width // 2 }
            , blocks = [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 1, 1 ) ]
            }

        theBackwardsL =
            { color = Blue
            , pos = { row = 1, col = Config.width // 2 }
            , blocks = [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 1, -1 ) ]
            }

        thePyramidThing =
            { color = Green
            , pos = { row = 1, col = Config.width // 2 }
            , blocks = [ ( -1, 0 ), ( 0, 0 ), ( 0, 1 ), ( 1, 0 ) ]
            }
    in
        Nonempty theLongStrightOne
            [ theOneThatLooksLikeL, theBackwardsL, thePyramidThing ]


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
