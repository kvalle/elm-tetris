module Types.Pos exposing (Pos, mapRow, mapCol)


type alias Pos =
    { row : Int, col : Int }


mapRow : (Int -> Int) -> Pos -> Pos
mapRow fn pos =
    { pos | row = fn pos.row }


mapCol : (Int -> Int) -> Pos -> Pos
mapCol fn pos =
    { pos | col = fn pos.col }
