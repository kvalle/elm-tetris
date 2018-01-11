module Types.Board
    exposing
        ( Board
        , Cell(..)
        , empty
        , clearFullRows
        , getCell
        , toPosList
        , addPiece
        )

import Array exposing (Array)
import Types.Pos exposing (Pos)
import Types.Common exposing (Color(..))
import Types.Piece as Piece exposing (Piece)
import Config


type alias Board =
    Array (Array Cell)


type Cell
    = Empty
    | Filled Color


empty : Board
empty =
    Array.repeat Config.height <| Array.repeat Config.width Empty


clearFullRows : Board -> ( Board, Int )
clearFullRows board =
    let
        filteredRows =
            Array.filter (Array.toList >> List.any ((==) Empty)) board

        rowsRemoved =
            Array.length board - Array.length filteredRows

        replacementRows =
            Array.repeat rowsRemoved (Array.repeat Config.width Empty)
    in
        ( Array.append replacementRows filteredRows
        , rowsRemoved
        )


getCell : Pos -> Board -> Maybe Cell
getCell pos board =
    board
        |> Array.get pos.row
        |> Maybe.map (Array.get pos.col)
        |> Maybe.withDefault Nothing


setCell : Pos -> Cell -> Board -> Board
setCell pos cell board =
    let
        maybeRow =
            board
                |> Array.get pos.row
                |> Maybe.map (Array.set pos.col cell)
    in
        case maybeRow of
            Just row ->
                Array.set pos.row row board

            Nothing ->
                board


toPosList : Board -> List ( Pos, Cell )
toPosList board =
    Array.map (Array.toIndexedList) board
        |> Array.toIndexedList
        |> List.concatMap
            (\( row, cells ) ->
                cells
                    |> List.map
                        (\( col, cell ) ->
                            ( Pos row col, cell )
                        )
            )


addPiece : Piece -> Board -> Board
addPiece piece board =
    List.foldl
        (\pos -> setCell pos <| Filled piece.color)
        board
        (Piece.positions piece)
