module Types exposing (..)

import Array exposing (Array)
import Config
import List.Nonempty exposing (Nonempty(..))
import Random exposing (Generator)
import Types.Pos as Pos exposing (Pos)


type alias Board =
    Array (Array Cell)


score : GameState -> Int
score gameState =
    case gameState of
        NotStarted ->
            0

        Running score ->
            score

        GameOver score ->
            score


clearFullRows : Board -> Board
clearFullRows board =
    let
        filteredRows =
            Array.filter (Array.toList >> List.any ((==) Empty)) board

        rowsRemoved =
            Array.length board - Array.length filteredRows

        replacementRows =
            Array.repeat rowsRemoved (Array.repeat Config.width Empty)
    in
        Array.append replacementRows filteredRows


emptyBoard : Board
emptyBoard =
    Array.repeat Config.height <| Array.repeat Config.width Empty


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


positions : Piece -> List Pos
positions piece =
    piece.blocks
        |> List.map (Tuple.mapFirst <| (+) piece.pos.row)
        |> List.map (Tuple.mapSecond <| (+) piece.pos.col)
        |> List.map (uncurry Pos)


legal : Piece -> Board -> Bool
legal piece board =
    let
        insideLeftEdge =
            List.all (\pos -> pos.col >= 0) (positions piece)

        insideRightEdge =
            List.all (\pos -> pos.col < Config.width) (positions piece)

        aboveBottom =
            List.all (\pos -> pos.row <= Config.height) (positions piece)

        collision =
            List.any (\pos -> (getCell pos board) /= Just Empty) (positions piece)
    in
        insideLeftEdge && insideRightEdge && aboveBottom && not collision


moveIfPossible : Direction -> Board -> Piece -> Piece
moveIfPossible direction board piece =
    let
        movedPiece =
            move direction piece
    in
        if legal movedPiece board then
            movedPiece
        else
            piece


addPiece : Piece -> Board -> Board
addPiece piece board =
    List.foldl
        (\pos -> setCell pos <| Filled piece.color)
        board
        (positions piece)


type GameState
    = NotStarted
    | Running Score
    | GameOver Score


type Direction
    = Down
    | Left
    | Right


type alias Score =
    Int


type Cell
    = Empty
    | Filled Color


type Color
    = Blue
    | Yellow
    | Red
    | Green
    | Purple


type alias Piece =
    { -- cell type for this piece
      color : Color
    , -- center position of piece
      pos : Pos
    , -- relative coordinates of blocks within piece
      blocks : List ( Int, Int )
    }


emptyPiece : Piece
emptyPiece =
    { color = Blue, pos = Pos 0 0, blocks = [] }


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


randomPiece : Generator Piece
randomPiece =
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
