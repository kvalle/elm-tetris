module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Collage exposing (defaultLine)
import Element
import Color
import Time exposing (Time)
import Keyboard
import Array
import Random exposing (Generator)
import List.Nonempty exposing (Nonempty(..))
import Array exposing (Array)


type alias Model =
    { board : Board
    , piece : Piece
    , state : GameState
    }


type alias Board =
    Array (Array Cell)


{-| Position as (col, row)
-}
type alias Pos =
    { row : Int, col : Int }


type Cell
    = Empty
    | Filled Color


type Color
    = Blue
    | Yellow
    | Red
    | Green
    | Purple


score : GameState -> Int
score gameState =
    case gameState of
        NotStarted ->
            0

        Running score ->
            score

        GameOver score ->
            score


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


width : number
width =
    15


height : number
height =
    15


type Msg
    = NoOp
    | StartGame
    | Tick
    | Move Direction
    | NewPiece Piece
    | Rotate


type alias Piece =
    { -- cell type for this piece
      color : Color
    , -- center position of piece
      pos : Pos
    , -- relative coordinates of blocks within piece
      blocks : List ( Int, Int )
    }


mapRow : (Int -> Int) -> Pos -> Pos
mapRow fn pos =
    { pos | row = fn pos.row }


mapCol : (Int -> Int) -> Pos -> Pos
mapCol fn pos =
    { pos | col = fn pos.col }


init : ( Model, Cmd Msg )
init =
    ( { board = emptyBoard
      , piece = emptyPiece
      , state = NotStarted
      }
    , Random.generate NewPiece randomPiece
    )


clearFullRows : Board -> Board
clearFullRows board =
    let
        filteredRows =
            Array.filter (Array.toList >> List.any ((==) Empty)) board

        rowsRemoved =
            Array.length board - Array.length filteredRows

        replacementRows =
            Array.repeat rowsRemoved (Array.repeat width Empty)
    in
        Array.append replacementRows filteredRows


emptyPiece : Piece
emptyPiece =
    { color = Blue, pos = Pos 0 0, blocks = [] }


pieces : Nonempty Piece
pieces =
    let
        theLongStrightOne =
            { color = Red
            , pos = { row = 1, col = width // 2 }
            , blocks = [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]
            }

        theOneThatLooksLikeL =
            { color = Yellow
            , pos = { row = 1, col = width // 2 }
            , blocks = [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 1, 1 ) ]
            }

        theBackwardsL =
            { color = Blue
            , pos = { row = 1, col = width // 2 }
            , blocks = [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 1, -1 ) ]
            }

        thePyramidThing =
            { color = Green
            , pos = { row = 1, col = width // 2 }
            , blocks = [ ( -1, 0 ), ( 0, 0 ), ( 1, 1 ), ( 1, 0 ) ]
            }
    in
        Nonempty theLongStrightOne
            [ theOneThatLooksLikeL, theBackwardsL, thePyramidThing ]


randomPiece : Generator Piece
randomPiece =
    List.Nonempty.sample pieces


emptyBoard : Board
emptyBoard =
    Array.repeat height <| Array.repeat width Empty


move : Direction -> Piece -> Piece
move direction piece =
    let
        fn =
            case direction of
                Down ->
                    mapRow <| (+) 1

                Left ->
                    mapCol <| flip (-) 1

                Right ->
                    mapCol <| (+) 1
    in
        { piece | pos = fn piece.pos }


rotate : Piece -> Piece
rotate piece =
    piece


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
            List.all (\pos -> pos.col < width) (positions piece)

        aboveBottom =
            List.all (\pos -> pos.row <= height) (positions piece)

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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartGame ->
            ( { model | state = Running 0 }
            , Cmd.none
            )

        NewPiece piece ->
            ( if legal piece model.board then
                { model | piece = piece }
              else
                { model | state = GameOver <| score model.state }
            , Cmd.none
            )

        Tick ->
            let
                movedPiece =
                    move Down model.piece
            in
                if legal movedPiece model.board then
                    ( { model | piece = movedPiece }
                    , Cmd.none
                    )
                else
                    let
                        newBoard =
                            model.board
                                |> addPiece model.piece
                                |> clearFullRows
                    in
                        ( { model
                            | board = newBoard
                            , piece = emptyPiece
                          }
                        , Random.generate NewPiece randomPiece
                        )

        Move direction ->
            ( { model
                | piece = moveIfPossible direction model.board model.piece
              }
            , Cmd.none
            )

        Rotate ->
            ( { model | piece = rotate model.piece }, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        NotStarted ->
            Keyboard.downs startGameOnSpace

        Running _ ->
            Sub.batch
                [ Time.every (Time.second * 0.5) (always Tick)
                , Keyboard.downs moveBrick
                ]

        GameOver _ ->
            Sub.none


startGameOnSpace : Keyboard.KeyCode -> Msg
startGameOnSpace keyCode =
    if keyCode == 32 then
        StartGame
    else
        NoOp


moveBrick : Keyboard.KeyCode -> Msg
moveBrick keyCode =
    case keyCode of
        37 ->
            Move Left

        39 ->
            Move Right

        40 ->
            Move Down

        _ ->
            NoOp


addPiece : Piece -> Board -> Board
addPiece piece board =
    List.foldl
        (\pos -> setCell pos <| Filled piece.color)
        board
        (positions piece)


view : Model -> Html Msg
view model =
    div [ class "game" ]
        [ div [ class "board" ]
            [ let
                pixelSize =
                    30

                cellColor cell =
                    case cell of
                        Empty ->
                            Color.white

                        Filled Blue ->
                            Color.blue

                        Filled Red ->
                            Color.red

                        Filled Green ->
                            Color.green

                        Filled Yellow ->
                            Color.yellow

                        Filled Purple ->
                            Color.purple

                toCanvasCoord : Int -> Int -> ( Float, Float )
                toCanvasCoord row col =
                    ( toFloat <| (col - (width // 2)) * pixelSize
                    , toFloat <| (height - 1 - (row + (height // 2))) * pixelSize
                    )

                board : List Collage.Form
                board =
                    model.board
                        |> addPiece model.piece
                        |> toPosList
                        |> List.map
                            (\( pos, cell ) ->
                                Collage.square pixelSize
                                    |> Collage.filled (cellColor cell)
                                    |> Collage.move (toCanvasCoord pos.row pos.col)
                            )
              in
                board
                    |> Collage.collage
                        (width * pixelSize - pixelSize // 2)
                        (height * pixelSize - pixelSize // 2)
                    |> Element.toHtml
            ]
        , div [ class "info" ]
            [ div [ class "title" ] [ text "Tetris" ]
            , div [ class "status" ]
                [ text <|
                    case model.state of
                        NotStarted ->
                            "press SPACE to begin"

                        Running _ ->
                            "⬅/➡/⬇ move, ⬆ rotate"

                        GameOver _ ->
                            "game over"
                ]
            ]
        ]
