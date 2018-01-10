module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Collage exposing (defaultLine)
import Element
import Color
import Time exposing (Time)
import Keyboard
import Matrix exposing (Matrix)
import Array
import Task exposing (Task)


type alias Model =
    { board : Board
    , piece : Piece
    , state : GameState
    }


type alias Board =
    Matrix Cell


{-| Position as (col, row)
-}
type alias Pos =
    { row : Int, col : Int }


type Cell
    = Empty
    | Filled Color


type Color
    = Blue


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


type alias Piece =
    { -- cell type for this piece
      cell : Cell
    , --  position of blocks within piece
      pos : List Pos
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
      , piece = initPiece
      , state = NotStarted
      }
    , Cmd.none
    )


initPiece : Piece
initPiece =
    { cell = Filled Blue
    , pos =
        [ { row = 1, col = width // 2 }
        , { row = 2, col = width // 2 }
        , { row = 3, col = width // 2 }
        , { row = 4, col = width // 2 }
        ]
    }


emptyPiece : Piece
emptyPiece =
    { cell = Empty, pos = [] }


nextPiece : Task Never Piece
nextPiece =
    Task.succeed initPiece


emptyBoard : Board
emptyBoard =
    Matrix.repeat height width Empty


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
        { piece | pos = piece.pos |> List.map fn }


legal : Piece -> Board -> Bool
legal piece board =
    let
        insideLeftEdge =
            List.all (\pos -> pos.col >= 0) piece.pos

        insideRightEdge =
            List.all (\pos -> pos.col < width) piece.pos

        aboveBottom =
            List.all (\pos -> pos.row <= height) piece.pos

        collision =
            List.any (\pos -> (Matrix.get pos.row pos.col board) /= Just Empty) piece.pos
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
            ( { model | piece = piece }
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
                    ( { model
                        | board = composeBoard model.board model.piece
                        , piece = emptyPiece
                      }
                    , Task.perform NewPiece nextPiece
                    )

        Move direction ->
            ( { model
                | piece = moveIfPossible direction model.board model.piece
              }
            , Cmd.none
            )


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


composeBoard : Board -> Piece -> Board
composeBoard board piece =
    List.foldl
        (\pos -> Matrix.set pos.row pos.col <| Filled Blue)
        board
        piece.pos


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

                toCanvasCoord : Int -> Int -> ( Float, Float )
                toCanvasCoord row col =
                    ( toFloat <| (col - (width // 2)) * pixelSize
                    , toFloat <| (height - (row + (height // 2))) * pixelSize
                    )

                board : List Collage.Form
                board =
                    composeBoard model.board model.piece
                        |> Matrix.toIndexedArray
                        |> Array.toList
                        |> List.map
                            (\( ( row, col ), cell ) ->
                                Collage.square pixelSize
                                    |> Collage.filled (cellColor cell)
                                    |> Collage.move (toCanvasCoord row col)
                            )
              in
                board
                    |> Collage.collage
                        (width * pixelSize + pixelSize // 2)
                        (height * pixelSize + pixelSize // 2)
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
