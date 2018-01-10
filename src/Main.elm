module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Collage exposing (defaultLine)
import Element
import Color
import Time exposing (Time)
import Keyboard
import List.Extra
import Matrix exposing (Matrix)
import Array


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
    | Blue


type GameState
    = NotStarted
    | Running Score
    | GameOver Score


type alias Score =
    Int


width : number
width =
    50


height : number
height =
    50


type Msg
    = NoOp
    | StartGame
    | Tick
    | MoveLeft
    | MoveRight


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
      , piece =
            { cell = Blue
            , pos =
                [ { row = 1, col = width // 2 }
                , { row = 2, col = width // 2 }
                , { row = 3, col = width // 2 }
                , { row = 4, col = width // 2 }
                ]
            }
      , state = NotStarted
      }
    , Cmd.none
    )


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


moveDown : Piece -> Piece
moveDown piece =
    { piece | pos = piece.pos |> List.map (mapRow <| (+) 1) }


moveLeft : Piece -> Piece
moveLeft piece =
    { piece | pos = piece.pos |> List.map (mapCol <| flip (-) 1) }


moveRight : Piece -> Piece
moveRight piece =
    { piece | pos = piece.pos |> List.map (mapCol <| (+) 1) }


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
            List.any (\pos -> (Matrix.get pos.row pos.col board) /= Empty) piece.pos
    in
        insideLeftEdge && insideRightEdge && aboveBottom && not collision


emptyBoard : Board
emptyBoard =
    Matrix.repeat height width Empty


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartGame ->
            ( { model | state = Running 0 }
            , Cmd.none
            )

        Tick ->
            ( { model | piece = moveDown model.piece }, Cmd.none )

        MoveLeft ->
            let
                movedPiece =
                    moveLeft model.piece
            in
                ( if legal movedPiece model.board then
                    { model | piece = movedPiece }
                  else
                    model
                , Cmd.none
                )

        MoveRight ->
            let
                movedPiece =
                    moveRight model.piece
            in
                ( if legal movedPiece model.board then
                    { model | piece = movedPiece }
                  else
                    model
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
                , Keyboard.downs moveLeftRight
                ]

        GameOver _ ->
            Sub.none


startGameOnSpace : Keyboard.KeyCode -> Msg
startGameOnSpace keyCode =
    if keyCode == 32 then
        StartGame
    else
        NoOp


moveLeftRight : Keyboard.KeyCode -> Msg
moveLeftRight keyCode =
    case keyCode of
        37 ->
            MoveLeft

        39 ->
            MoveRight

        _ ->
            NoOp


composeBoard : Board -> Piece -> Board
composeBoard board piece =
    List.foldl
        (\pos -> Matrix.set pos.row pos.col Blue)
        board
        piece.pos


view : Model -> Html Msg
view model =
    div [ class "game" ]
        [ div [ class "board" ]
            [ let
                pixelSize =
                    10

                cellColor cell =
                    case cell of
                        Empty ->
                            Color.white

                        Blue ->
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
                            "⬅/➡ move, ⬆/⬇ rotate"

                        GameOver _ ->
                            "game over"
                ]
            ]
        ]
