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
    ( Int, Int )


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


type alias Piece =
    { -- cell type for this piece
      cell : Cell
    , --  position of blocks within piece
      pos : List Pos
    }


init : ( Model, Cmd Msg )
init =
    ( { board = emptyBoard
      , piece =
            { cell = Blue
            , pos =
                [ ( 1, width // 2 )
                , ( 2, width // 2 )
                , ( 3, width // 2 )
                , ( 4, width // 2 )
                ]
            }
      , state = NotStarted
      }
    , Cmd.none
    )


emptyBoard : Board
emptyBoard =
    Matrix.repeat height width Empty


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


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
    Sub.none


composeBoard : Board -> Piece -> Board
composeBoard board piece =
    List.foldl
        (\( row, col ) -> Matrix.set row col Blue)
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
