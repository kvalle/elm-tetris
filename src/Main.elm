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
    | NewGame


type alias Piece =
    { -- absolute position of piece
      pos : Pos
    , -- relative position of blocks within piece
      cells : List Pos
    }


init : ( Model, Cmd Msg )
init =
    ( { board = emptyBoard
      , piece =
            { pos = ( 0, width // 2 )
            , cells = [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 3, 0 ) ]
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

        NewGame ->
            init


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
    board


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
            , div [ class "new-game" ]
                [ button
                    [ onClick NewGame
                    , disabled <|
                        case model.state of
                            GameOver _ ->
                                False

                            _ ->
                                True
                    ]
                    [ text "new game" ]
                ]
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
