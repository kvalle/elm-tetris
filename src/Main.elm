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


type alias Model =
    { board : Board
    , piece : Piece
    , state : GameState
    }


type alias Board =
    List (List Cell)


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
    { pos : ( Int, Int )
    , cells : List ( Int, Int )
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


emptyBoard : List (List Cell)
emptyBoard =
    List.repeat height (List.repeat width Empty)


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
        |> List.Extra.setAt 4
            ([ Blue, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ])
        |> List.Extra.setAt 5
            ([ Blue, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ])
        |> List.Extra.setAt 6
            ([ Blue, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ])
        |> List.Extra.setAt 7
            ([ Blue, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ])


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

                board =
                    composeBoard model.board model.piece
                        |> List.indexedMap
                            (\row cells ->
                                cells
                                    |> List.indexedMap
                                        (\col cell ->
                                            Collage.square pixelSize
                                                |> Collage.filled (cellColor cell)
                                                |> Collage.move (toCanvasCoord row col)
                                        )
                            )
                        |> List.concat
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
