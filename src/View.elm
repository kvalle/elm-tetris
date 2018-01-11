module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Collage exposing (defaultLine)
import Element
import Color
import Model exposing (Model)
import Config
import Types.Common exposing (Color(..), GameState(..))
import Types.Board as Board exposing (Board, Cell(..))
import Types.Pos as Pos exposing (Pos)


view : Model -> Html msg
view model =
    div [ class "game" ]
        [ div [ class "board" ]
            [ tetrisCanvas model ]
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


tetrisCanvas : Model -> Html msg
tetrisCanvas model =
    model
        |> renderBoard
        |> Collage.collage
            (Config.width * Config.pixelSize - Config.pixelSize // 2)
            (Config.height * Config.pixelSize - Config.pixelSize // 2)
        |> Element.toHtml


renderBoard : Model -> List Collage.Form
renderBoard model =
    model.board
        |> Board.addPiece model.piece
        |> Board.toPosList
        |> List.map (uncurry renderCell)


renderCell : Pos -> Cell -> Collage.Form
renderCell pos cell =
    Collage.square Config.pixelSize
        |> Collage.filled (cellColor cell)
        |> Collage.move (toCanvasCoord pos)


cellColor : Cell -> Color.Color
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


toCanvasCoord : Pos -> ( Float, Float )
toCanvasCoord pos =
    ( toFloat <| (pos.col - (Config.width // 2)) * Config.pixelSize
    , toFloat <| (Config.height - 1 - (pos.row + (Config.height // 2))) * Config.pixelSize
    )
