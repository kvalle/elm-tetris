module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Collage exposing (defaultLine)
import Element
import Color
import Model exposing (Model)
import Config
import Types exposing (Color(..), Cell(..), GameState(..))


view : Model -> Html msg
view model =
    div [ class "game" ]
        [ div [ class "board" ]
            [ let
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
                    ( toFloat <| (col - (Config.width // 2)) * Config.pixelSize
                    , toFloat <| (Config.height - 1 - (row + (Config.height // 2))) * Config.pixelSize
                    )

                board : List Collage.Form
                board =
                    model.board
                        |> Types.addPiece model.piece
                        |> Types.toPosList
                        |> List.map
                            (\( pos, cell ) ->
                                Collage.square Config.pixelSize
                                    |> Collage.filled (cellColor cell)
                                    |> Collage.move (toCanvasCoord pos.row pos.col)
                            )
              in
                board
                    |> Collage.collage
                        (Config.width * Config.pixelSize - Config.pixelSize // 2)
                        (Config.height * Config.pixelSize - Config.pixelSize // 2)
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
