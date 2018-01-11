module View exposing (view)

import Collage exposing (defaultLine)
import Color
import Config
import Element
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (Model)
import Types.Board as Board exposing (Board, Cell(..))
import Types.Common exposing (Color(..))
import Types.GameState as GameState exposing (GameState(..))
import Types.Piece as Piece exposing (Piece)
import Types.Pos as Pos exposing (Pos)


view : Model -> Html msg
view model =
    div [ class "game" ]
        [ div [ class "board" ]
            [ tetrisCanvas model
            , overlay model.state
            ]
        , div [ class "info" ]
            [ div [ class "title" ]
                [ text "Tetris" ]
            , div [ class "info-item status" ]
                [ text <| "score: " ++ toString (GameState.score model.state) ]
            , div [ class "info-item status" ]
                [ text <| "speed: " ++ toString model.speed ]
            , div [ class "info-item status" ]
                [ if not <| Piece.isEmpty model.nextPiece then
                    text "next piece: "
                  else
                    text ""
                ]
            , div [ class "info-item status" ]
                [ nextPieceCanvas model.nextPiece ]
            , div [ class "info-item controls" ]
                [ text "move: ⬅/➡/⬇"
                , br [] []
                , text "rotate: ⬆"
                ]
            ]
        ]


overlay : GameState -> Html msg
overlay state =
    case state of
        NotStarted ->
            div [ class "board-overlay" ]
                [ text "press SPACE to begin" ]

        GameOver _ ->
            div [ class "board-overlay" ]
                [ h1 [] [ text "GAME OVER" ]
                , text "press SPACE for new game"
                ]

        Running _ ->
            text ""


tetrisCanvas : Model -> Html msg
tetrisCanvas model =
    model
        |> renderBoard
        |> Collage.collage
            (Config.width * Config.pixelSize - Config.pixelSize // 2)
            (Config.height * Config.pixelSize - Config.pixelSize // 2)
        |> Element.toHtml


nextPieceCanvas : Piece -> Html msg
nextPieceCanvas piece =
    let
        renderBlock ( rowOffset, colOffset ) =
            Collage.square Config.pixelSize
                |> Collage.filled (cellColor <| Filled piece.color)
                |> Collage.move
                    ( toFloat colOffset * Config.pixelSize
                    , toFloat rowOffset * Config.pixelSize
                    )
    in
        piece.blocks
            |> List.map renderBlock
            |> Collage.collage
                (Config.pixelSize * 4)
                (Config.pixelSize * 4)
            |> Element.toHtml


renderBoard : Model -> List Collage.Form
renderBoard model =
    model.board
        |> Board.addPiece model.currentPiece
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
