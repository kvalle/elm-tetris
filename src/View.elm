module View exposing (view)

import Collage exposing (defaultLine)
import Color
import Config
import Element
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (Model)
import Types.Board as Board exposing (Board, Cell(..))
import Types.Common exposing (Color(..), Direction(..))
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
                , br [] []
                , text "drop: ENTER"
                , br [] []
                , text "pause: P"
                ]
            ]
        ]


overlay : GameState -> Html msg
overlay state =
    case state of
        NotStarted ->
            div [ class "board-overlay" ]
                [ text "press SPACE to begin" ]

        Paused _ ->
            div [ class "board-overlay" ]
                [ h1 [] [ text "PAUSED" ]
                , text "press R to resume"
                , br [] []
                , br [] []
                , text "press N for new game"
                ]

        GameOver _ ->
            div [ class "board-overlay board-overlay--transparent" ]
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
                    , toFloat rowOffset * Config.pixelSize * -1
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
    let
        board =
            model.board
                |> Board.toPosList
                |> List.map (uncurry renderCell)

        piece =
            model.currentPiece
                |> Piece.positions
                |> List.map (\pos -> ( pos, Filled model.currentPiece.color ))
                |> List.map (uncurry renderCell)

        shadow =
            if Piece.isEmpty model.currentPiece then
                []
            else
                model.currentPiece
                    |> Board.moveToBottom model.board
                    |> Piece.positions
                    |> List.map (\pos -> ( pos, Filled model.currentPiece.color ))
                    |> List.map (uncurry renderCell)
                    |> List.map (Collage.alpha 0.1)
    in
        board ++ shadow ++ piece


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

        Filled Gray ->
            Color.darkGray

        Filled Orange ->
            Color.orange


toCanvasCoord : Pos -> ( Float, Float )
toCanvasCoord pos =
    ( toFloat <| (pos.col - (Config.width // 2)) * Config.pixelSize
    , toFloat <| (Config.height - 1 - (pos.row + (Config.height // 2))) * Config.pixelSize
    )
