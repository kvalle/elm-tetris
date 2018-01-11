module Update exposing (update)

import Config
import Types.Common exposing (..)
import Types.Board as Board exposing (Board, Cell(..))
import Types.Piece as Piece exposing (Piece)
import Types.GameState as GameState exposing (GameState(..))
import Messages exposing (Msg(..))
import Model exposing (Model)
import Random


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartGame ->
            ( { model
                | state = Running 0
                , board = Board.empty
              }
            , Random.generate NewPiece Piece.random
            )

        NewPiece piece ->
            ( if piece |> legalOn model.board then
                { model | piece = piece }
              else
                { model | state = GameOver <| GameState.score model.state }
            , Cmd.none
            )

        Tick ->
            let
                movedPiece =
                    Piece.move Down model.piece
            in
                if movedPiece |> legalOn model.board then
                    ( { model | piece = movedPiece }
                    , Cmd.none
                    )
                else
                    let
                        ( newBoard, numClearedRows ) =
                            model.board
                                |> Board.addPiece model.piece
                                |> Board.clearFullRows
                    in
                        ( { model
                            | board = newBoard
                            , piece = Piece.empty
                            , state = model.state |> GameState.addPoints (numClearedRows * numClearedRows)
                          }
                        , Random.generate NewPiece Piece.random
                        )

        Move direction ->
            ( { model
                | piece = moveIfPossible direction model.board model.piece
              }
            , Cmd.none
            )

        Rotate ->
            ( { model | piece = rotateWherePossible model.board model.piece }
            , Cmd.none
            )


legalOn : Board -> Piece -> Bool
legalOn board piece =
    let
        insideLeftEdge =
            List.all (\pos -> pos.col >= 0) (Piece.positions piece)

        insideRightEdge =
            List.all (\pos -> pos.col < Config.width) (Piece.positions piece)

        aboveBottom =
            List.all (\pos -> pos.row <= Config.height) (Piece.positions piece)

        collision =
            List.any (\pos -> (Board.getCell pos board) /= Just Empty) (Piece.positions piece)
    in
        insideLeftEdge && insideRightEdge && aboveBottom && not collision


moveIfPossible : Direction -> Board -> Piece -> Piece
moveIfPossible direction board piece =
    let
        movedPiece =
            Piece.move direction piece
    in
        if movedPiece |> legalOn board then
            movedPiece
        else
            piece


{-| If the piece can be legally rotated at its current positions, or at any
space adjacent position up to two spaces away, perform rotation. Otherwise
return piece as is.
-}
rotateWherePossible : Board -> Piece -> Piece
rotateWherePossible board piece =
    let
        possiblePositions =
            [ piece |> Piece.rotate
            , piece |> Piece.move Left |> Piece.rotate
            , piece |> Piece.move Right |> Piece.rotate
            , piece |> Piece.move Left |> Piece.move Left |> Piece.rotate
            , piece |> Piece.move Right |> Piece.move Right |> Piece.rotate
            ]
    in
        possiblePositions
            |> List.filter (legalOn board)
            |> List.head
            |> Maybe.withDefault piece
