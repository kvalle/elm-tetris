module Update exposing (update)

import Config
import Types.Common exposing (..)
import Types.Board as Board exposing (Board, Cell(..))
import Types.Piece as Piece exposing (Piece)
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
            ( if legal piece model.board then
                { model | piece = piece }
              else
                { model | state = GameOver <| Model.score model }
            , Cmd.none
            )

        Tick ->
            let
                movedPiece =
                    Piece.move Down model.piece
            in
                if legal movedPiece model.board then
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
                            , state = model.state |> Model.addPoints (numClearedRows * numClearedRows)
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
            ( { model | piece = Piece.rotate model.piece }, Cmd.none )


legal : Piece -> Board -> Bool
legal piece board =
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
        if legal movedPiece board then
            movedPiece
        else
            piece
