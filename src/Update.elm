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

        NewGame currentPiece nextPiece ->
            ( { model
                | state = Running 0
                , speed = 1
                , board = Board.empty
                , currentPiece = currentPiece
                , nextPiece = nextPiece
              }
            , Cmd.none
            )

        Drop ->
            ( { model
                | currentPiece = Board.moveToBottom model.board model.currentPiece
              }
            , Cmd.none
            )

        StartNewGame ->
            ( model
            , Random.generate identity <|
                Random.map2 NewGame
                    Piece.random
                    Piece.random
            )

        NewPiece piece ->
            ( if piece |> Board.legalOn model.board then
                { model
                    | currentPiece = model.nextPiece
                    , nextPiece = piece
                }
              else
                { model
                    | state = GameOver <| GameState.score model.state
                }
            , Cmd.none
            )

        Tick ->
            let
                movedPiece =
                    Piece.move Down model.currentPiece
            in
                if movedPiece |> Board.legalOn model.board then
                    ( { model | currentPiece = movedPiece }
                    , Cmd.none
                    )
                else
                    let
                        ( newBoard, numClearedRows ) =
                            model.board
                                |> Board.addPiece model.currentPiece
                                |> Board.clearFullRows

                        newSpeed =
                            if numClearedRows > 0 then
                                model.speed + 1
                            else
                                model.speed
                    in
                        ( { model
                            | board = newBoard
                            , currentPiece = Piece.empty
                            , state = model.state |> GameState.addPoints (numClearedRows * numClearedRows)
                            , speed = newSpeed
                          }
                        , Random.generate NewPiece Piece.random
                        )

        Move direction ->
            ( { model
                | currentPiece = moveIfPossible direction model.board model.currentPiece
              }
            , Cmd.none
            )

        Rotate ->
            ( { model
                | currentPiece = rotateWherePossible model.board model.currentPiece
              }
            , Cmd.none
            )


moveIfPossible : Direction -> Board -> Piece -> Piece
moveIfPossible direction board piece =
    let
        movedPiece =
            Piece.move direction piece
    in
        if movedPiece |> Board.legalOn board then
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
            |> List.filter (Board.legalOn board)
            |> List.head
            |> Maybe.withDefault piece
