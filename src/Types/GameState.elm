module Types.GameState exposing (GameState(..), score, addPoints)


type GameState
    = NotStarted
    | Paused Score
    | Running Score
    | GameOver Score


type alias Score =
    Int


score : GameState -> Int
score state =
    case state of
        NotStarted ->
            0

        Paused score ->
            score

        Running score ->
            score

        GameOver score ->
            score


addPoints : Int -> GameState -> GameState
addPoints n state =
    case state of
        NotStarted ->
            NotStarted

        Paused score ->
            Paused <| score + n

        Running score ->
            Running <| score + n

        GameOver score ->
            GameOver <| score + n
