module Types.GameState exposing (GameState(..), score, addPoints)


type GameState
    = NotStarted
    | Running Score
    | GameOver Score


type alias Score =
    Int


score : GameState -> Int
score state =
    case state of
        NotStarted ->
            0

        Running score ->
            score

        GameOver score ->
            score


addPoints : Int -> GameState -> GameState
addPoints n state =
    case state of
        NotStarted ->
            NotStarted

        Running score ->
            Running <| score + n

        GameOver score ->
            GameOver <| score + n
