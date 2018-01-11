module Types.Common exposing (GameState(..), Direction(..), Color(..))


type GameState
    = NotStarted
    | Running Score
    | GameOver Score


type alias Score =
    Int


type Direction
    = Down
    | Left
    | Right


type Color
    = Blue
    | Yellow
    | Red
    | Green
    | Purple
