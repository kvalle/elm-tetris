module Main exposing (main)

import Html
import Model exposing (Model, init)
import View exposing (view)
import Messages exposing (Msg)
import Subscriptions exposing (subscriptions)
import Update exposing (update)


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
