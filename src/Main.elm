module Main exposing (main)

import Browser
import ElmViewer.App exposing (Model, Msg, init, subscriptions, update, view)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
