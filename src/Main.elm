module Main exposing (main)

import Browser
import ElmViewer.App exposing (Flags, Model, Msg, init, subscriptions, update, view)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
