module ElmViewer.Utils exposing
    ( Direction(..)
    , flip
    , getFromDict
    , isArrowDown
    , isArrowUp
    , isEsc
    , isNavKey
    , isSpace
    , msgWhen
    , rgbPaletteColor
    , seconds
    )

import Color as PaletteColor exposing (toRGB)
import Dict exposing (Dict)
import Element
import Json.Decode as Json



-- Direction


type Direction
    = Forward
    | Backward


toDirection : String -> Maybe Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            Just Backward

        "ArrowRight" ->
            Just Forward

        _ ->
            Nothing



-- Utility


flip : (arg2 -> arg1 -> o) -> arg1 -> arg2 -> o
flip func arg1 arg2 =
    func arg2 arg1


getFromDict : Dict comparable value -> comparable -> Maybe value
getFromDict dict key =
    Dict.get key dict


seconds =
    (*) 1000



-- Colors


rgbFromTuple : ( Float, Float, Float ) -> Element.Color
rgbFromTuple ( r, g, b ) =
    Element.rgb (r / 255) (g / 255) (b / 255)


rgbPaletteColor =
    PaletteColor.toRGB >> rgbFromTuple



-- Decoders


msgWhen : List String -> (String -> msg) -> Json.Decoder msg
msgWhen keys msg =
    Json.map msg (Json.field "key" Json.string |> Json.andThen (keyMatchDecoder keys))


keyMatchDecoder : List String -> String -> Json.Decoder String
keyMatchDecoder stringList key =
    case List.member key stringList of
        True ->
            Json.succeed key

        False ->
            Json.fail key



--
-- msgWhen : (String -> Json.Decoder b) -> (b -> msg) -> Json.Decoder msg
-- msgWhen keyDecoder msg =
--     Json.map msg (Json.field "key" Json.string |> Json.andThen keyDecoder)


isEsc : String -> Json.Decoder String
isEsc string =
    case string of
        "Escape" ->
            Json.succeed string

        _ ->
            Json.fail "Not the Escape Key"


isArrowDown : String -> Json.Decoder String
isArrowDown string =
    case string of
        "ArrowDown" ->
            Json.succeed string

        _ ->
            Json.fail "Not the Down Key"


isArrowUp : String -> Json.Decoder String
isArrowUp string =
    case string of
        "ArrowUp" ->
            Json.succeed string

        _ ->
            Json.fail "Not the Down Key"


isNavKey : String -> Json.Decoder Direction
isNavKey string =
    case toDirection string of
        Just direction ->
            Json.succeed direction

        Nothing ->
            Json.fail "Not a Direction Key"


isSpace : String -> Json.Decoder String
isSpace string =
    case string of
        " " ->
            Json.succeed " "

        _ ->
            Json.fail "Not the Space Key"
