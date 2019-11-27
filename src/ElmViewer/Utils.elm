module ElmViewer.Utils exposing
    ( flip
    , getFromDict
    , msgWhenKeyOf
    , rgbPaletteColor
    , seconds
    )

import Color as PaletteColor exposing (toRGB)
import Dict exposing (Dict)
import Element
import Json.Decode as Json



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


msgWhenKeyOf : List String -> (String -> msg) -> Json.Decoder msg
msgWhenKeyOf keys msg =
    Json.map msg (Json.field "key" Json.string |> Json.andThen (keyMatchDecoder keys))


keyMatchDecoder : List String -> String -> Json.Decoder String
keyMatchDecoder stringList key =
    case List.member key stringList of
        True ->
            Json.succeed key

        False ->
            Json.fail key
