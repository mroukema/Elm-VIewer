module ElmViewer.Utils exposing
    ( Direction(..)
    , colorFromPalette
    , flip
    , getFromDict
    , getRotatedDimensions
    , msgWhenKeyOf
    , rgbPaletteColor
    , seconds
    , stepTupleList
    )

import Color as PaletteColor exposing (toRGB)
import Dict exposing (Dict)
import Element
import Json.Decode as Json
import List.Extra as List



-- Utility


type Direction
    = Forward
    | Backward


flip : (arg2 -> arg1 -> o) -> arg1 -> arg2 -> o
flip func arg1 arg2 =
    func arg2 arg1


getFromDict : Dict comparable value -> comparable -> Maybe value
getFromDict dict key =
    Dict.get key dict


seconds : Float -> Float
seconds =
    (*) 1000


stepTupleList : Direction -> ( a, List a ) -> ( a, List a )
stepTupleList direction ( head, tail ) =
    case ( tail, direction ) of
        ( [], _ ) ->
            ( head, tail )

        ( tailHead :: rest, Forward ) ->
            ( tailHead
            , List.append rest [ head ]
            )

        ( tailHead :: tailTail, Backward ) ->
            case List.unconsLast tailTail of
                Just ( last, middle ) ->
                    ( last
                    , head :: tailHead :: middle
                    )

                Nothing ->
                    ( head, tail )


getRotatedDimensions :
    Float
    -> { element | width : Float, height : Float }
    -> { width : Float, height : Float }
getRotatedDimensions rotation ({ width, height } as dims) =
    let
        radians =
            turns rotation

        s =
            abs <| sin radians

        c =
            abs <| cos radians
    in
    { width = c * width + s * height
    , height = s * width + c * height
    }



-- Colors


rgbFromTuple : ( Float, Float, Float ) -> Element.Color
rgbFromTuple ( r, g, b ) =
    Element.rgb (r / 255) (g / 255) (b / 255)


rgbPaletteColor : PaletteColor.Color -> Element.Color
rgbPaletteColor =
    PaletteColor.toRGB >> rgbFromTuple


colorFromPalette : ( PaletteColor.Color, List PaletteColor.Color ) -> Int -> Element.Color
colorFromPalette palette offset =
    let
        ( head_, colorList ) =
            case palette of
                ( head, tail ) ->
                    ( head, head :: tail )
    in
    colorList
        |> List.map rgbPaletteColor
        |> List.getAt (offset |> modBy (List.length colorList))
        |> Maybe.withDefault (head_ |> rgbPaletteColor)



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
