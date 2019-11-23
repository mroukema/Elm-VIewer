module Main exposing (main)

import Basics exposing (not, pi)
import Browser
import Browser.Events as Browser
import Color as Cubehelix exposing (toRGB)
import Dict exposing (Dict)
import Element exposing (Element, fill, height, px, rgb, rgba255, width)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Html as Html exposing (Html)
import Html.Attributes exposing (src, style)
import Html.Events exposing (keyCode, on)
import Json.Decode as Json
import List.Extra as List
import Palette.Cubehelix as Cubehelix
import Task
import Time
import Url exposing (Url)
import Utils exposing (Direction(..), flip, getFromDict, isEsc, isNavKey, isSpace, msgWhen, rgb255, rgbTuple)



-- Init


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Dict.empty defaultPreferences PreviewView, Cmd.none )


defaultPreferences : Preferences
defaultPreferences =
    { slideshowSpeed = 3 * 1000
    , previewItemsPerRow = 8
    , backgroundColor = List.getAt 1 (List.map (toRGB >> (\( r, g, b ) -> rgb255 r g b)) colorPalette) |> Maybe.withDefault (rgb 0 0 0)
    }


colorPalette : List Cubehelix.Color
colorPalette =
    Cubehelix.generate 20



-- Model


type Model
    = Model Data Preferences ViewState


type ViewModel
    = ImageList
        (List ( ImageKey, ImageUrl ))
        { imagesPerRow : Int
        , backgroundColor : Element.Color
        }
    | Slideshow ImageUrl { backgroundColor : Element.Color }
    | EditPreferences Preferences


type ViewState
    = SlideshowView SlideshowState
    | PreviewView
    | PreferencesView


type alias SlideshowState =
    { running : Bool, slidelist : List ImageKey }


type alias Data =
    Dict ImageKey ImageUrl


type alias Preferences =
    { slideshowSpeed : Float
    , previewItemsPerRow : Int
    , backgroundColor : Element.Color
    }


type alias ImageKey =
    String


type alias ImageUrl =
    String



-- Msg


type Msg
    = NoOp
    | OpenImagePicker
    | FilesReceived File (List File)
    | InsertImage String (Result () ImageUrl)
    | UpdateView ViewState
    | UpdatePreferences Preferences
    | RemoveImage ImageKey


insertImageFromFile : File -> Cmd Msg
insertImageFromFile file =
    Task.attempt
        (InsertImage (File.name file))
        (File.toUrl file)


updateSlideshow : SlideshowState -> Msg
updateSlideshow state =
    UpdateView <| SlideshowView state


startSlideshow : List ImageKey -> Msg
startSlideshow slides =
    updateSlideshow { running = True, slidelist = slides }


togglePauseSlideshow : SlideshowState -> Msg
togglePauseSlideshow state =
    updateSlideshow (toggleRunning state)


stepSlideshow : SlideshowState -> Direction -> Msg
stepSlideshow state direction =
    let
        stepList list =
            case list of
                head :: tail ->
                    List.append tail [ head ]

                [] ->
                    []
    in
    case direction of
        Forward ->
            { state | slidelist = stepList state.slidelist }
                |> SlideshowView
                |> UpdateView

        Backward ->
            { state | slidelist = (List.reverse << stepList << List.reverse) state.slidelist }
                |> SlideshowView
                |> UpdateView



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        NoOp ->
            noop

        OpenImagePicker ->
            ( model, Select.files [ "image/png", "image/jpg" ] FilesReceived )

        FilesReceived file otherFiles ->
            ( model
            , List.map insertImageFromFile (file :: otherFiles) |> Cmd.batch
            )

        InsertImage filename result ->
            case result of
                Ok imageUrl ->
                    case model of
                        Model images preferences state ->
                            ( Model (Dict.insert filename imageUrl images) preferences state, Cmd.none )

                Err _ ->
                    noop

        UpdatePreferences preferences ->
            case model of
                Model data _ state ->
                    ( Model data preferences state, Cmd.none )

        UpdateView newState ->
            case model of
                Model data preferences _ ->
                    ( Model data preferences newState, Cmd.none )

        RemoveImage imageKey ->
            case model of
                Model data preferences state ->
                    ( Model (Dict.remove imageKey data) preferences state, Cmd.none )


toggleRunning : { r | running : Bool } -> { r | running : Bool }
toggleRunning state =
    { state | running = (not << .running) state }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Model data { slideshowSpeed } state ->
            case state of
                SlideshowView currentState ->
                    let
                        { running, slidelist } =
                            currentState

                        navigationListeners =
                            [ Browser.onKeyPress <| msgWhen isSpace (always <| togglePauseSlideshow currentState)
                            , Browser.onKeyUp <| msgWhen isNavKey (stepSlideshow currentState)
                            , Browser.onKeyUp <| msgWhen isEsc (always <| UpdateView PreviewView)
                            ]
                    in
                    case running of
                        True ->
                            Sub.batch
                                (Time.every slideshowSpeed (always <| stepSlideshow currentState Forward)
                                    :: navigationListeners
                                )

                        False ->
                            Sub.batch navigationListeners

                PreviewView ->
                    Browser.onKeyPress <|
                        msgWhen isSpace
                            (always <| startSlideshow <| List.sort <| Dict.keys data)

                PreferencesView ->
                    Browser.onKeyUp <| msgWhen isEsc (always <| UpdateView PreviewView)



-- View


view : Model -> Html Msg
view =
    viewSelector >> imageViewer


viewSelector : Model -> ViewModel
viewSelector model =
    case model of
        Model data preferences viewState ->
            let
                imageList =
                    Dict.toList data
            in
            case viewState of
                PreferencesView ->
                    EditPreferences preferences

                PreviewView ->
                    ImageList imageList
                        { imagesPerRow = preferences.previewItemsPerRow, backgroundColor = preferences.backgroundColor }

                SlideshowView { running, slidelist } ->
                    case List.filterMap (getFromDict data) slidelist of
                        [] ->
                            ImageList imageList
                                { imagesPerRow = preferences.previewItemsPerRow
                                , backgroundColor = preferences.backgroundColor
                                }

                        firstImage :: images ->
                            Slideshow firstImage { backgroundColor = preferences.backgroundColor }


imageHeader model =
    let
        headerBackground =
            colorPalette |> List.map (toRGB >> rgbTuple) |> List.getAt 3 |> Maybe.withDefault (rgb255 0 0 0)

        fontColor =
            colorPalette |> List.map (toRGB >> rgbTuple) |> List.last |> Maybe.withDefault (rgb255 0 0 0)
    in
    case model of
        EditPreferences _ ->
            Element.row [ width fill, Background.color <| headerBackground, Element.spaceEvenly, Element.padding 5 ]
                [ Element.text ""
                , Element.text "Preview View"
                    |> Element.el [ onClick <| UpdateView PreviewView, Font.color fontColor ]
                , Element.text "Select Images"
                    |> Element.el [ onClick OpenImagePicker, Font.color fontColor ]
                ]

        ImageList imageUrls _ ->
            Element.row [ width fill, Background.color <| headerBackground, Element.spaceEvenly, Element.padding 5 ]
                [ Element.text "Start Slideshow"
                    |> Element.el [ onClick <| startSlideshow <| List.sort <| List.map Tuple.first imageUrls, Font.color fontColor ]
                , Element.text "Preferences"
                    |> Element.el [ onClick <| UpdateView PreferencesView, Font.color fontColor ]
                , Element.text "Select Images"
                    |> Element.el [ onClick OpenImagePicker, Font.color fontColor ]
                ]

        _ ->
            Element.row [ width fill, Background.color <| rgba255 220 220 220 0.5, Element.spaceEvenly, Element.padding 5 ]
                [ Element.text ""
                , Element.text "Preferences" |> Element.el [ onClick <| UpdateView PreferencesView, Font.color fontColor ]
                , Element.text "Select Images" |> Element.el [ onClick OpenImagePicker, Font.color fontColor ]
                ]


imageViewer : ViewModel -> Html Msg
imageViewer model =
    let
        content =
            case model of
                ImageList images preferences ->
                    Element.column [ width fill, height fill ]
                        [ imageHeader model
                        , filePreviewView images preferences
                        ]

                Slideshow currentImage { backgroundColor } ->
                    slideshowView currentImage backgroundColor

                EditPreferences preferences ->
                    Element.column [ width fill, height fill ]
                        [ imageHeader model
                        , editPreferencesView preferences
                        ]
    in
    content
        |> Element.layout [ height fill, width fill ]


colorPicker updateMsg =
    colorPalette
        |> List.map
            (toRGB
                >> (\( r, g, b ) -> rgb255 r g b)
                >> (\rgbColor -> colorPickerBox rgbColor updateMsg)
            )


colorPickerBox color colorChangeMsg =
    Element.el [ Background.color color, width fill, height fill, onClick (colorChangeMsg color) ] <| Element.column [] []


editPreferencesView : Preferences -> Element Msg
editPreferencesView ({ slideshowSpeed, backgroundColor, previewItemsPerRow } as preferences) =
    Element.el [ width fill, height fill, Background.color backgroundColor, Element.spacing 50, Element.padding 20 ] <|
        Element.column [ width Element.fill, Element.padding 35, Background.color <| rgba255 0 0 0 0.5, Element.spacing 10 ]
            [ Input.slider
                [ width <| Element.fillPortion 4
                , Element.behindContent <|
                    Element.el [ Background.color <| rgba255 255 255 255 1, height (5 |> px), width fill, Element.centerY ] Element.none
                ]
                { onChange = \newSpeed -> UpdatePreferences { preferences | slideshowSpeed = newSpeed }
                , label =
                    Input.labelLeft
                        [ Font.color <| rgba255 250 250 250 1.0, width <| Element.fillPortion 1 ]
                        (Element.text
                            ("Slideshow Speed = "
                                ++ String.fromFloat
                                    (((slideshowSpeed / 100) |> round |> toFloat) / 10)
                                ++ "s"
                            )
                        )
                , min = 100
                , max = 60 * 1000
                , value = slideshowSpeed
                , thumb = Input.defaultThumb
                , step = Nothing
                }
            , Input.slider
                [ width <| Element.fillPortion 4
                , Element.behindContent <|
                    Element.el [ Background.color <| rgba255 255 255 255 1, height (5 |> px), width fill, Element.centerY ] Element.none
                ]
                { onChange = round >> (\newCount -> UpdatePreferences { preferences | previewItemsPerRow = newCount })
                , label =
                    Input.labelLeft
                        [ Font.color <| rgba255 250 250 250 1.0, width <| Element.fillPortion 1 ]
                        (Element.text
                            ("Images per Row = " ++ String.fromInt previewItemsPerRow)
                        )
                , min = 1
                , max = 10
                , value = previewItemsPerRow |> toFloat
                , thumb = Input.defaultThumb
                , step = Just 1
                }
            , Element.row [ width fill, height (20 |> px), Font.color <| Element.rgb 1 1 1 ]
                [ Element.el [ width <| Element.fillPortion 1 ] <| Element.text "Background Color"
                , Element.row
                    [ width <| Element.fillPortion 4, height (20 |> px) ]
                    (colorPicker (\newColor -> UpdatePreferences { preferences | backgroundColor = newColor }))
                ]
            ]


filePreviewView : List ( ImageKey, ImageUrl ) -> { r | imagesPerRow : Int, backgroundColor : Element.Color } -> Element Msg
filePreviewView images { imagesPerRow, backgroundColor } =
    List.greedyGroupsOf imagesPerRow images
        |> List.map
            (\group ->
                let
                    elementDeficit =
                        imagesPerRow - List.length group
                in
                group
                    |> List.map singleFileView
                    |> flip List.append
                        (List.repeat
                            elementDeficit
                            (Element.el [ width fill, height fill ] Element.none)
                        )
                    |> Element.row
                        [ Element.spaceEvenly
                        , Element.spacing 5
                        , width fill
                        ]
            )
        |> Element.column
            [ width fill
            , height fill
            , Background.color backgroundColor
            , Element.spacing 5
            , Element.padding 5
            ]


{-| slideshowView
Use of Html.img due ot Element.img not respecting parent height with base64 encoded image
-}
slideshowView : String -> Element.Color -> Element Msg
slideshowView imageUrl backgroundColor =
    let
        url =
            imageUrl
    in
    Element.el
        [ Element.clip
        , width fill
        , height fill
        , Background.color backgroundColor
        ]
        (Element.html
            (Html.img
                --  Black CSS Magic to make image fit within bounds at normal aspect ratio
                [ src url
                , style "position" "absolute"
                , style "object-fit" "contain"
                , style "height" "100%"
                , style "width" "100%"
                , style "max-height" "100%"
                , style "max-width" "100%"
                ]
                []
            )
        )


singleFileView ( imageKey, imageSrc ) =
    Element.image
        [ width fill
        , height fill
        , Element.centerX
        , Element.centerY
        , onClick <| RemoveImage imageKey
        ]
        { src = imageSrc
        , description = ""
        }
