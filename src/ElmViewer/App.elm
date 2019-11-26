module ElmViewer.App exposing (Model, Msg, init, subscriptions, update, view)

import Basics exposing (identity, not, pi)
import Browser
import Browser.Events as Browser
import Color as Color exposing (Color, toRGB)
import Dict exposing (Dict)
import Element
    exposing
        ( Element
        , alignBottom
        , alignRight
        , alignTop
        , fill
        , height
        , inFront
        , px
        , rgb
        , rgba255
        , text
        , width
        )
import Element.Background as Background
import Element.Events exposing (onClick, onFocus)
import Element.Font as Font
import Element.Input as Input
import ElmViewer.Utils
    exposing
        ( flip
        , getFromDict
        , msgWhen
        , rgbPaletteColor
        , seconds
        )
import FeatherIcons as Icon
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html as Html exposing (Html)
import Html.Attributes exposing (src, style)
import Html.Events exposing (keyCode, on)
import Json.Decode as Json
import Json.Encode as Encode
import List.Extra as List
import Palette.Cubehelix as Cubehelix
import Svg
import Svg.Attributes as Svg
import Task
import Time
import Url exposing (Url)


{-| Image viewing and slideshow program for local files
-}



-- Init


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Dict.empty defaultPreferences previewCatalogState, Cmd.none )


defaultPreferences : Preferences
defaultPreferences =
    { slideshowSpeed = 3 |> seconds
    , previewItemsPerRow = 4
    , backgroundColor = defaultBackground
    , keyboardControls = defaultKeyboardMappings
    }


defaultBackground : Color
defaultBackground =
    case colorPalette of
        ( head, tail ) ->
            tail
                |> List.getAt 1
                |> Maybe.withDefault head


colorPalette : ( Color, List Color )
colorPalette =
    case Cubehelix.generate 20 of
        head :: tail ->
            ( head, tail )

        [] ->
            ( Color.fromRGB ( 1, 1, 1 ), [] )


previewCatalogState : ViewState
previewCatalogState =
    Preview Catalog


defaultSlideshowMap =
    { next = [ "ArrowLeft" ]
    , prev = [ "ArrowRight" ]
    , exit = [ "Escape", "ArrowDown" ]
    , toggle = [ " " ]
    }


defaultPreferencesMap =
    { exit = [ "Escape", "ArrowDown" ]
    }


defaultPreviewMap =
    { openCurrent = [ "ArrowUp" ]
    , closeCurrent = [ "ArrowDown", "Escape" ]
    , startSlideshow = [ " " ]
    }


defaultKeyboardMappings : KeyboardMappings
defaultKeyboardMappings =
    { slideshowMap = defaultSlideshowMap
    , preferencesMap = defaultPreferencesMap
    , previewMap = defaultPreviewMap
    }



-- Model


{-| Model
The persisted model of the application.

We make a distinction between application data `Data`, configurable's `Preferences`,
and view data `ViewState`

  - `Data` The data that drives our application; images and their organization
  - `Preferences` The data that controls behaviors and appearances of our application
  - `ViewState` The data pertaining to what we are currently showing

-}
type Model
    = Model Data Preferences ViewState


{-| ViewModel
ViewModel is the set of data, derived from Model, needed to render a particular scene.

Data will be derived whenever `view` executes. This is accomplished composing a
view selector and view renderer.

`view = viewSelector >> renderView`

  - `viewSelector` will select raw data from our Model and perform any transformations required
    to produce the information needed for current scene.
  - `renderView` does the job of generating the html based on the provided scene data.

This allows clean separation of underlying model from concerns of particular views.

Note: memoization makes this process more efficient than it may appear\_

-}
type ViewModel
    = PreviewView
        (List ( ImageKey, ImageUrl ))
        (Maybe ( ImageKey, ImageUrl ))
        { imagesPerRow : Int
        , backgroundColor : Element.Color
        }
    | SlideshowView ImageUrl { backgroundColor : Element.Color }
    | SettingsView Preferences


{-| ViewState
ViewState is the persistence of what both what the current view is any
state data used by that scene.
-}
type ViewState
    = Slideshow SlideshowState
    | Preview PreviewState
    | Settings


{-| SlideshowState
Data that needs to be persisted when in the slideshow scene

Note: persisted data for scene not data required to render the scene

-}
type alias SlideshowState =
    { running : Bool, slidelist : List ImageKey }


type PreviewState
    = Catalog
    | Focused FocusedImage


type alias FocusedImage =
    ImageKey


type alias Data =
    Dict ImageKey ImageUrl


type alias Preferences =
    { slideshowSpeed : Float
    , previewItemsPerRow : Int
    , backgroundColor : Color
    , keyboardControls : KeyboardMappings
    }


type alias ImageKey =
    String


type alias ImageUrl =
    String


type alias KeyboardMappings =
    { slideshowMap : SlideshowMap
    , preferencesMap : PreferencesMap
    , previewMap : PreviewMap
    }


type alias SlideshowMap =
    { next : List String
    , prev : List String
    , exit : List String
    , toggle : List String
    }


type alias PreferencesMap =
    { exit : List String
    }


type alias PreviewMap =
    { openCurrent : List String
    , closeCurrent : List String
    , startSlideshow : List String
    }


type Direction
    = Forward
    | Backward



-- Msg


type Msg
    = OpenImagePicker
    | FilesReceived File (List File)
    | InsertImage ImageKey (Result () ImageUrl)
    | RemoveImage ImageKey
    | UpdateView ViewState
    | UpdatePreferences Preferences
    | SaveCatalog
    | LoadCatalog
    | CatalogFileReceived File
    | CatalogDecoded (Result () ImageKey)


updateSlideshow : SlideshowState -> Msg
updateSlideshow state =
    UpdateView <| Slideshow state


startSlideshow : List ImageKey -> Msg
startSlideshow slides =
    updateSlideshow { running = True, slidelist = slides }


togglePauseSlideshow : SlideshowState -> Msg
togglePauseSlideshow state =
    updateSlideshow <| toggleRunning state


toggleRunning : { r | running : Bool } -> { r | running : Bool }
toggleRunning state =
    { state | running = (not << .running) state }


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
                |> Slideshow
                |> UpdateView

        Backward ->
            { state | slidelist = (List.reverse << stepList << List.reverse) state.slidelist }
                |> Slideshow
                |> UpdateView



-- Commands


insertImageFromFile : File -> Cmd Msg
insertImageFromFile file =
    Task.attempt
        (InsertImage (File.name file))
        (File.toUrl file)


loadCatalog : Cmd Msg
loadCatalog =
    Select.file [ "application/json" ] CatalogFileReceived


saveCatalog : Data -> Preferences -> Cmd Msg
saveCatalog data preferences =
    Download.string "imagerState.json" "application/json" (encodeSaveData data preferences)



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenImagePicker ->
            ( model, Select.files [ "image/png", "image/jpg" ] FilesReceived )

        FilesReceived file otherFiles ->
            ( model
            , List.map insertImageFromFile (file :: otherFiles) |> Cmd.batch
            )

        InsertImage filename (Ok imageUrl) ->
            case model of
                Model images preferences state ->
                    ( Model (Dict.insert filename imageUrl images) preferences state, Cmd.none )

        InsertImage filename (Err _) ->
            ( model, Cmd.none )

        RemoveImage imageKey ->
            case model of
                Model data preferences state ->
                    ( Model (Dict.remove imageKey data) preferences state, Cmd.none )

        UpdatePreferences preferences ->
            case model of
                Model data _ state ->
                    ( Model data preferences state, Cmd.none )

        UpdateView newState ->
            case model of
                Model data preferences _ ->
                    ( Model data preferences newState, Cmd.none )

        SaveCatalog ->
            case model of
                Model data preferences _ ->
                    ( model, saveCatalog data preferences )

        LoadCatalog ->
            case model of
                Model data _ _ ->
                    ( model, loadCatalog )

        CatalogFileReceived file ->
            ( model, Task.attempt CatalogDecoded (File.toString file) )

        CatalogDecoded (Ok jsonString) ->
            case model of
                Model data preferences viewState ->
                    let
                        dataAndPreferences =
                            decodeSaveData jsonString |> (Maybe.withDefault <| Model data preferences)
                    in
                    ( dataAndPreferences viewState, Cmd.none )

        CatalogDecoded (Err _) ->
            ( model, Cmd.none )


decodeSaveData : String -> Maybe (ViewState -> Model)
decodeSaveData jsonString =
    jsonString
        |> Json.decodeString
            (Json.map2
                Model
                (Json.field "data" catalogDecoder)
                (Json.field "preferences" preferencesDecoder)
            )
        |> Result.andThen (Ok << Just)
        |> Result.withDefault Nothing


encodeSaveData : Data -> Preferences -> String
encodeSaveData data preferences =
    let
        catalogRecord =
            [ ( "version", 1 |> Encode.int )
            , ( "data", data |> Encode.dict identity Encode.string )
            , ( "preferences", preferences |> preferencesEncoder )
            ]
    in
    Encode.object catalogRecord
        |> Encode.encode 2


setStartingSlide : ImageKey -> List ImageKey -> List ImageKey
setStartingSlide imageKey slides =
    slides
        |> List.splitWhen ((==) imageKey)
        |> Maybe.andThen (\( head, tail ) -> List.append tail head |> Just)
        |> Maybe.withDefault slides


openSlideshowWith : ImageKey -> (List ImageKey -> ViewState)
openSlideshowWith startingImage =
    setStartingSlide startingImage >> SlideshowState False >> Slideshow



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Model data { slideshowSpeed, keyboardControls } state ->
            case state of
                Slideshow currentState ->
                    let
                        controls =
                            keyboardControls.slideshowMap

                        navigationListeners =
                            [ Browser.onKeyPress <| msgWhen controls.toggle <| \_ -> togglePauseSlideshow currentState
                            , Browser.onKeyUp <| msgWhen controls.next <| \_ -> stepSlideshow currentState Forward
                            , Browser.onKeyUp <| msgWhen controls.prev <| \_ -> stepSlideshow currentState Backward
                            , Browser.onKeyUp <| msgWhen controls.exit <| \_ -> UpdateView (Preview Catalog)
                            ]
                    in
                    case currentState.running of
                        True ->
                            navigationListeners
                                |> (::) (Time.every slideshowSpeed <| \_ -> stepSlideshow currentState Forward)
                                |> Sub.batch

                        False ->
                            navigationListeners
                                |> Sub.batch

                Preview (Focused imageKey) ->
                    let
                        controls =
                            keyboardControls.previewMap
                    in
                    Sub.batch
                        [ Browser.onKeyPress <|
                            msgWhen controls.startSlideshow
                                (\_ -> UpdateView <| openSlideshowWith imageKey <| List.sort <| Dict.keys data)
                        , Browser.onKeyUp <| msgWhen controls.closeCurrent <| \_ -> UpdateView <| Preview Catalog
                        , Browser.onKeyUp <|
                            msgWhen controls.openCurrent
                                (\_ -> UpdateView <| openSlideshowWith imageKey <| List.sort <| Dict.keys data)
                        ]

                Preview Catalog ->
                    let
                        controls =
                            keyboardControls.previewMap
                    in
                    Browser.onKeyPress <|
                        msgWhen controls.startSlideshow <|
                            \_ -> Dict.keys data |> List.sort |> startSlideshow

                Settings ->
                    let
                        controls =
                            keyboardControls.preferencesMap
                    in
                    Browser.onKeyUp <| msgWhen controls.exit (always <| UpdateView previewCatalogState)



-- Encodings


preferencesEncoder : Preferences -> Encode.Value
preferencesEncoder { slideshowSpeed, previewItemsPerRow, backgroundColor, keyboardControls } =
    Encode.object
        [ ( "slideshowSpeed", slideshowSpeed |> Encode.float )
        , ( "previewItemsPerRow", previewItemsPerRow |> Encode.int )
        , ( "backgroundColor", backgroundColor |> Color.toHex |> Encode.string )
        , ( "keyboardControls", keyboardControls |> keyboardControlsEncoder )
        ]


keyboardControlsEncoder : KeyboardMappings -> Encode.Value
keyboardControlsEncoder { slideshowMap, preferencesMap, previewMap } =
    Encode.object
        [ ( "slideshowMap"
          , Encode.object
                [ ( "exit", Encode.list Encode.string slideshowMap.exit )
                , ( "next", Encode.list Encode.string slideshowMap.next )
                , ( "prev", Encode.list Encode.string slideshowMap.prev )
                , ( "toggle", Encode.list Encode.string slideshowMap.toggle )
                ]
          )
        , ( "preferencesMap"
          , Encode.object
                [ ( "exit", Encode.list Encode.string preferencesMap.exit )
                ]
          )
        , ( "previewsMap"
          , Encode.object
                [ ( "startSlideshow", Encode.list Encode.string previewMap.startSlideshow )
                , ( "openCurrent", Encode.list Encode.string previewMap.openCurrent )
                , ( "closeCurrent", Encode.list Encode.string previewMap.closeCurrent )
                ]
          )
        ]


preferencesDecoder : Json.Decoder Preferences
preferencesDecoder =
    Json.map4
        Preferences
        (Json.field "slideshowSpeed" Json.float)
        (Json.field "previewItemsPerRow" Json.int)
        (Json.field "backgroundColor" hexColorDecoder)
        (Json.field "keyboardControls" keyboardMappingDecoder)


hexColorDecoder : Json.Decoder Color
hexColorDecoder =
    Json.string
        |> Json.andThen (always <| Json.succeed defaultBackground)


catalogDecoder : Json.Decoder Data
catalogDecoder =
    Json.keyValuePairs Json.string
        |> Json.andThen (Json.succeed << Dict.fromList)


keyboardMappingDecoder : Json.Decoder KeyboardMappings
keyboardMappingDecoder =
    Json.succeed defaultKeyboardMappings



-- View


view : Model -> Html Msg
view =
    viewSelector >> renderView


{-| viewSelector
Select and transform our Model data into set of required data for the current view.

  - Current view is determined by the type of `ViewState` in our model.
  - The data selected is determined by type of `ViewState` as well as any
    data persisted in that state.

-}
viewSelector : Model -> ViewModel
viewSelector model =
    case model of
        Model data preferences viewState ->
            let
                imageList =
                    Dict.toList data

                backgroundColor =
                    preferences.backgroundColor |> rgbPaletteColor
            in
            case viewState of
                Settings ->
                    SettingsView preferences

                Preview Catalog ->
                    PreviewView imageList
                        Nothing
                        { imagesPerRow = preferences.previewItemsPerRow
                        , backgroundColor = backgroundColor
                        }

                Preview (Focused imageKey) ->
                    let
                        focusedImage =
                            data
                                |> Dict.get imageKey
                                |> Maybe.andThen (Just << Tuple.pair imageKey)
                    in
                    PreviewView
                        imageList
                        focusedImage
                        { imagesPerRow = preferences.previewItemsPerRow
                        , backgroundColor = backgroundColor
                        }

                Slideshow { running, slidelist } ->
                    case List.filterMap (getFromDict data) slidelist of
                        [] ->
                            PreviewView imageList
                                Nothing
                                { imagesPerRow = preferences.previewItemsPerRow
                                , backgroundColor = backgroundColor
                                }

                        firstImage :: images ->
                            SlideshowView firstImage { backgroundColor = backgroundColor }


renderView : ViewModel -> Html Msg
renderView model =
    let
        overlay =
            case model of
                PreviewView _ (Just ( imageKey, imageUrl )) _ ->
                    expandedImage imageUrl

                _ ->
                    Element.none

        content =
            case model of
                PreviewView images _ preferences ->
                    Element.column [ width fill, height fill, Background.color preferences.backgroundColor ]
                        [ imageHeader model
                        , filePreviewView images preferences
                        ]

                SlideshowView currentImage { backgroundColor } ->
                    Element.el
                        [ width fill, height fill, Background.color backgroundColor ]
                        (slideshowView currentImage)

                SettingsView ({ backgroundColor } as preferences) ->
                    Element.column [ width fill, height fill, Background.color (backgroundColor |> rgbPaletteColor) ]
                        [ imageHeader model
                        , editPreferencesView preferences
                        ]
    in
    content
        |> Element.layout [ height fill, width fill, inFront <| overlay ]


imageHeader : ViewModel -> Element Msg
imageHeader model =
    let
        headerBackground =
            case colorPalette of
                ( head, tail ) ->
                    tail |> List.getAt 3 |> Maybe.withDefault head |> rgbPaletteColor

        fontColor =
            case colorPalette of
                ( head, tail ) ->
                    tail
                        |> (::) head
                        |> List.map rgbPaletteColor
                        |> List.last
                        |> Maybe.withDefault (head |> rgbPaletteColor)
    in
    case model of
        SettingsView _ ->
            Element.row [ width fill, Background.color <| headerBackground, Element.spaceEvenly, Element.padding 5 ]
                [ Element.text ""
                , "Preview View"
                    |> Element.text
                    |> Element.el [ onClick <| UpdateView previewCatalogState, Font.color fontColor ]
                , "Select Images"
                    |> Element.text
                    |> Element.el [ onClick OpenImagePicker, Font.color fontColor ]
                , "Save"
                    |> Element.text
                    |> Element.el [ onClick SaveCatalog, Font.color fontColor ]
                , "Load"
                    |> Element.text
                    |> Element.el [ onClick LoadCatalog, Font.color fontColor ]
                ]

        PreviewView imageUrls _ _ ->
            Element.row [ width fill, Background.color <| headerBackground, Element.spaceEvenly, Element.padding 5 ]
                [ "Start Slideshow"
                    |> Element.text
                    |> Element.el
                        [ onClick <| startSlideshow <| List.sort <| List.map Tuple.first imageUrls
                        , Font.color fontColor
                        ]
                , "Preferences"
                    |> Element.text
                    |> Element.el [ onClick <| UpdateView Settings, Font.color fontColor ]
                , "Select Images"
                    |> Element.text
                    |> Element.el [ onClick OpenImagePicker, Font.color fontColor ]
                , "Save"
                    |> Element.text
                    |> Element.el [ onClick SaveCatalog, Font.color fontColor ]
                , "Load"
                    |> Element.text
                    |> Element.el [ onClick LoadCatalog, Font.color fontColor ]
                ]

        _ ->
            Element.row [ width fill, Background.color <| rgba255 220 220 220 0.5, Element.spaceEvenly, Element.padding 5 ]
                [ "" |> Element.text
                , "Preferences" |> Element.text |> Element.el [ onClick <| UpdateView Settings, Font.color fontColor ]
                , "Select Images" |> Element.text |> Element.el [ onClick OpenImagePicker, Font.color fontColor ]
                ]


colorPicker updateMsg =
    case colorPalette of
        ( head, tail ) ->
            tail
                |> (::) head
                |> List.map (colorPickerBox updateMsg)


colorPickerBox colorChangeMsg color =
    Element.el
        [ Background.color (color |> rgbPaletteColor)
        , width fill
        , height fill
        , onClick (colorChangeMsg color)
        ]
        Element.none


editPreferencesView : Preferences -> Element Msg
editPreferencesView preferences =
    let
        { slideshowSpeed, backgroundColor, previewItemsPerRow } =
            preferences
    in
    Element.el [ width fill, height fill, Background.color (backgroundColor |> rgbPaletteColor), Element.spacing 50, Element.padding 20 ] <|
        Element.column [ width Element.fill, Element.padding 35, Background.color <| rgba255 0 0 0 0.5, Element.spacing 10 ]
            [ Input.slider
                [ width <| Element.fillPortion 4
                , Element.behindContent <|
                    Element.el
                        [ Background.color <| rgba255 255 255 255 1
                        , height (5 |> px)
                        , width fill
                        , Element.centerY
                        ]
                        Element.none
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
                    Element.el
                        [ Background.color <| rgba255 255 255 255 1
                        , height (5 |> px)
                        , width fill
                        , Element.centerY
                        ]
                        Element.none
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


filePreviewView :
    List ( ImageKey, ImageUrl )
    -> { r | imagesPerRow : Int, backgroundColor : Element.Color }
    -> Element Msg
filePreviewView images { imagesPerRow, backgroundColor } =
    List.greedyGroupsOf imagesPerRow images
        |> List.map
            (\group ->
                let
                    elementDeficit =
                        imagesPerRow - List.length group
                in
                group
                    |> List.map previewImage
                    |> flip List.append
                        (List.repeat
                            elementDeficit
                            (Element.el [ width fill, height fill ] Element.none)
                        )
                    |> Element.row
                        [ Element.spaceEvenly
                        , Element.spacing 10
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


squareXIconControl msg =
    Icon.x
        |> Icon.toHtml [ Svg.color "#C00000" ]
        |> Element.html
        |> Element.el
            [ onClick msg
            , width (24 |> px)
            , height (24 |> px)
            , alignRight
            , Element.alpha 0.8
            , Element.mouseOver
                [ Element.alpha 1
                , Element.scale 1.2
                ]
            ]


expandIconControl msg =
    Icon.maximize2
        |> Icon.toHtml [ Svg.color "#0000C0" ]
        |> Element.html
        |> Element.el
            [ onClick msg
            , width (24 |> px)
            , height (24 |> px)
            , alignRight
            , alignBottom
            , Element.rotate (Basics.pi / 2)
            , Element.alpha 0.8
            , Element.mouseOver
                [ Element.alpha 1
                , Element.scale 1.2
                , Element.rotate (Basics.pi / 2)
                ]
            ]


previewImageControls imageKey =
    Element.column
        [ width fill
        , height fill
        , Element.padding 8
        , Background.color <| Element.rgba 0.2 0.2 0.2 0.3
        , Element.transparent <| True
        , Element.mouseOver [ Element.transparent <| False ]
        ]
        [ squareXIconControl <| RemoveImage imageKey
        , expandIconControl <| UpdateView <| Preview <| Focused imageKey
        ]
        |> Element.el [ width fill, height fill ]


previewImage : ( ImageKey, ImageUrl ) -> Element Msg
previewImage ( imageKey, imageSrc ) =
    { src = imageSrc
    , description = ""
    }
        |> Element.image
            [ width fill
            , height fill
            , Element.centerX
            , Element.centerY
            , inFront <| previewImageControls imageKey
            ]
        |> Element.el
            [ width fill
            , height fill
            , Element.mouseOver [ Element.scale 1.035 ]
            ]


expandedImage : ImageUrl -> Element Msg
expandedImage imageUrl =
    Element.el
        [ width fill
        , height fill
        , Background.color (Element.rgba 0.1 0.1 0.1 0.9)
        , Element.padding 24
        ]
    <|
        Element.el
            [ width fill
            , height fill
            , inFront <| squareXIconControl <| UpdateView <| Preview Catalog
            ]
            (Element.html
                (Html.img
                    --  Black CSS Magic to make image fit within bounds at normal aspect ratio
                    [ src imageUrl
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


{-| slideshowView
Use of Html.img due to Element.img not respecting parent height with base64 encoded image
-}
slideshowView : String -> Element Msg
slideshowView imageUrl =
    let
        url =
            imageUrl
    in
    Element.el
        [ Element.clip
        , width fill
        , height fill
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
