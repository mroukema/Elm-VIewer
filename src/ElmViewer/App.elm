module ElmViewer.App exposing (Flags, Model, Msg, init, subscriptions, update, view)

import Basics exposing (identity, not, pi)
import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Browser
import Bytes exposing (Bytes)
import Bytes.Decode as Bytes
import Bytes.Encode as BytesEncoder
import Color as Color exposing (Color, toRGB)
import DataUri exposing (DataUri)
import Dict exposing (Dict)
import Element
    exposing
        ( Element
        , alignBottom
        , alignRight
        , alignTop
        , centerX
        , centerY
        , fill
        , fillPortion
        , height
        , inFront
        , maximum
        , mouseOver
        , px
        , rgb
        , rgba255
        , rotate
        , scale
        , text
        , width
        )
import Element.Background as Background
import Element.Events exposing (onClick, onFocus)
import Element.Font as Font
import Element.Input as Input
import ElmViewer.Utils
    exposing
        ( Direction(..)
        , flip
        , getFromDict
        , getRotatedDimensions
        , msgWhenKeyOf
        , rgbPaletteColor
        , seconds
        , stepTupleList
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
import Tuple exposing (mapBoth)
import Url exposing (Url)


{-| Image viewing and slideshow program for local files
-}



-- Init


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model (initialViewport flags) Dict.empty defaultPreferences previewCatalogState
    , Task.perform ViewportChange Dom.getViewport
    )


initialViewport : Flags -> Viewport
initialViewport ( width, height ) =
    Dom.Viewport
        { width = width, height = height }
        { x = 0.0, y = 0.0, width = width, height = height }


defaultPreferences : Preferences
defaultPreferences =
    { slideshowSpeed = 3 |> seconds
    , previewItemsPerRow = 4
    , backgroundColor = defaultBackground
    , keyboardControls = defaultKeyboardMappings
    , defaultRotation = 0
    , defaultZoom = 1
    , saveFilename = Nothing
    , infinityScroll = Vertical
    }


defaultBackground : Color
defaultBackground =
    let
        defaultBackgroundIndex =
            1
    in
    case colorPalette of
        ( head, tail ) ->
            tail
                |> List.getAt defaultBackgroundIndex
                |> Maybe.withDefault head


colorPalette : ( Color, List Color )
colorPalette =
    case Cubehelix.generate 20 of
        head :: tail ->
            ( head, tail )

        [] ->
            ( Color.fromRGB ( 1, 1, 1 ), [] )


{-| Hard coded constant increment size for zoom set to 0.05 (5%)
-}
zoomGranularity : Float
zoomGranularity =
    1 / 20


{-| Hard coded costant for rotation set to 1/72 (5 degrees)
-}
rotationGranularity : Float
rotationGranularity =
    1 / 72


sizeCheckIdPrefix =
    "sizeCheckPrefix"


previewCatalogState : ViewState
previewCatalogState =
    Preview (Catalog Nothing)


defaultSlideshowMap =
    { next = [ "ArrowRight" ]
    , prev = [ "ArrowLeft" ]
    , exit = [ "Escape", "ArrowDown" ]
    , toggle = [ " " ]
    , rotateP = [ "|" ]
    , rotateM = [ "\\" ]
    , zoomP = [ "+", "=" ]
    , zoomM = [ "-", "_" ]
    }


defaultPreferencesMap =
    { exit = [ "Escape", "ArrowDown" ]
    }


defaultPreviewMap =
    { openCurrent = [ "ArrowUp" ]
    , closeCurrent = [ "ArrowDown", "Escape" ]
    , startSlideshow = [ " " ]
    , startInfinityMode = [ "i" ]
    }


defaultInfinityMap =
    { closeInfinity = [ "x", "Escape" ]
    }


defaultKeyboardMappings : KeyboardMappings
defaultKeyboardMappings =
    { slideshowMap = defaultSlideshowMap
    , preferencesMap = defaultPreferencesMap
    , previewMap = defaultPreviewMap
    , infinityMap = defaultInfinityMap
    }


defaultSaveFilename : Filename
defaultSaveFilename =
    "imagerState.json"



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
    = Model Viewport Data Preferences ViewState


type alias Flags =
    ( Float, Float )


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
        (List ( ImageKey, ReadyImage ))
        (Maybe ( ImageKey, ReadyImage ))
        { imagesPerRow : Int
        , backgroundColor : Element.Color
        , imageSelection : Maybe (List ImageKey)
        , dimensionlessImages : List ( ImageKey, LoadingImage )
        , viewport : Dom.Viewport
        , defaultRotation : Float
        , defaultZoom : Float
        , saveFilename : Maybe String
        }
    | SlideshowView
        ReadyImage
        { backgroundColor : Element.Color
        , defaultRotation : Float
        , defaultZoom : Float
        , width : Float
        , height : Float
        , dimensionlessImages : List ( ImageKey, LoadingImage )
        }
    | SettingsView Preferences
    | InfinityView
        { images : List ( ImageKey, ReadyImage )
        , backgroundColor : Element.Color
        , dimensionlessImages : List ( ImageKey, LoadingImage )
        , viewport : Dom.Viewport
        , defaultZoom : Float
        , defaultRotation : Float
        , scroll : InfinityScroll
        }


{-| ViewState
ViewState is the persistence of what both what the current view is any
state data used by that scene.
-}
type ViewState
    = Slideshow SlideshowState
    | Preview PreviewState
    | Settings
    | Infinity InfinityState


{-| SlideshowState
Data that needs to be persisted when in the slideshow scene

Note: persisted data for scene not data required to render the scene

-}
type alias SlideshowState =
    { running : Bool, slidelist : List ImageKey }


{-| PreviewState
-}
type PreviewState
    = Catalog (Maybe (List ImageKey))
    | Focused ( FocusedImage, List ImageKey )


type InfinityScroll
    = Vertical
    | Horizontal


type alias InfinityState =
    { scroll : InfinityScroll
    }


type alias FocusedImage =
    ImageKey


{-| An image is Loading until we have computed native (natural) image dimensions
-}
type alias LoadingImage =
    { imageUrl : ImageUrl
    , rotation : Maybe Float
    , zoom : Maybe Float
    }


{-| An image is ready to be displayed when we have computed native (natural) image dimensions
-}
type alias ReadyImage =
    { imageUrl : ImageUrl
    , nativeDimensions : { width : Float, height : Float }
    , rotation : Maybe Float
    , zoom : Maybe Float
    }


{-| An Image can be ready to be displayed or can still be processing
-}
type Image
    = Processing LoadingImage
    | Ready ReadyImage


{-| Dict of all the { Image Key : Image Data }
-}
type alias Data =
    Dict ImageKey Image


{-| Common app preferences
-}
type alias Preferences =
    { slideshowSpeed : Float
    , previewItemsPerRow : Int
    , backgroundColor : Color
    , keyboardControls : KeyboardMappings
    , defaultRotation : Float
    , defaultZoom : Float
    , saveFilename : Maybe String
    , infinityScroll : InfinityScroll
    }


type alias ImageKey =
    String


type alias ImageUrl =
    String


type alias KeyboardMappings =
    { slideshowMap : SlideshowMap
    , preferencesMap : PreferencesMap
    , previewMap : PreviewMap
    , infinityMap : InfinityMap
    }


type alias SlideshowMap =
    { next : List String
    , prev : List String
    , exit : List String
    , toggle : List String
    , rotateP : List String
    , rotateM : List String
    , zoomP : List String
    , zoomM : List String
    }


type alias PreferencesMap =
    { exit : List String
    }


type alias PreviewMap =
    { openCurrent : List String
    , closeCurrent : List String
    , startSlideshow : List String
    , startInfinityMode : List String
    }


type alias InfinityMap =
    { closeInfinity : List String
    }


type alias Filename =
    String



-- Msg


type Msg
    = OpenImagePicker
    | FilesReceived File (List File)
    | InsertImage ImageKey (Result () Image)
    | RemoveImage ImageKey
    | UpdateImage ImageKey (Maybe Image)
    | UpdateView ViewState
    | UpdatePreferences Preferences
    | SaveCatalog String
    | LoadCatalog
    | CatalogFileReceived File
    | CatalogDecoded (Result () ImageKey)
    | ViewportChange Dom.Viewport
    | GetImageDimensions ImageKey
    | ImageDimensions ImageKey (Result Dom.Error Dom.Element)
    | UpdateSaveName Filename
    | GetViewport
    | NoOp


updateSlideshow : SlideshowState -> Msg
updateSlideshow state =
    UpdateView <| Slideshow state


startSlideshow : List ImageKey -> Msg
startSlideshow slides =
    updateSlideshow { running = True, slidelist = slides }


startInfinityMode : InfinityScroll -> List ImageKey -> Msg
startInfinityMode scroll slides =
    UpdateView <| Infinity (InfinityState scroll)


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
    Task.attempt (InsertImage (File.name file))
        (File.toUrl file
            |> Task.andThen
                (\imageUrl ->
                    LoadingImage imageUrl Nothing Nothing |> Processing |> Task.succeed
                )
        )


loadCatalog : Cmd Msg
loadCatalog =
    Select.file [ "application/json" ] CatalogFileReceived


saveCatalog : Data -> Preferences -> String -> Cmd Msg
saveCatalog data preferences filename =
    Download.string filename "application/json" (encodeSaveData data preferences)


getImageDimensions : ImageKey -> Cmd Msg
getImageDimensions imageKey =
    Task.attempt (ImageDimensions imageKey)
        (Dom.getElement (sizeCheckIdPrefix ++ imageKey))


scrollToElement : ImageKey -> Cmd Msg
scrollToElement imageKey =
    Dom.getElement imageKey
        |> Task.andThen (\{ element } -> Dom.setViewport element.x element.y)
        |> Task.attempt (always NoOp)



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { cViewport, cImages, cPreferences, cState } =
            case model of
                Model viewport_ images_ preferences_ state_ ->
                    { cViewport = viewport_
                    , cImages = images_
                    , cPreferences = preferences_
                    , cState = state_
                    }
    in
    case msg of
        ViewportChange viewport ->
            ( Model viewport cImages cPreferences cState, Cmd.none )

        OpenImagePicker ->
            ( model, Select.files [ "image/png", "image/jpg" ] FilesReceived )

        FilesReceived file otherFiles ->
            ( model
            , List.map insertImageFromFile (file :: otherFiles) |> Cmd.batch
            )

        InsertImage filename (Ok image) ->
            case model of
                Model viewport images preferences state ->
                    ( Model
                        viewport
                        (Dict.insert filename image images)
                        preferences
                        state
                    , getImageDimensions filename
                    )

        InsertImage filename (Err _) ->
            ( model, Cmd.none )

        RemoveImage imageKey ->
            case model of
                Model viewport data preferences state ->
                    ( Model viewport (Dict.remove imageKey data) preferences state, Cmd.none )

        UpdateImage imageKey maybeImage ->
            case maybeImage of
                Just newImage ->
                    ( Model
                        cViewport
                        (Dict.update imageKey (always (Just newImage)) cImages)
                        cPreferences
                        cState
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        UpdatePreferences preferences ->
            case model of
                Model viewport data _ state ->
                    ( Model viewport data preferences state, Cmd.none )

        UpdateView newState ->
            let
                newModel =
                    Model cViewport cImages cPreferences newState
            in
            case cState of
                Preview (Focused ( imageKey, _ )) ->
                    ( newModel, scrollToElement imageKey )

                Slideshow { slidelist } ->
                    case slidelist of
                        [] ->
                            ( newModel, Cmd.none )

                        imageKey :: _ ->
                            ( newModel, scrollToElement imageKey )

                _ ->
                    ( newModel, Cmd.none )

        SaveCatalog filename ->
            case model of
                Model _ data preferences _ ->
                    ( model, saveCatalog data preferences filename )

        LoadCatalog ->
            case model of
                Model _ data _ _ ->
                    ( model, loadCatalog )

        CatalogFileReceived file ->
            ( model, Task.attempt CatalogDecoded (File.toString file) )

        CatalogDecoded (Ok jsonString) ->
            case model of
                Model viewport data preferences viewState ->
                    let
                        dataAndPreferences =
                            decodeSaveData viewport jsonString
                                |> Maybe.withDefault (Model viewport data preferences)

                        newModel =
                            dataAndPreferences viewState

                        ( _, dimensionlessImages ) =
                            case newModel of
                                Model _ newData _ _ ->
                                    partitionReadyImages newData
                    in
                    ( newModel
                    , dimensionlessImages
                        |> List.map
                            (Tuple.first
                                >> GetImageDimensions
                                >> Task.succeed
                                >> Task.attempt
                                    (\result ->
                                        case result of
                                            Ok imageMsg ->
                                                imageMsg

                                            _ ->
                                                NoOp
                                    )
                            )
                        |> Cmd.batch
                    )

        CatalogDecoded (Err _) ->
            ( model, Cmd.none )

        GetImageDimensions filename ->
            ( model, getImageDimensions filename )

        ImageDimensions filename result ->
            case result of
                Ok element ->
                    ( Model cViewport
                        (updateImageDimensions cImages filename element.element)
                        cPreferences
                        cState
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateSaveName newFilename ->
            case newFilename == "" of
                True ->
                    ( Model cViewport cImages { cPreferences | saveFilename = Nothing } cState, Cmd.none )

                _ ->
                    ( Model
                        cViewport
                        cImages
                        { cPreferences | saveFilename = Just <| newFilename }
                        cState
                    , Cmd.none
                    )

        GetViewport ->
            ( model, Task.perform ViewportChange Dom.getViewport )

        NoOp ->
            ( model, Cmd.none )


updateImageDimensions : Data -> Filename -> { r | width : Float, height : Float } -> Data
updateImageDimensions data filename { width, height } =
    Dict.update filename
        (\value ->
            case value of
                Just (Processing image) ->
                    Just
                        (Ready <|
                            ReadyImage image.imageUrl { width = width, height = height } Nothing Nothing
                        )

                Just (Ready image) ->
                    Just
                        (Ready <|
                            ReadyImage image.imageUrl { width = width, height = height } Nothing Nothing
                        )

                Nothing ->
                    Nothing
        )
        data


decodeSaveData : Viewport -> String -> Maybe (ViewState -> Model)
decodeSaveData viewport jsonString =
    jsonString
        |> Json.decodeString
            (Json.map3
                Model
                (Json.succeed viewport)
                (Json.field "data" catalogDecoder)
                (Json.field "preferences" preferencesDecoder)
            )
        |> Result.andThen (Ok << Just)
        |> Result.withDefault Nothing


encodeImage : Image -> Encode.Value
encodeImage image_ =
    case image_ of
        Processing loadingImage ->
            loadingImage |> .imageUrl >> Encode.string

        Ready readyImage ->
            readyImage |> .imageUrl >> Encode.string


encodeSaveData : Data -> Preferences -> String
encodeSaveData data preferences =
    let
        version =
            1

        indentLevel =
            2

        catalogRecord =
            [ ( "version", version |> Encode.int )
            , ( "data", data |> Encode.dict identity encodeImage )
            , ( "preferences", preferences |> preferencesEncoder )
            ]
    in
    Encode.object catalogRecord
        |> Encode.encode indentLevel


setStartingSlide : ImageKey -> List ImageKey -> List ImageKey
setStartingSlide imageKey slides =
    slides
        |> List.splitWhen ((==) imageKey)
        |> Maybe.andThen (\( head, tail ) -> List.append tail head |> Just)
        |> Maybe.withDefault slides


openSlideshowWith : ImageKey -> (List ImageKey -> ViewState)
openSlideshowWith startingImage =
    setStartingSlide startingImage >> SlideshowState False >> Slideshow


updateImageRotation : Data -> Float -> ImageKey -> Float -> Maybe Image
updateImageRotation data defaultRotation imageKey delta =
    let
        boundedAdd =
            \delta_ rotation ->
                case ((rotation + delta_) > 0) && (rotation + delta_ < 1) of
                    True ->
                        rotation + delta_

                    False ->
                        (rotation + delta_) - (toFloat << round <| (rotation + delta_))

        activeImage =
            Dict.get imageKey data
    in
    activeImage
        |> Maybe.andThen
            (\image_ ->
                case image_ of
                    Processing image ->
                        (Just << Processing)
                            { image
                                | rotation =
                                    image.rotation
                                        |> Maybe.withDefault defaultRotation
                                        |> boundedAdd delta
                                        |> Just
                            }

                    Ready image ->
                        (Just << Ready)
                            { image
                                | rotation =
                                    image.rotation
                                        |> Maybe.withDefault defaultRotation
                                        |> boundedAdd delta
                                        |> Just
                            }
            )



-- Subscriptions


updateImageZoom : Data -> Float -> ImageKey -> Float -> Maybe Image
updateImageZoom data defaultZoom imageKey delta =
    let
        boundedAdd =
            \delta_ zoom ->
                case ((zoom + delta_) > 0) && (zoom + delta_ <= 5) of
                    True ->
                        zoom + delta_

                    False ->
                        zoom

        activeImage =
            Dict.get imageKey data
    in
    activeImage
        |> Maybe.andThen
            (\image_ ->
                case image_ of
                    Processing image ->
                        (Just << Processing)
                            { image
                                | zoom =
                                    image.zoom
                                        |> Maybe.withDefault defaultZoom
                                        |> boundedAdd delta
                                        |> Just
                            }

                    Ready image ->
                        (Just << Ready)
                            { image
                                | zoom =
                                    image.zoom
                                        |> Maybe.withDefault defaultZoom
                                        |> boundedAdd delta
                                        |> Just
                            }
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        viewportResizeListener =
            Browser.onResize (\_ _ -> GetViewport)
    in
    case model of
        Model _ data preferences state ->
            let
                { slideshowSpeed, keyboardControls, defaultRotation, defaultZoom, infinityScroll } =
                    preferences
            in
            case state of
                Slideshow currentState ->
                    let
                        currentSlideKey =
                            List.head currentState.slidelist

                        controls =
                            keyboardControls.slideshowMap

                        navigationListeners =
                            [ Browser.onKeyPress <|
                                msgWhenKeyOf controls.toggle (always <| togglePauseSlideshow currentState)
                            , Browser.onKeyUp <|
                                msgWhenKeyOf controls.next (always <| stepSlideshow currentState Forward)
                            , Browser.onKeyUp <|
                                msgWhenKeyOf controls.prev (always <| stepSlideshow currentState Backward)
                            , Browser.onKeyUp <|
                                msgWhenKeyOf controls.exit (always <| UpdateView (Preview (Catalog Nothing)))
                            , Browser.onKeyUp <|
                                msgWhenKeyOf controls.rotateP
                                    (always <|
                                        UpdateImage (currentSlideKey |> Maybe.withDefault "")
                                            (updateImageRotation data
                                                defaultRotation
                                                (currentSlideKey |> Maybe.withDefault "")
                                                rotationGranularity
                                            )
                                    )
                            , Browser.onKeyUp <|
                                msgWhenKeyOf controls.rotateM
                                    (always <|
                                        UpdateImage (currentSlideKey |> Maybe.withDefault "")
                                            (updateImageRotation data
                                                defaultRotation
                                                (currentSlideKey |> Maybe.withDefault "")
                                                -rotationGranularity
                                            )
                                    )
                            , Browser.onKeyUp <|
                                msgWhenKeyOf controls.zoomP
                                    (always <|
                                        UpdateImage (currentSlideKey |> Maybe.withDefault "")
                                            (updateImageZoom data
                                                defaultZoom
                                                (currentSlideKey |> Maybe.withDefault "")
                                                zoomGranularity
                                            )
                                    )
                            , Browser.onKeyUp <|
                                msgWhenKeyOf controls.zoomM
                                    (always <|
                                        UpdateImage (currentSlideKey |> Maybe.withDefault "")
                                            (updateImageZoom data
                                                defaultZoom
                                                (currentSlideKey |> Maybe.withDefault "")
                                                -zoomGranularity
                                            )
                                    )
                            ]
                    in
                    case currentState.running && (Dict.size data > 1) of
                        True ->
                            navigationListeners
                                |> (::)
                                    (Time.every slideshowSpeed
                                        (always <| stepSlideshow currentState Forward)
                                    )
                                |> (::) viewportResizeListener
                                |> Sub.batch

                        False ->
                            navigationListeners
                                |> (::) viewportResizeListener
                                |> Sub.batch

                Preview (Focused (( imageKey, imageList ) as tupleList)) ->
                    let
                        controlKeys =
                            keyboardControls.previewMap

                        keyPressListeners =
                            [ Browser.onKeyPress <|
                                msgWhenKeyOf controlKeys.startSlideshow
                                    (always <|
                                        UpdateView (openSlideshowWith imageKey <| List.sort <| Dict.keys data)
                                    )
                            , Browser.onKeyUp <|
                                msgWhenKeyOf controlKeys.closeCurrent
                                    (always <| UpdateView <| Preview (Catalog Nothing))
                            , Browser.onKeyUp <|
                                msgWhenKeyOf controlKeys.openCurrent
                                    (always <|
                                        UpdateView (openSlideshowWith imageKey <| List.sort <| Dict.keys data)
                                    )
                            , Browser.onKeyUp <|
                                msgWhenKeyOf [ "ArrowRight" ]
                                    (always <|
                                        UpdateView (Preview (Focused (tupleList |> stepTupleList Forward)))
                                    )
                            , Browser.onKeyUp <|
                                msgWhenKeyOf [ "ArrowLeft" ]
                                    (always <|
                                        UpdateView (Preview (Focused (tupleList |> stepTupleList Backward)))
                                    )
                            ]
                    in
                    keyPressListeners
                        |> (::) viewportResizeListener
                        |> Sub.batch

                Preview (Catalog imageList) ->
                    let
                        controls =
                            keyboardControls.previewMap

                        keyPressListeners =
                            [ Browser.onKeyPress <|
                                msgWhenKeyOf controls.startSlideshow
                                    (always <| startSlideshow <| List.sort <| Dict.keys data)
                            , Browser.onKeyPress <|
                                msgWhenKeyOf controls.startInfinityMode
                                    (always <|
                                        startInfinityMode infinityScroll <|
                                            List.sort <|
                                                Dict.keys data
                                    )
                            ]
                    in
                    keyPressListeners
                        |> (::) viewportResizeListener
                        |> Sub.batch

                Settings ->
                    let
                        controls =
                            keyboardControls.preferencesMap

                        keyPressListeners =
                            [ Browser.onKeyUp <|
                                msgWhenKeyOf controls.exit
                                    (always <| UpdateView previewCatalogState)
                            ]
                    in
                    keyPressListeners
                        |> (::) viewportResizeListener
                        |> Sub.batch

                Infinity _ ->
                    let
                        controls =
                            keyboardControls.infinityMap

                        keyPressListeners =
                            [ Browser.onKeyUp <|
                                msgWhenKeyOf controls.closeInfinity
                                    (always <| UpdateView (Preview (Catalog Nothing)))
                            ]
                    in
                    keyPressListeners
                        |> (::) viewportResizeListener
                        |> Sub.batch



-- Encodings


preferencesEncoder : Preferences -> Encode.Value
preferencesEncoder preferences =
    let
        { slideshowSpeed, previewItemsPerRow, saveFilename, infinityScroll } =
            preferences

        { backgroundColor, keyboardControls, defaultRotation, defaultZoom } =
            preferences

        saveFilenameEntry =
            saveFilename
                |> Maybe.andThen (\filename -> [ ( "saveFilename", filename |> Encode.string ) ] |> Just)
                |> Maybe.withDefault []
    in
    Encode.object
        (List.append saveFilenameEntry
            [ ( "slideshowSpeed", slideshowSpeed |> Encode.float )
            , ( "previewItemsPerRow", previewItemsPerRow |> Encode.int )
            , ( "backgroundColor", backgroundColor |> Color.toHex |> Encode.string )
            , ( "keyboardControls", keyboardControls |> keyboardControlsEncoder )
            , ( "defaultRotation", defaultRotation |> Encode.float )
            , ( "defaultZoom", defaultZoom |> Encode.float )
            , ( "infinityState", infinityScroll |> encodeInfinityState )
            ]
        )


encodeInfinityState state =
    case state of
        Vertical ->
            "vertical" |> Encode.string

        Horizontal ->
            "horizontal" |> Encode.string


preferencesDecoder : Json.Decoder Preferences
preferencesDecoder =
    Json.map8
        Preferences
        (fieldWithDefault "slideshowSpeed" defaultPreferences.slideshowSpeed Json.float)
        (fieldWithDefault "previewItemsPerRow" defaultPreferences.previewItemsPerRow Json.int)
        (fieldWithDefault "backgroundColor" defaultPreferences.backgroundColor hexColorDecoder)
        (fieldWithDefault "keyboardControls" defaultPreferences.keyboardControls keyboardControlsDecoder)
        (fieldWithDefault "defaultRotation" defaultPreferences.defaultRotation Json.float)
        (fieldWithDefault "defaultZoom" defaultPreferences.defaultZoom Json.float)
        (Json.maybe (Json.field "saveFilename" Json.string))
        (fieldWithDefault "infinityScroll"
            defaultPreferences.infinityScroll
            decodeInfinityScroll
        )


decodeInfinityScroll =
    Json.string
        |> Json.andThen
            (\value ->
                case value of
                    "vertical" ->
                        Json.succeed Vertical

                    "horizontal" ->
                        Json.succeed Horizontal

                    _ ->
                        Json.fail "invalid value for infinityState"
            )


keyboardControlsEncoder : KeyboardMappings -> Encode.Value
keyboardControlsEncoder { slideshowMap, preferencesMap, previewMap, infinityMap } =
    Encode.object
        [ ( "slideshowMap"
          , Encode.object
                [ ( "exit", Encode.list Encode.string slideshowMap.exit )
                , ( "next", Encode.list Encode.string slideshowMap.next )
                , ( "prev", Encode.list Encode.string slideshowMap.prev )
                , ( "toggle", Encode.list Encode.string slideshowMap.toggle )
                , ( "rotateP", Encode.list Encode.string slideshowMap.rotateP )
                , ( "rotateM", Encode.list Encode.string slideshowMap.rotateM )
                , ( "zoomP", Encode.list Encode.string slideshowMap.zoomP )
                , ( "zoomM", Encode.list Encode.string slideshowMap.zoomM )
                ]
          )
        , ( "preferenceMap"
          , Encode.object
                [ ( "exit", Encode.list Encode.string preferencesMap.exit )
                ]
          )
        , ( "previewMap"
          , Encode.object
                [ ( "startSlideshow", Encode.list Encode.string previewMap.startSlideshow )
                , ( "openCurrent", Encode.list Encode.string previewMap.openCurrent )
                , ( "closeCurrent", Encode.list Encode.string previewMap.closeCurrent )
                , ( "startInfinityMode", Encode.list Encode.string previewMap.startInfinityMode )
                ]
          )
        , ( "infinityMap"
          , Encode.object
                [ ( "closeInfinity", Encode.list Encode.string infinityMap.closeInfinity )
                ]
          )
        ]


fieldWithDefault : String -> a -> Json.Decoder a -> Json.Decoder a
fieldWithDefault fieldLabel default decoder =
    Json.andThen
        (\maybeField ->
            case maybeField of
                Just value ->
                    Json.succeed value

                Nothing ->
                    Json.succeed default
        )
        (Json.maybe
            (Json.field fieldLabel decoder)
        )


keyboardControlsDecoder : Json.Decoder KeyboardMappings
keyboardControlsDecoder =
    Json.map4
        KeyboardMappings
        (Json.field "slideshowMap"
            (Json.map8 SlideshowMap
                (fieldWithDefault "next" defaultSlideshowMap.next (Json.list Json.string))
                (fieldWithDefault "prev" defaultSlideshowMap.prev (Json.list Json.string))
                (fieldWithDefault "exit" defaultSlideshowMap.exit (Json.list Json.string))
                (fieldWithDefault "toggle" defaultSlideshowMap.toggle (Json.list Json.string))
                (fieldWithDefault "rotateP" defaultSlideshowMap.rotateP (Json.list Json.string))
                (fieldWithDefault "rotateM" defaultSlideshowMap.rotateM (Json.list Json.string))
                (fieldWithDefault "zoomP" defaultSlideshowMap.zoomP (Json.list Json.string))
                (fieldWithDefault "zoomM" defaultSlideshowMap.zoomM (Json.list Json.string))
            )
        )
        (Json.field "preferenceMap"
            (Json.map
                PreferencesMap
                (fieldWithDefault "exit" defaultPreferencesMap.exit (Json.list Json.string))
            )
        )
        (Json.field "previewMap"
            (Json.map4 PreviewMap
                (fieldWithDefault "openCurrent" defaultPreviewMap.openCurrent (Json.list Json.string))
                (fieldWithDefault "closeCurrent" defaultPreviewMap.closeCurrent (Json.list Json.string))
                (fieldWithDefault "startSlideshow" defaultPreviewMap.startSlideshow (Json.list Json.string))
                (fieldWithDefault
                    "startInfinityMode"
                    defaultPreviewMap.startInfinityMode
                    (Json.list Json.string)
                )
            )
        )
        (Json.field "infinityMap"
            (Json.map InfinityMap
                (fieldWithDefault "closeInfinity" defaultInfinityMap.closeInfinity (Json.list Json.string))
            )
        )


hexColorDecoder : Json.Decoder Color
hexColorDecoder =
    Json.string
        |> Json.andThen (always <| Json.succeed defaultBackground)


decodeImage : String -> Json.Decoder Image
decodeImage imageUrl =
    Json.succeed <| Processing { imageUrl = imageUrl, rotation = Nothing, zoom = Nothing }


catalogDecoder : Json.Decoder (Dict ImageKey Image)
catalogDecoder =
    Json.keyValuePairs (Json.string |> Json.andThen decodeImage)
        |> Json.andThen (Json.succeed << Dict.fromList)



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
        Model viewport data preferences viewState ->
            let
                ( readyImages, processingImages ) =
                    partitionReadyImages data

                readyImageDict =
                    Dict.fromList readyImages

                backgroundColor =
                    preferences.backgroundColor |> rgbPaletteColor
            in
            case viewState of
                Settings ->
                    SettingsView preferences

                Preview (Catalog imageList) ->
                    PreviewView readyImages
                        Nothing
                        { imagesPerRow = preferences.previewItemsPerRow
                        , backgroundColor = backgroundColor
                        , imageSelection = Just <| Dict.keys data
                        , dimensionlessImages = processingImages
                        , viewport = viewport
                        , defaultRotation = preferences.defaultRotation
                        , defaultZoom = preferences.defaultZoom
                        , saveFilename = preferences.saveFilename
                        }

                Preview (Focused ( imageKey, imageList )) ->
                    let
                        focusedImage =
                            readyImageDict
                                |> Dict.get imageKey
                                |> Maybe.andThen (Just << Tuple.pair imageKey)
                    in
                    PreviewView
                        readyImages
                        focusedImage
                        { imagesPerRow = preferences.previewItemsPerRow
                        , backgroundColor = backgroundColor
                        , imageSelection = Just imageList
                        , dimensionlessImages = processingImages
                        , viewport = viewport
                        , defaultRotation = preferences.defaultRotation
                        , defaultZoom = preferences.defaultZoom
                        , saveFilename = preferences.saveFilename
                        }

                Slideshow { running, slidelist } ->
                    case List.filterMap (getFromDict readyImageDict) slidelist of
                        [] ->
                            PreviewView readyImages
                                Nothing
                                { imagesPerRow = preferences.previewItemsPerRow
                                , backgroundColor = backgroundColor
                                , imageSelection = Nothing
                                , dimensionlessImages = processingImages
                                , viewport = viewport
                                , defaultRotation = preferences.defaultRotation
                                , defaultZoom = preferences.defaultZoom
                                , saveFilename = preferences.saveFilename
                                }

                        firstImage :: images ->
                            SlideshowView firstImage
                                { backgroundColor = backgroundColor
                                , defaultRotation = preferences.defaultRotation
                                , defaultZoom = preferences.defaultZoom
                                , width = viewport.viewport.width
                                , height = viewport.viewport.height
                                , dimensionlessImages = processingImages
                                }

                Infinity { scroll } ->
                    InfinityView
                        { backgroundColor = backgroundColor
                        , defaultRotation = 0
                        , defaultZoom = 1
                        , dimensionlessImages = processingImages
                        , images = readyImages
                        , viewport = viewport
                        , scroll = scroll
                        }


partitionReadyImages : Data -> ( List ( Filename, ReadyImage ), List ( Filename, LoadingImage ) )
partitionReadyImages data =
    Dict.foldl
        (\key value ( ready, processing ) ->
            case value of
                Processing pImage ->
                    ( ready, ( key, pImage ) :: processing )

                Ready rImage ->
                    ( ( key, rImage ) :: ready, processing )
        )
        ( [], [] )
        data


renderView : ViewModel -> Html Msg
renderView model =
    let
        overlay =
            case model of
                PreviewView _ (Just ( imageKey, image )) ({ viewport } as options) ->
                    expandedImage ( imageKey, image ) viewport.viewport options

                _ ->
                    Element.none

        getDimensionList =
            case model of
                PreviewView _ _ { dimensionlessImages } ->
                    dimensionlessImages

                SlideshowView _ { dimensionlessImages } ->
                    dimensionlessImages

                SettingsView _ ->
                    []

                InfinityView { dimensionlessImages } ->
                    dimensionlessImages

        backgroundColor =
            case model of
                PreviewView _ _ preferences ->
                    preferences.backgroundColor

                SlideshowView _ preferences ->
                    preferences.backgroundColor

                SettingsView preferences ->
                    preferences.backgroundColor |> rgbPaletteColor

                InfinityView state ->
                    state.backgroundColor

        content =
            case model of
                PreviewView images _ preferences ->
                    Element.column
                        [ width fill, height fill, Element.clipX ]
                        [ imageHeader model
                        , filePreviewView images preferences
                        ]

                SlideshowView currentImage ({ width, height } as options) ->
                    Element.el
                        [ Element.width fill, Element.height fill ]
                        (slideshowViewElement currentImage
                            { width = width, height = height }
                            options
                        )

                SettingsView preferences ->
                    Element.column
                        [ width fill, height fill ]
                        [ imageHeader model
                        , editPreferencesView preferences
                        ]

                InfinityView state ->
                    Element.column
                        [ width fill
                        , height fill
                        ]
                        [ infinityReadingView state ]
    in
    content
        |> Element.layout
            [ inFront <| overlay
            , Background.color backgroundColor
            , Element.onRight (dimensionGetter getDimensionList)
            ]


dimensionGetter : List ( Filename, LoadingImage ) -> Element Msg
dimensionGetter dimensionlessImages =
    Element.row []
        (List.map
            (\( filename, image ) ->
                Element.image
                    [ elementId (sizeCheckIdPrefix ++ filename)
                    ]
                    { src = image.imageUrl, description = "" }
            )
            dimensionlessImages
        )


elementId =
    Html.Attributes.id >> Element.htmlAttribute


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

        startSlides =
            \images ->
                Icon.film
                    |> iconElement [ Svg.color "#FFFFFF" ]
                    |> Element.el
                        [ centerX
                        , onClick <| startSlideshow <| List.sort <| List.map Tuple.first images
                        , Element.mouseOver [ Font.color <| Element.rgb255 230 247 241, Element.scale 1.1 ]
                        ]
                    |> Element.el [ Font.color fontColor, width fill, centerX ]

        preferences =
            Icon.eye
                |> iconElement [ Svg.color "#FFFFFF" ]
                |> Element.el
                    [ centerX
                    , onClick <| UpdateView Settings
                    , Element.mouseOver [ Font.color <| Element.rgb255 230 247 241, Element.scale 1.1 ]
                    ]
                |> Element.el [ Font.color fontColor, width fill, centerX ]

        previewView =
            Icon.eye
                |> iconElement [ Svg.color "#FFFFFF" ]
                |> Element.el
                    [ centerX
                    , onClick <| UpdateView previewCatalogState
                    , Element.mouseOver [ Font.color <| Element.rgb255 230 247 241, Element.scale 1.1 ]
                    ]
                |> Element.el [ Font.color fontColor, centerX, width fill ]

        selectImages =
            Icon.filePlus
                |> iconElement [ Svg.color "#FFFFFF" ]
                |> Element.el
                    [ centerX
                    , onClick OpenImagePicker
                    , Element.mouseOver [ Font.color <| Element.rgb255 230 247 241, Element.scale 1.1 ]
                    ]
                |> Element.el [ Font.color fontColor, centerX, width fill ]

        save =
            \saveName ->
                Element.row [ centerX, width fill ]
                    [ Icon.download
                        |> iconElement [ Svg.color "#FFFFFF" ]
                        |> Element.el
                            [ centerX
                            , onClick (SaveCatalog (saveName |> Maybe.withDefault defaultSaveFilename))
                            , Element.mouseOver [ Font.color <| Element.rgb255 230 247 241, Element.scale 1.1 ]
                            ]
                        |> Element.el [ Font.color fontColor, centerX, width fill ]
                    , Input.text [ width fill, height fill ]
                        { onChange = UpdateSaveName
                        , text = saveName |> Maybe.withDefault ""
                        , placeholder = Just <| Input.placeholder [] (text defaultSaveFilename)
                        , label = Input.labelHidden "Filename"
                        }
                    ]

        load =
            Icon.upload
                |> iconElement [ Svg.color "#FFFFFF" ]
                |> Element.el
                    [ centerX
                    , onClick LoadCatalog
                    , Element.mouseOver [ Font.color <| Element.rgb255 230 247 241, Element.scale 1.1 ]
                    ]
                |> Element.el [ Font.color fontColor, centerX, width fill ]
    in
    case model of
        SettingsView { saveFilename } ->
            Element.row
                [ Background.color <| headerBackground
                , Element.spaceEvenly
                , Element.padding 5
                , centerX
                , width fill
                , height (50 |> px)
                , Element.padding 20
                ]
                [ Element.el [ centerX, width fill ] Element.none
                , Element.text ""
                , previewView
                , selectImages
                , save saveFilename
                , load
                ]

        PreviewView imageUrls _ { saveFilename } ->
            Element.row
                [ width fill
                , height (50 |> px)
                , Background.color <| headerBackground
                , Element.spaceEvenly
                , Element.padding 20
                ]
                [ startSlides imageUrls
                , preferences
                , selectImages
                , save saveFilename
                , load
                ]

        _ ->
            Element.none


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


degreesSymbol =
    Char.fromCode 0xB0


percentSymbol =
    Char.fromCode 0xFE6A


editPreferencesView : Preferences -> Element Msg
editPreferencesView preferences =
    let
        { slideshowSpeed, backgroundColor, previewItemsPerRow } =
            preferences

        { keyboardControls, defaultRotation, defaultZoom, infinityScroll } =
            preferences
    in
    Element.el
        [ width fill
        , height fill
        , Background.color (backgroundColor |> rgbPaletteColor)
        , Element.spacing 50
        , Element.padding 20
        ]
    <|
        Element.column
            [ width Element.fill
            , Element.padding 35
            , Background.color <| rgba255 0 0 0 0.5
            , Element.spacing 20
            ]
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
                { onChange =
                    round
                        >> (\newCount ->
                                UpdatePreferences
                                    { preferences | previewItemsPerRow = newCount }
                           )
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
                    (colorPicker
                        (\newColor ->
                            UpdatePreferences { preferences | backgroundColor = newColor }
                        )
                    )
                ]
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
                { onChange =
                    \newAngle ->
                        UpdatePreferences
                            { preferences | defaultRotation = newAngle }
                , label =
                    Input.labelLeft
                        [ Font.color <| rgba255 250 250 250 1.0, width <| Element.fillPortion 1 ]
                        (Element.text
                            ("Default Image Rotation = "
                                ++ String.fromFloat ((defaultRotation * 360) |> round |> toFloat)
                                ++ (String.fromChar <| degreesSymbol)
                            )
                        )
                , min = 0
                , max = 1
                , value = defaultRotation
                , thumb = Input.defaultThumb
                , step = Just rotationGranularity
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
                { onChange =
                    \newZoom ->
                        UpdatePreferences
                            { preferences | defaultZoom = newZoom }
                , label =
                    Input.labelLeft
                        [ Font.color <| rgba255 250 250 250 1.0, width <| Element.fillPortion 1 ]
                        (Element.text
                            ("Default Image Zoom = "
                                ++ (String.fromInt << round) (defaultZoom * 100)
                                ++ (String.fromChar <| percentSymbol)
                            )
                        )
                , min = zoomGranularity
                , max = 5
                , value = defaultZoom
                , thumb = Input.defaultThumb
                , step = Just zoomGranularity
                }
            , Input.radio
                [ Font.color <| rgba255 250 250 250 1.0, width <| Element.fillPortion 4 ]
                { onChange = \scroll -> UpdatePreferences { preferences | infinityScroll = scroll }
                , options =
                    [ Input.optionWith Vertical
                        (\optionState ->
                            Element.row [ Element.spacing 12 ]
                                [ Element.el [ Element.alignRight ] <|
                                    Input.defaultCheckbox (optionState == Input.Selected)
                                , Element.el [ Element.alignLeft ] <| text "Vertical"
                                ]
                        )
                    , Input.optionWith Horizontal
                        (\optionState ->
                            Element.row [ Element.spacing 12 ]
                                [ Element.el [ Element.alignRight ] <|
                                    Input.defaultCheckbox (optionState == Input.Selected)
                                , Element.el [ Element.alignLeft ] <| text "Horizontal"
                                ]
                        )
                    ]
                , selected = Just infinityScroll
                , label =
                    Input.labelLeft
                        [ Font.color <| rgba255 250 250 250 1.0, width <| Element.fillPortion 1 ]
                        (Element.text "Infinity Mode Scroll Direction")
                }

            -- , Element.row [ width fill, height Element.fill ]
            --     [ Element.el [ width <| Element.fillPortion 1, Font.color <| rgb 1 1 1 ] <|
            --         Element.text "Keyboard Controls"
            --     , Element.el
            --         [ width <| Element.fillPortion 4 ]
            --         (keyboardMappingPreferences keyboardControls)
            --     ]
            ]


keyboardMappingsView : String -> List ( String, List String ) -> Element Msg
keyboardMappingsView headingText mappings =
    Element.column [ width fill ]
        [ Element.el
            [ width fill, Font.color <| rgb 1 1 1 ]
            (headingText |> text)
        , mappings |> mappingsEditor
        ]


mappingEditor : ( String, List String ) -> Element Msg
mappingEditor ( key, values ) =
    let
        capitalizedKey =
            case String.uncons key of
                Nothing ->
                    ""

                Just ( head, tail ) ->
                    (Char.toUpper >> String.fromChar) head ++ tail
    in
    Element.row [ width fill, Element.padding 5 ]
        [ Element.el
            [ width <| fillPortion 1
            , Font.color <| rgb 1 1 1
            , Element.paddingXY 26 0
            ]
          <|
            Element.text capitalizedKey
        , Input.text [ width <| fillPortion 6, height (30 |> px), Font.size 16 ]
            { onChange = always <| UpdateView <| Preview (Catalog Nothing)
            , text = List.foldl ((++) " " >> (++)) "" values
            , placeholder = Nothing
            , label = Input.labelHidden key
            }
        ]


mappingsEditor : List ( String, List String ) -> Element Msg
mappingsEditor mappings =
    Element.column
        [ width fill, height fill ]
        [ Element.column [ width fill ]
            (List.map
                mappingEditor
                mappings
            )
        ]


keyboardMappingPreferences : KeyboardMappings -> Element Msg
keyboardMappingPreferences { slideshowMap, preferencesMap, previewMap } =
    Element.column
        [ width fill, height fill ]
        [ keyboardMappingsView "Slideshow View"
            [ ( "next", slideshowMap.next )
            , ( "prev", slideshowMap.prev )
            , ( "exit", slideshowMap.exit )
            , ( "toggle", slideshowMap.toggle )
            ]
        , keyboardMappingsView "Preferences View"
            [ ( "exit", preferencesMap.exit )
            ]
        , keyboardMappingsView "Preview View"
            [ ( "openCurrent", previewMap.openCurrent )
            , ( "closeCurrent", previewMap.closeCurrent )
            , ( "startSlideshow", previewMap.startSlideshow )
            ]
        ]


filePreviewView :
    List ( ImageKey, ReadyImage )
    ->
        { r
            | imagesPerRow : Int
            , backgroundColor : Element.Color
            , imageSelection : Maybe (List ImageKey)
        }
    -> Element Msg
filePreviewView images { imagesPerRow, backgroundColor, imageSelection } =
    Element.column
        [ width fill
        , height fill
        , Background.color backgroundColor
        , Element.spacing 5
        , Element.padding 5
        ]
        (images
            |> List.sortBy (Tuple.mapSecond .imageUrl)
            |> List.greedyGroupsOf imagesPerRow
            |> List.map
                (\group ->
                    let
                        paddingElements =
                            List.repeat
                                (imagesPerRow - List.length group)
                                (Element.el [ width fill, height fill ] Element.none)
                    in
                    Element.row
                        [ Element.spaceEvenly
                        , Element.spacing 10
                        , width fill
                        ]
                        (group
                            |> List.map (previewImage (imageSelection |> Maybe.withDefault []))
                            |> (\imageElements -> List.append imageElements paddingElements)
                        )
                )
        )


iconElement style =
    Icon.toHtml style >> Element.html


squareXIconControl msg =
    Icon.x
        |> iconElement [ Svg.color "#C00000" ]
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


previewImageControls otherSelected imageKey =
    Element.column
        [ width fill
        , height fill
        , Element.padding 8
        , Background.color <| Element.rgba 0.2 0.2 0.2 0.3
        , Element.transparent <| True
        , Element.mouseOver [ Element.transparent <| False ]
        ]
        [ squareXIconControl <| RemoveImage imageKey
        , expandIconControl <| UpdateView <| Preview <| Focused ( imageKey, otherSelected )
        ]
        |> Element.el [ width fill, height fill ]


previewImage : List ImageKey -> ( ImageKey, ReadyImage ) -> Element Msg
previewImage otherSelected ( imageKey, { imageUrl } ) =
    let
        orderedSelected =
            otherSelected
                |> List.splitWhen ((==) imageKey)
                |> Maybe.andThen (\( head, tail ) -> Just (List.append (tail |> List.remove imageKey) head))
                |> Maybe.withDefault otherSelected

        mouseOverScaleFactor =
            1.035
    in
    Element.el
        [ width fill
        , height fill
        , centerX
        , centerY
        , mouseOver [ scale mouseOverScaleFactor ]
        ]
        (Element.image
            [ width fill
            , height fill
            , centerX
            , centerY
            , inFront <| previewImageControls orderedSelected imageKey
            , elementId imageKey
            ]
            { src = imageUrl
            , description = ""
            }
        )


expandedImage :
    ( ImageKey, ReadyImage )
    -> { viewport | height : Float, width : Float }
    -> { options | defaultRotation : Float, defaultZoom : Float }
    -> Element Msg
expandedImage ( imageKey, image ) viewport options =
    let
        imageUrl =
            .imageUrl image

        padding =
            24

        totalPadding =
            padding * 2
    in
    Element.el
        [ width fill
        , height fill
        , Background.color (Element.rgba 0.1 0.1 0.1 0.9)
        , Element.padding padding
        , elementId imageKey
        ]
        (Element.el
            [ width fill
            , height fill
            , inFront <| squareXIconControl <| UpdateView <| Preview <| Catalog Nothing
            ]
            (Element.el
                [ width ((viewport.width - totalPadding) |> round >> px)
                , height ((viewport.height - totalPadding) |> round >> px)
                ]
                (viewportBoundImage image
                    { viewport
                        | width = viewport.width - totalPadding
                        , height = viewport.height - totalPadding
                    }
                    options
                )
            )
        )


imagePxDimensions :
    { element | width : Float, height : Float }
    -> ( Element.Length, Element.Length )
imagePxDimensions { width, height } =
    let
        toPx =
            round >> px
    in
    Tuple.mapBoth toPx toPx ( width, height )


scaleImageToViewport :
    { viewport | width : Float, height : Float }
    -> { image | width : Float, height : Float }
    -> { width : Float, height : Float }
scaleImageToViewport viewport image =
    let
        ( widthRatio, heightRatio ) =
            ( image.width / viewport.width
            , image.height / viewport.height
            )

        heightLimited =
            widthRatio < heightRatio
    in
    case heightLimited of
        True ->
            { width = image.width / heightRatio, height = image.height / heightRatio }

        False ->
            { width = image.width / widthRatio, height = image.height / widthRatio }


slideshowViewElement :
    ReadyImage
    -> { viewport | width : Float, height : Float }
    -> { options | defaultRotation : Float, defaultZoom : Float }
    -> Element Msg
slideshowViewElement =
    viewportBoundImage


{-| Image within a container of specific dimensions taken from provided viewport
Rotation will resize to keep image in bounds while maximizing size
Zoom relative to the viewport size. Enlarging will cause image to overflow bounds and
shrinking will introduce spacing within the boarders of viewport
-}
viewportBoundImage :
    ReadyImage
    -> { viewport | width : Float, height : Float }
    -> { options | defaultRotation : Float, defaultZoom : Float }
    -> Element Msg
viewportBoundImage image viewport { defaultRotation, defaultZoom } =
    let
        defaultDimensions =
            ( fill, fill )

        rotation =
            image.rotation |> Maybe.withDefault defaultRotation

        radians =
            turns rotation

        zoom =
            image.zoom |> Maybe.withDefault defaultZoom

        scaledImageDimensions =
            image.nativeDimensions |> scaleImageToViewport viewport

        ( imageWidth, imageHeight ) =
            scaledImageDimensions |> imagePxDimensions

        scalePostRotation =
            scaledImageDimensions
                |> getRotatedDimensions rotation
                |> (\rotated ->
                        rotated
                            |> scaleImageToViewport viewport
                            |> (\{ width } -> width / rotated.width)
                   )
    in
    Element.image
        [ width imageWidth
        , height imageHeight
        , rotate radians
        , centerX
        , centerY
        , scale <| (*) zoom <| scalePostRotation
        ]
        { src = image.imageUrl, description = "Current Slide Image" }
        |> Element.el [ width fill, height fill ]


infinityReadingView :
    { state
        | images : List ( ImageKey, ReadyImage )
        , viewport : Dom.Viewport
        , defaultRotation : Float
        , defaultZoom : Float
        , scroll : InfinityScroll
    }
    -> Element Msg
infinityReadingView ({ images, viewport, scroll } as options) =
    let
        imageList =
            images
                |> List.sortBy Tuple.first
                |> List.map Tuple.second

        viewport_ =
            viewport.viewport

        commonContainerAttr =
            [ Element.spacing 1 ]

        container =
            case scroll of
                Vertical ->
                    Element.column (centerX :: commonContainerAttr)

                Horizontal ->
                    Element.row (centerY :: commonContainerAttr)
    in
    container
        (imageList
            |> List.map (\image -> viewportBoundImage image viewport_ options)
        )
