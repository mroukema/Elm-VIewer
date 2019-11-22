module Main exposing (main)

import Basics exposing (not, pi)
import Browser
import Browser.Events as Browser
import Color exposing (toRGB)
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



-- Model


type Model
    = Model Data Preferences ViewState


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


type ViewModel
    = ImageList
        (List ( ImageKey, ImageUrl ))
        { imagesPerRow : Int
        , backgroundColor : Element.Color
        }
    | Slideshow ImageUrl { backgroundColor : Element.Color }
    | EditPreferences Preferences


type Msg
    = NoOp
    | ImagesRequested
    | ImageFilesReceived File (List File)
    | ImageUrlEncoding String (Result () ImageUrl)
    | StepSlideshow Direction
    | TogglePauseSlideshow
    | SetSlideshowInterval Float
    | SetBackgroundColor Element.Color
    | SetElementsPerRow Int
    | ChangeView ViewState


type alias ImageKey =
    String


type alias ImageUrl =
    String


type Direction
    = Forward
    | Backward


defaultPreferences : Preferences
defaultPreferences =
    { slideshowSpeed = 3 * 1000
    , previewItemsPerRow = 8
    , backgroundColor = List.getAt 1 (List.map (toRGB >> (\( r, g, b ) -> rgb255 r g b)) colorPalette) |> Maybe.withDefault (rgb 0 0 0)
    }


rgb255 : Float -> Float -> Float -> Element.Color
rgb255 r g b =
    rgb (r / 255) (g / 255) (b / 255)


rgbTuple : ( Float, Float, Float ) -> Element.Color
rgbTuple ( r, g, b ) =
    rgb255 r g b



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

        ImagesRequested ->
            ( model, Select.files [ "image/png", "image/jpg" ] ImageFilesReceived )

        ImageFilesReceived file otherFiles ->
            ( model
            , List.map getFileAsImageUrl (file :: otherFiles) |> Cmd.batch
            )

        ImageUrlEncoding filename result ->
            case result of
                Ok imageUrl ->
                    case model of
                        Model images preferences state ->
                            ( Model (Dict.insert filename imageUrl images) preferences state, Cmd.none )

                Err _ ->
                    noop

        StepSlideshow direction ->
            case model of
                Model images preferences state ->
                    case state of
                        SlideshowView slideshowState ->
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
                                    ( Model
                                        images
                                        preferences
                                        (SlideshowView { slideshowState | slidelist = stepList slideshowState.slidelist })
                                    , Cmd.none
                                    )

                                Backward ->
                                    ( Model images
                                        preferences
                                        (SlideshowView
                                            { slideshowState
                                                | slidelist =
                                                    List.reverse <| stepList <| List.reverse slideshowState.slidelist
                                            }
                                        )
                                    , Cmd.none
                                    )

                        _ ->
                            noop

        TogglePauseSlideshow ->
            case model of
                Model data preferences state ->
                    case state of
                        SlideshowView slideshowState ->
                            ( Model
                                data
                                preferences
                                (SlideshowView { slideshowState | running = toggleRunning slideshowState })
                            , Cmd.none
                            )

                        _ ->
                            noop

        SetBackgroundColor color ->
            case model of
                Model data preferences state ->
                    ( Model data { preferences | backgroundColor = color } state, Cmd.none )

        SetSlideshowInterval newInterval ->
            case model of
                Model data preferences state ->
                    ( Model data { preferences | slideshowSpeed = newInterval } state, Cmd.none )

        SetElementsPerRow count ->
            case model of
                Model data preferences state ->
                    ( Model data { preferences | previewItemsPerRow = count } state, Cmd.none )

        ChangeView newState ->
            case model of
                Model data preferences _ ->
                    ( Model data preferences newState, Cmd.none )


colorPalette =
    Cubehelix.generate 20


toggleRunning =
    not << .running


getFileAsImageUrl : File -> Cmd Msg
getFileAsImageUrl file =
    Task.attempt
        (ImageUrlEncoding (File.name file))
        (File.toUrl file)


getFromDict : Dict comparable value -> comparable -> Maybe value
getFromDict dict key =
    Dict.get key dict


gotoSlideshowView { running, slidelist } =
    ChangeView <| SlideshowView { running = running, slidelist = slidelist }


startSlideshow slides =
    ChangeView <| SlideshowView { running = True, slidelist = slides }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Model data { slideshowSpeed } state ->
            case state of
                SlideshowView { running } ->
                    let
                        navigationListeners =
                            [ Browser.onKeyPress <| msgWhen isSpace (always TogglePauseSlideshow)
                            , Browser.onKeyUp <| msgWhen isNavKey StepSlideshow
                            , Browser.onKeyUp <| msgWhen isEsc (always <| ChangeView PreviewView)
                            ]
                    in
                    case running of
                        True ->
                            Sub.batch
                                (Time.every slideshowSpeed (always <| StepSlideshow Forward)
                                    :: navigationListeners
                                )

                        False ->
                            Sub.batch navigationListeners

                PreviewView ->
                    Browser.onKeyPress <|
                        msgWhen isSpace
                            (always <| startSlideshow (Dict.keys data))

                PreferencesView ->
                    Browser.onKeyUp <| msgWhen isEsc (always <| ChangeView PreviewView)


msgWhen : (String -> Json.Decoder b) -> (b -> Msg) -> Json.Decoder Msg
msgWhen keyDecoder msg =
    Json.map msg (Json.field "key" Json.string |> Json.andThen keyDecoder)


toDirection : String -> Maybe Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            Just Backward

        "ArrowRight" ->
            Just Forward

        _ ->
            Nothing


isEsc string =
    case string of
        "Escape" ->
            Json.succeed string

        _ ->
            Json.fail "Not the Escape Key"


isNavKey string =
    case toDirection string of
        Just direction ->
            Json.succeed direction

        Nothing ->
            Json.fail "Not a Direction Key"


isSpace string =
    case string of
        " " ->
            Json.succeed " "

        _ ->
            Json.fail "Not the Space Key"



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
                    ImageList imageList { imagesPerRow = preferences.previewItemsPerRow, backgroundColor = preferences.backgroundColor }

                SlideshowView { running, slidelist } ->
                    case List.filterMap (getFromDict data) slidelist of
                        [] ->
                            ImageList imageList { imagesPerRow = preferences.previewItemsPerRow, backgroundColor = preferences.backgroundColor }

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
            Element.row [ Element.width fill, Background.color <| headerBackground, Element.spaceEvenly, Element.padding 5 ]
                [ Element.text ""
                , Element.text "Preview View" |> Element.el [ onClick <| ChangeView PreviewView, Font.color fontColor ]
                , Element.text "Select Images" |> Element.el [ onClick ImagesRequested, Font.color fontColor ]
                ]

        ImageList imageUrls _ ->
            Element.row [ Element.width fill, Background.color <| headerBackground, Element.spaceEvenly, Element.padding 5 ]
                [ Element.text "Start Slideshow" |> Element.el [ onClick <| startSlideshow <| List.map Tuple.first imageUrls, Font.color fontColor ]
                , Element.text "Preferences" |> Element.el [ onClick <| ChangeView PreferencesView, Font.color fontColor ]
                , Element.text "Select Images" |> Element.el [ onClick ImagesRequested, Font.color fontColor ]
                ]

        _ ->
            Element.row [ Element.width fill, Background.color <| rgba255 220 220 220 0.5, Element.spaceEvenly, Element.padding 5 ]
                [ Element.text ""
                , Element.text "Preferences" |> Element.el [ onClick <| ChangeView PreferencesView, Font.color fontColor ]
                , Element.text "Select Images" |> Element.el [ onClick ImagesRequested, Font.color fontColor ]
                ]


imageViewer : ViewModel -> Html Msg
imageViewer model =
    let
        content =
            case model of
                ImageList images preferences ->
                    Element.column [ Element.width fill, Element.height fill ]
                        [ imageHeader model
                        , filePreviewView (List.map Tuple.second images) preferences
                        ]

                Slideshow currentImage { backgroundColor } ->
                    slideshowView currentImage backgroundColor

                EditPreferences preferences ->
                    Element.column [ Element.width fill, Element.height fill ]
                        [ imageHeader model
                        , editPreferencesView preferences
                        ]
    in
    content
        |> Element.layout
            [ Element.height fill
            , Element.width fill
            , Background.color defaultPreferences.backgroundColor
            ]


colorPicker =
    colorPalette
        |> List.map
            (toRGB
                >> (\( r, g, b ) -> rgb255 r g b)
                >> colorBox
            )


colorBox color =
    Element.el [ Background.color color, width fill, height fill, onClick <| SetBackgroundColor color ] <| Element.column [] []


editPreferencesView : Preferences -> Element Msg
editPreferencesView { slideshowSpeed, backgroundColor, previewItemsPerRow } =
    Element.el [ Element.width fill, Element.height fill, Background.color backgroundColor, Element.spacing 50, Element.padding 20 ] <|
        Element.column [ Element.width Element.fill, Element.padding 35, Background.color <| rgba255 0 0 0 0.5, Element.spacing 10 ]
            [ Input.slider
                [ Element.width <| Element.fillPortion 4
                , Element.behindContent <| Element.el [ Background.color <| rgba255 255 255 255 1, Element.height (5 |> px), Element.width fill, Element.centerY ] Element.none
                ]
                { onChange = SetSlideshowInterval
                , label =
                    Input.labelLeft
                        [ Font.color <| rgba255 250 250 250 1.0, Element.width <| Element.fillPortion 1 ]
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
                [ Element.width <| Element.fillPortion 4
                , Element.behindContent <| Element.el [ Background.color <| rgba255 255 255 255 1, Element.height (5 |> px), Element.width fill, Element.centerY ] Element.none
                ]
                { onChange = round >> SetElementsPerRow
                , label =
                    Input.labelLeft
                        [ Font.color <| rgba255 250 250 250 1.0, Element.width <| Element.fillPortion 1 ]
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
                , Element.row [ width <| Element.fillPortion 4, height (20 |> px) ] colorPicker
                ]
            ]


filePreviewView : List ImageUrl -> { r | imagesPerRow : Int, backgroundColor : Element.Color } -> Element Msg
filePreviewView files preferences =
    List.greedyGroupsOf preferences.imagesPerRow files
        |> List.map
            (List.map singleFileView
                >> Element.row
                    [ Element.spaceEvenly
                    , Element.spacing 5
                    , Element.width fill

                    --, Element.rotate (Basics.pi / 2)
                    ]
            )
        |> Element.column
            [ Element.width fill
            , Element.height fill
            , Background.color preferences.backgroundColor
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
        , Element.width fill
        , Element.height fill
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


singleFileView fileSrc =
    Element.image [ Element.width fill, Element.height fill, Element.centerX, Element.centerY ]
        { src = fileSrc
        , description = ""
        }
