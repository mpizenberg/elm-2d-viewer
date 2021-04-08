module Main exposing (main)

import Browser
import Browser.Events
import Canvas
import Canvas.Settings.Advanced
import Canvas.Texture exposing (Texture)
import Element exposing (Element, centerX, fill, height, padding, width)
import Element.Background
import Element.Input
import FeatherIcons as Icons exposing (Icon)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder, Value)
import Viewer exposing (Viewer)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { windowSize : Size
    , image : Value
    }


type alias Size =
    { width : Int
    , height : Int
    }


type Model
    = ImageLoadingError
    | ImageLoaded State


type alias State =
    { windowSize : Size
    , viewer : Viewer
    , pointerMode : PointerMode
    , texture : Texture
    }


type PointerMode
    = PointerUp
    | PointerMovingFrom ( Float, Float )


init : Flags -> ( Model, Cmd Msg )
init { windowSize, image } =
    ( initialModel windowSize image, Cmd.none )


initialModel : Size -> Value -> Model
initialModel windowSize image =
    case Canvas.Texture.fromDomImage image of
        Nothing ->
            ImageLoadingError

        Just texture ->
            let
                imgSize =
                    Canvas.Texture.dimensions texture

                viewer =
                    Viewer.withSize ( toFloat windowSize.width, toFloat windowSize.height )
                        |> Viewer.fitImage 1.0 ( imgSize.width, imgSize.height )
            in
            ImageLoaded
                { windowSize = windowSize
                , viewer = viewer
                , pointerMode = PointerUp
                , texture = texture
                }



-- Update ############################################################


type Msg
    = WindowResizes Size
    | ZoomMsg ZoomMsg
    | MouseDownMsg ( Float, Float )
    | MouseMoveMsg ( Float, Float )
    | MouseUpMsg ( Float, Float )


type ZoomMsg
    = ZoomFit Texture
    | ZoomIn
    | ZoomOut
    | ZoomToward ( Float, Float )
    | ZoomAwayFrom ( Float, Float )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        ImageLoadingError ->
            Sub.none

        ImageLoaded state ->
            -- Only listen to mousemove if there was already a mousedown event.
            case state.pointerMode of
                PointerUp ->
                    Sub.batch
                        [ Browser.Events.onResize (\w h -> WindowResizes (Size w h))
                        , Browser.Events.onMouseDown (Decode.map MouseDownMsg clientPosDecoder)
                        , Browser.Events.onMouseUp (Decode.map MouseUpMsg clientPosDecoder)
                        ]

                PointerMovingFrom _ ->
                    Sub.batch
                        [ Browser.Events.onResize (\w h -> WindowResizes (Size w h))
                        , Browser.Events.onMouseDown (Decode.map MouseDownMsg clientPosDecoder)
                        , Browser.Events.onMouseUp (Decode.map MouseUpMsg clientPosDecoder)

                        -- This is new
                        , Browser.Events.onMouseMove (Decode.map MouseMoveMsg clientPosDecoder)
                        ]


clientPosDecoder : Decoder ( Float, Float )
clientPosDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        ImageLoadingError ->
            ( model, Cmd.none )

        ImageLoaded state ->
            ( ImageLoaded (updateState msg state), Cmd.none )


updateState : Msg -> State -> State
updateState msg model =
    case ( msg, model.pointerMode ) of
        ( WindowResizes size, _ ) ->
            { model
                | windowSize = size
                , viewer = Viewer.resize ( toFloat size.width, toFloat size.height ) model.viewer
            }

        ( ZoomMsg zoomMsg, _ ) ->
            { model | viewer = zoomViewer zoomMsg model.viewer }

        ( MouseDownMsg pos, PointerUp ) ->
            { model | pointerMode = PointerMovingFrom pos }

        ( MouseMoveMsg ( newX, newY ), PointerMovingFrom ( x, y ) ) ->
            { model
                | viewer = Viewer.pan ( newX - x, newY - y ) model.viewer
                , pointerMode = PointerMovingFrom ( newX, newY )
            }

        ( MouseUpMsg _, PointerMovingFrom _ ) ->
            { model | pointerMode = PointerUp }

        _ ->
            model


zoomViewer : ZoomMsg -> Viewer -> Viewer
zoomViewer msg viewer =
    case msg of
        ZoomFit texture ->
            let
                imgSize =
                    Canvas.Texture.dimensions texture
            in
            Viewer.fitImage 1.0 ( imgSize.width, imgSize.height ) viewer

        ZoomIn ->
            Viewer.zoomIn viewer

        ZoomOut ->
            Viewer.zoomOut viewer

        ZoomToward coordinates ->
            Viewer.zoomToward coordinates viewer

        ZoomAwayFrom coordinates ->
            Viewer.zoomAwayFrom coordinates viewer



-- View ##############################################################


view : Model -> Html Msg
view model =
    case model of
        ImageLoadingError ->
            Html.text "Error loading the image"

        ImageLoaded state ->
            viewState state


viewState : State -> Html Msg
viewState { viewer, texture } =
    let
        clickButton msg title icon =
            Element.Input.button
                [ padding 6
                , Element.Background.color (Element.rgba255 255 255 255 0.8)
                , Element.htmlAttribute (Html.Attributes.style "box-shadow" "none")
                , Element.htmlAttribute (Html.Attributes.title title)
                ]
                { onPress = Just msg
                , label = icon
                }

        zoomButtons =
            [ clickButton (ZoomMsg (ZoomFit texture)) "Fit zoom to image" (featherIcon Icons.maximize 32)
            , clickButton (ZoomMsg ZoomOut) "Zoom out" (featherIcon Icons.zoomOut 32)
            , clickButton (ZoomMsg ZoomIn) "Zoom in" (featherIcon Icons.zoomIn 32)
            ]

        ( viewerWidth, viewerHeight ) =
            viewer.size

        clearCanvas : Canvas.Renderable
        clearCanvas =
            Canvas.clear ( 0, 0 ) viewerWidth viewerHeight

        renderedImage : Canvas.Renderable
        renderedImage =
            let
                scale =
                    1.0 / viewer.scale

                ( ox, oy ) =
                    viewer.origin
            in
            Canvas.texture
                [ Canvas.Settings.Advanced.transform
                    [ Canvas.Settings.Advanced.scale scale scale
                    , Canvas.Settings.Advanced.translate -ox -oy
                    ]
                ]
                ( 0, 0 )
                texture

        canvasViewer =
            Canvas.toHtml ( round viewerWidth, round viewerHeight )
                [ Html.Attributes.id "theCanvas"
                , Html.Attributes.style "pointer-events" "none"
                , Html.Attributes.style "display" "block"
                ]
                [ clearCanvas, renderedImage ]
    in
    Element.layout [ Element.clip, width fill, height fill ] <|
        Element.el
            [ Element.inFront (Element.row [ centerX ] zoomButtons)
            , Element.clip
            , height fill
            , width fill
            , Element.htmlAttribute <|
                Html.Events.preventDefaultOn "wheel"
                    (Decode.map (\msg -> ( msg, True )) (wheelDecoder viewer))
            ]
            (Element.html canvasViewer)


featherIcon : Icon -> Float -> Element msg
featherIcon icon size =
    Element.html (Icons.toHtml [] (Icons.withSize size icon))


wheelDecoder : Viewer -> Decoder Msg
wheelDecoder viewer =
    let
        imageCoords pos =
            Viewer.coordinatesAt pos viewer
    in
    Decode.field "deltaY" Decode.float
        |> Decode.andThen
            (\deltaY ->
                if deltaY > 0 then
                    Decode.map (ZoomMsg << ZoomAwayFrom << imageCoords) clientPosDecoder

                else
                    Decode.map (ZoomMsg << ZoomToward << imageCoords) clientPosDecoder
            )
