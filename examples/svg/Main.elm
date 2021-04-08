module Main exposing (main)

import Browser
import Browser.Events
import Element exposing (Element, centerX, fill, height, padding, width)
import Element.Background
import Element.Input
import FeatherIcons as Icons exposing (Icon)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Svg
import Svg.Attributes
import Viewer exposing (Viewer)
import Viewer.Svg


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
    , image : Image
    }


type alias Size =
    { width : Int
    , height : Int
    }


type alias Image =
    { id : String
    , url : String
    , width : Int
    , height : Int
    }


type alias Model =
    { windowSize : Size
    , viewer : Viewer
    , pointerMode : PointerMode
    , image : Image
    }


type PointerMode
    = PointerUp
    | PointerMovingFrom ( Float, Float )


init : Flags -> ( Model, Cmd Msg )
init { windowSize, image } =
    ( initialModel windowSize image, Cmd.none )


initialModel : Size -> Image -> Model
initialModel windowSize image =
    { windowSize = windowSize
    , viewer =
        Viewer.withSize ( toFloat windowSize.width, toFloat windowSize.height )
            |> Viewer.fitImage 1.0 ( toFloat image.width, toFloat image.height )
    , pointerMode = PointerUp
    , image = image
    }



-- Update ############################################################


type Msg
    = WindowResizes Size
    | ZoomMsg ZoomMsg
    | MouseDownMsg ( Float, Float )
    | MouseMoveMsg ( Float, Float )
    | MouseUpMsg ( Float, Float )


type ZoomMsg
    = ZoomFit Image
    | ZoomIn
    | ZoomOut
    | ZoomToward ( Float, Float )
    | ZoomAwayFrom ( Float, Float )


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Only listen to mousemove if there was already a mousedown event.
    case model.pointerMode of
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
    case ( msg, model.pointerMode ) of
        ( WindowResizes size, _ ) ->
            ( { model
                | windowSize = size
                , viewer = Viewer.resize ( toFloat size.width, toFloat size.height ) model.viewer
              }
            , Cmd.none
            )

        ( ZoomMsg zoomMsg, _ ) ->
            ( { model | viewer = zoomViewer zoomMsg model.viewer }, Cmd.none )

        ( MouseDownMsg pos, PointerUp ) ->
            ( { model | pointerMode = PointerMovingFrom pos }, Cmd.none )

        ( MouseMoveMsg ( newX, newY ), PointerMovingFrom ( x, y ) ) ->
            ( { model
                | viewer = Viewer.pan ( newX - x, newY - y ) model.viewer
                , pointerMode = PointerMovingFrom ( newX, newY )
              }
            , Cmd.none
            )

        ( MouseUpMsg _, PointerMovingFrom _ ) ->
            ( { model | pointerMode = PointerUp }, Cmd.none )

        _ ->
            ( model, Cmd.none )


zoomViewer : ZoomMsg -> Viewer -> Viewer
zoomViewer msg viewer =
    case msg of
        ZoomFit img ->
            Viewer.fitImage 1.0 ( toFloat img.width, toFloat img.height ) viewer

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
view { viewer, image } =
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
            [ clickButton (ZoomMsg (ZoomFit image)) "Fit zoom to image" (featherIcon Icons.maximize 32)
            , clickButton (ZoomMsg ZoomOut) "Zoom out" (featherIcon Icons.zoomOut 32)
            , clickButton (ZoomMsg ZoomIn) "Zoom in" (featherIcon Icons.zoomIn 32)
            ]

        imgSvgAttributes =
            [ Svg.Attributes.xlinkHref image.url
            , Svg.Attributes.width (String.fromInt image.width)
            , Svg.Attributes.height (String.fromInt image.height)
            , Svg.Attributes.class "pixelated"
            ]

        ( viewerWidth, viewerHeight ) =
            viewer.size

        svgViewer =
            Svg.svg
                [ Html.Attributes.width (floor viewerWidth)
                , Html.Attributes.height (floor viewerHeight)
                , Html.Events.preventDefaultOn "wheel"
                    (Decode.map (\msg -> ( msg, True )) (wheelDecoder viewer))
                ]
                [ Svg.g
                    [ Html.Attributes.style "pointer-events" "none"
                    , Viewer.Svg.transform viewer
                    ]
                    [ Svg.image imgSvgAttributes [] ]
                ]
    in
    Element.layout [ Element.clip ] <|
        Element.column [ width fill, height fill ]
            [ Element.html <|
                Html.node "style" [] [ Html.text ".pixelated { image-rendering: pixelated; image-rendering: crisp-edges; }" ]
            , Element.el
                [ Element.inFront (Element.row [ centerX ] zoomButtons)
                , Element.clip
                , height fill
                ]
                (Element.html svgViewer)
            ]


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
