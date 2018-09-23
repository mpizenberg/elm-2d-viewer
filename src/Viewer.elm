module Viewer exposing
    ( Viewer, withSize
    , coordinatesAt, coordinatesAtCenter, coordinatesInViewer
    , centerAtCoordinates, translate, pan
    , fitImage, rescaleCentered, zoomIn, zoomOut, rescaleFixPoint, zoomToward, zoomAwayFrom
    )

{-| A 2D viewer, providing helper functions for zooming and translations.


# The Viewer type

@docs Viewer, withSize


# Viewer properties

@docs coordinatesAt, coordinatesAtCenter, coordinatesInViewer


# Viewer translations

@docs centerAtCoordinates, translate, pan


# Viewer scaling

@docs fitImage, rescaleCentered, zoomIn, zoomOut, rescaleFixPoint, zoomToward, zoomAwayFrom

-}


{-| A viewer has:

  - a size, provided by its context
  - an origin, corresponding to the image coordinates at the top left corner
  - a scale, which is a multiplier coefficient from viewer coordinates to image coordinates.

Its size (or dimension) corresponds to the size, in CSS pixels that the viewer
is taking in the DOM. It has to be kept up to date if the viewer dimensions change
due to responsive layout.

Contrary to the size, the origin is given in "image" coordinates,
i.e. the coordinates sytem of the content displayed in the viewer.

The scale acts as a multiplier coefficient between the viewer coordinates
(in CSS pixels) and the image coordinates. This means that for any two points,
with a distance of `d` pixels between each other in the web page, their distance
in the image coordinates is actually `scale * d`.

-}
type alias Viewer =
    { size : ( Float, Float )
    , origin : ( Float, Float )
    , scale : Float
    }



-- Constructors


{-| Initialize a viewer of a given size.
Default origin is (0,0) and scale 1.0.
-}
withSize : ( Float, Float ) -> Viewer
withSize size =
    { size = size
    , origin = ( 0.0, 0.0 )
    , scale = 1.0
    }



-- PROPERTIES ########################################################


{-| Transform viewer coordinates into image coordinates.

    Viewer.coordinatesAt ( 0, 0 ) viewer
        == viewer.origin

    Viewer.coordinatesAt ( x, y ) viewer
        == (viewer.origin + viewer.scale * ( x, y ))

-}
coordinatesAt : ( Float, Float ) -> Viewer -> ( Float, Float )
coordinatesAt ( x, y ) { origin, scale } =
    let
        ( ox, oy ) =
            origin
    in
    ( ox + scale * x
    , oy + scale * y
    )


{-| Image coordinates at the center of the viewer.

    Viewer.coordinatesAtCenter viewer
        == Viewer.coordinatesAt (0.5 * viewer.size)

-}
coordinatesAtCenter : Viewer -> ( Float, Float )
coordinatesAtCenter viewer =
    let
        ( width, height ) =
            viewer.size
    in
    coordinatesAt ( 0.5 * width, 0.5 * height ) viewer


{-| Transform image coordinates into viewer coordinates.
Inverse of `coordinatesAt`.

    Viewer.coordinatesAt ( x, y ) viewer
        |> Viewer.coordinatesInViewer viewer
        == ( x, y )

-}
coordinatesInViewer : Viewer -> ( Float, Float ) -> ( Float, Float )
coordinatesInViewer viewer ( x, y ) =
    let
        ( ox, oy ) =
            viewer.origin
    in
    ( x - ox / viewer.scale
    , y - oy / viewer.scale
    )



-- TRANSLATIONS ######################################################


{-| Center the viewer at the given image coordinates.

    Viewer.centerAtCoordinates ( x, y ) viewer
        |> Viewer.coordinatesAtCenter
        == ( x, y )

-}
centerAtCoordinates : ( Float, Float ) -> Viewer -> Viewer
centerAtCoordinates ( x, y ) ({ size, scale } as viewer) =
    let
        ( width, height ) =
            size
    in
    { viewer | origin = ( x - 0.5 * scale * width, y - 0.5 * scale * height ) }


{-| Translate the viewer by the given image vector.

    .origin (Viewer.translate ( tx, ty ) viewer)
        == (viewer.origin + ( tx, ty ))

-}
translate : ( Float, Float ) -> Viewer -> Viewer
translate ( tx, ty ) viewer =
    let
        ( ox, oy ) =
            viewer.origin
    in
    { viewer | origin = ( ox + tx, oy + ty ) }


{-| Operate the translation on the viewer that would occur
if you make a pan gesture (touch hold and move).
Let (px, py) be the pan displacement in viewer coordinates, then:

    Viewer.pan ( px, py ) viewer
        == Viewer.translate (viewer.scale * ( -px, -py )) viewer

-}
pan : ( Float, Float ) -> Viewer -> Viewer
pan ( px, py ) ({ scale } as viewer) =
    translate ( -scale * px, -scale * py ) viewer



-- SCALING ###########################################################


{-| Rescale the viewer such that it fits an image of a given dimension
( width, height ). To get the full image perfectly centered and zoomed
to fit in the viewer, you can use:

    Viewer.fitImage 1.0 ( width, height ) viewer

If you want to have some margin around the image area,
it means that you want the image a bit smaller, so the viewer
a bit bigger. For 10% margin, for example, you can use:

    Viewer.fitImage 1.1 ( width, height ) viewer

> Remarq: This does not resize the viewer or the image,
> only changes the origin and scale of the viewer.

-}
fitImage : Float -> ( Float, Float ) -> Viewer -> Viewer
fitImage rescale ( width, height ) viewer =
    let
        ( viewerWidth, viewerHeight ) =
            viewer.size

        newScale =
            rescale * max (width / viewerWidth) (height / viewerHeight)
    in
    { viewer | scale = newScale }
        |> centerAtCoordinates ( width / 2.0, height / 2.0 )


{-| Rescale the viewer, keeping the center point fix.
-}
rescaleCentered : Float -> Viewer -> Viewer
rescaleCentered scale viewer =
    { viewer | scale = scale }
        |> centerAtCoordinates (coordinatesAtCenter viewer)


{-| Zoom in, keeping the center point fix.
-}
zoomIn : Viewer -> Viewer
zoomIn viewer =
    rescaleCentered (zoomInCoef * viewer.scale) viewer


{-| Zoom out, keeping the center point fix.
-}
zoomOut : Viewer -> Viewer
zoomOut viewer =
    rescaleCentered (zoomOutCoef * viewer.scale) viewer


{-| Rescale the viewer about a given fix point.
Very convinient to keep the point under the mouse fix for example.
This is equivalent to `rescaleCentered` if the fix point is the center point.

    Viewer.rescaleFixPoint scale (Viewer.coordinatesAtCenter viewer) viewer
        == Viewer.rescaleCentered scale viewer

-}
rescaleFixPoint : Float -> ( Float, Float ) -> Viewer -> Viewer
rescaleFixPoint scale (( x, y ) as coordinates) viewer =
    let
        ( viewerX, viewerY ) =
            coordinatesInViewer viewer coordinates

        ( ox, oy ) =
            viewer.origin

        newOrigin =
            ( x - scale * viewerX
            , y - scale * viewerY
            )
    in
    { viewer | origin = newOrigin, scale = scale }


{-| Zoom in, keeping a given fix point.
Very convinient to keep the point under the mouse fix for example.
This is equivalent to `zoomIn` if the fix point is the center point.

    Viewer.zoomToward (Viewer.coordinatesAtCenter viewer) viewer
        == Viewer.zoomIn viewer

-}
zoomToward : ( Float, Float ) -> Viewer -> Viewer
zoomToward coordinates viewer =
    rescaleFixPoint (zoomInCoef * viewer.scale) coordinates viewer


{-| Zoom out, keeping a given fix point.
Very convinient to keep the point under the mouse fix for example.
This is equivalent to `zoomOut` if the fix point is the center point.

    Viewer.zoomAwayFrom (Viewer.coordinatesAtCenter viewer) viewer
        == Viewer.zoomOut viewer

-}
zoomAwayFrom : ( Float, Float ) -> Viewer -> Viewer
zoomAwayFrom coordinates viewer =
    rescaleFixPoint (zoomOutCoef * viewer.scale) coordinates viewer


zoomInCoef : Float
zoomInCoef =
    2.0 / 3.0


zoomOutCoef : Float
zoomOutCoef =
    1.0 / zoomInCoef
