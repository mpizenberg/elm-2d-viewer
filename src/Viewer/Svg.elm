module Viewer.Svg exposing (placeIn)

{-| Helper module to transform SVG drawings with a Viewer.

@docs placeIn

-}

import Svg exposing (Svg)
import Svg.Attributes
import Viewer exposing (Viewer)


{-| Apply a group transformation to visualize the SVG elements
as specified by the given viewer.
-}
placeIn : Viewer -> List (Svg msg) -> Svg msg
placeIn viewer content =
    let
        ( ox, oy ) =
            viewer.origin

        transformString =
            String.concat <|
                [ "translate("
                , String.fromFloat -ox
                , " "
                , String.fromFloat -oy
                , ") scale("
                , String.fromFloat (1.0 / viewer.scale)
                , ")"
                ]
    in
    Svg.g [ Svg.Attributes.transform transformString ] content
