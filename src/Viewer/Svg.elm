module Viewer.Svg exposing (transform)

{-| Helper module to transform SVG drawings with a Viewer.

@docs transform

-}

import Svg
import Svg.Attributes
import Viewer exposing (Viewer)


{-| Generate the transform attribute from a viewer
-}
transform : Viewer -> Svg.Attribute msg
transform viewer =
    Svg.Attributes.transform (transformString viewer.scale viewer.origin)


transformString : Float -> ( Float, Float ) -> String
transformString scale ( ox, oy ) =
    String.concat
        [ "translate("
        , String.fromFloat -ox
        , " "
        , String.fromFloat -oy
        , ") scale("
        , String.fromFloat (1.0 / scale)
        , ")"
        ]
