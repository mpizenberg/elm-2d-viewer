module Viewer.Canvas exposing (transform)

{-| Helper module to transform Canvas drawings with a Viewer.

@docs transform

-}

import Canvas.Settings exposing (Setting)
import Canvas.Settings.Advanced as Advanced
import Viewer exposing (Viewer)


{-| Generate the transform attribute from a viewer
-}
transform : Viewer -> Setting
transform viewer =
    let
        scale =
            1.0 / viewer.scale

        ( ox, oy ) =
            viewer.origin
    in
    Advanced.transform
        [ Advanced.scale scale scale
        , Advanced.translate -ox -oy
        ]
