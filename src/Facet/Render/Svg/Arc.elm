module Facet.Render.Svg.Arc exposing (render)

import Facet.Scenegraph.Mark as Mark
import Svg exposing (Svg)


{-| TODO!
-}
render : Mark.Arc -> Svg msg
render mark =
    Svg.g [] []
