module Facet.Render.Svg exposing (render)

{-|
@docs render
-}

import Svg exposing (Svg, Attribute, g)
import Svg.Attributes as A
import Facet.Scenegraph as Scenegraph exposing (Scenegraph, ViewBox)
import Facet.Render.Svg.Arc as Arc
import Facet.Render.Svg.Area as Area
import Facet.Render.Svg.Group as Group
import Facet.Render.Svg.Line as Line
import Facet.Render.Svg.Path as Path
import Facet.Render.Svg.Polygon as Polygon
import Facet.Render.Svg.Rect as Rect
import Facet.Render.Svg.Rule as Rule
import Facet.Render.Svg.Symbol as Symbol
import Facet.Render.Svg.Text as Text
import Facet.Render.Svg.Trail as Trail
import Facet.Render.Svg.Helpers exposing (viewBoxToString, px)


{-| -}
render : Maybe Float -> Maybe Float -> ( ViewBox, Scenegraph ) -> Svg msg
render width height ( viewBox, scenegraph ) =
    let
        screenWidth =
            Maybe.map px width
                |> Maybe.withDefault "100%"
                |> A.width

        screenHeight =
            Maybe.map px height
                |> Maybe.withDefault "100%"
                |> A.height

        styles =
            A.style "shape-rendering:geometricPrecision"

        vb =
            viewBoxToString viewBox |> A.viewBox

        attrs =
            [ screenWidth, screenHeight, vb, styles ]

        elems =
            renderHelper scenegraph
    in
        Svg.svg attrs elems


renderHelper : Scenegraph -> List (Svg msg)
renderHelper scenegraph =
    case scenegraph of
        Scenegraph.Group marks ->
            marks
                |> List.map
                    (\( groupMark, groups ) ->
                        groups
                            |> List.concatMap renderHelper
                            |> Group.render groupMark
                    )

        Scenegraph.Arc marks ->
            List.map Arc.render marks

        Scenegraph.Area marks ->
            List.map Area.render marks

        Scenegraph.Line marks ->
            List.map Line.render marks

        Scenegraph.Polygon marks ->
            List.map Polygon.render marks

        Scenegraph.Path marks ->
            List.map Path.render marks

        Scenegraph.Rect marks ->
            List.map Rect.render marks

        Scenegraph.Rule marks ->
            List.map Rule.render marks

        Scenegraph.Symbol marks ->
            List.map Symbol.render marks

        Scenegraph.Text marks ->
            List.map Text.render marks

        Scenegraph.Trail marks ->
            List.map Trail.render marks
