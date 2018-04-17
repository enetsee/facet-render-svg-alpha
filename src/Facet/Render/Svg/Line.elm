module Facet.Render.Svg.Line exposing (render)

import Facet.Scenegraph.Mark as Mark exposing (Behaviour(..))
import Facet.Render.Svg.Cursor as Cursor
import Facet.Render.Svg.LineLike exposing (points, curve)
import Facet.Render.Svg.Stroke as Stroke
import Path
import Svg exposing (Svg, Attribute)
import Svg.Attributes as A


render : Mark.Line -> Svg msg
render mark =
    let
        strokeAttrs =
            Stroke.stroke mark.stroke

        fill =
            A.fill "none"

        cursor =
            Cursor.cursor mark.cursor

        pathDef =
            points mark.behaviour mark.x mark.y
                |> List.map (curve mark.interpolate)
                |> Path.toString
                |> A.d

        attrs =
            List.filterMap identity
                [ Just pathDef
                , Just cursor
                , Just fill
                , Just strokeAttrs.stroke
                , strokeAttrs.strokeOpacity
                , strokeAttrs.strokeWidth
                , strokeAttrs.strokeLineCap
                , strokeAttrs.strokeDashArray
                , strokeAttrs.strokeDashOffset
                , strokeAttrs.strokeLineJoin
                , strokeAttrs.strokeMiterLimit
                ]
    in
        Svg.path attrs []
