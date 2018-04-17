module Facet.Render.Svg.Polygon exposing (render)

import Facet.Scenegraph.Mark as Mark
import Facet.Render.Svg.Cursor as Cursor
import Facet.Render.Svg.Fill as Fill
import Facet.Render.Svg.Stroke as Stroke
import Facet.Render.Svg.LineLike exposing (points, curve)
import SubPath as SubPath
import Path
import Svg exposing (Svg)
import Svg.Attributes as A


render : Mark.Polygon -> Svg msg
render mark =
    let
        strokeAttrs =
            Stroke.stroke mark.stroke

        fillAttrs =
            Fill.fill mark.fill

        cursor =
            Cursor.cursor mark.cursor

        pathDef =
            points mark.behaviour mark.x mark.y
                |> List.map
                    (\points ->
                        points
                            |> curve mark.interpolate
                            |> SubPath.close
                    )
                |> Path.toString
                |> A.d

        attrs =
            List.filterMap identity
                [ Just pathDef
                , Just cursor
                , Just fillAttrs.fill
                , fillAttrs.fillOpacity
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
