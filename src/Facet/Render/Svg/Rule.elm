module Facet.Render.Svg.Rule exposing (render)

import Facet.Scenegraph.Mark as Mark
import Facet.Render.Svg.Cursor as Cursor
import Facet.Render.Svg.Position as Position
import Facet.Render.Svg.Stroke as Stroke
import Svg exposing (Svg, Attribute, line)


render : Mark.Rule -> Svg msg
render mark =
    let
        ( x, x2 ) =
            Position.xPrimarySecondary mark.x

        ( y, y2 ) =
            Position.yPrimarySecondary mark.y

        strokeAttrs =
            Stroke.stroke mark.stroke

        cursor =
            Cursor.cursor mark.cursor

        attrs =
            List.filterMap identity
                [ Just x
                , Just x2
                , Just y
                , Just y2
                , Just cursor
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
        line attrs []
