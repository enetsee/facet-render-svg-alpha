module Facet.Render.Svg.Rect exposing (render)

import Facet.Scenegraph.Mark as Mark
import Facet.Render.Svg.Cursor as Cursor
import Facet.Render.Svg.Fill as Fill
import Facet.Render.Svg.Helpers exposing (px)
import Facet.Render.Svg.Position as Position
import Facet.Render.Svg.Stroke as Stroke
import Svg exposing (Svg, Attribute, rect)
import Svg.Attributes as A


render : Mark.Rect -> Svg msg
render mark =
    let
        ( rx, ry ) =
            let
                rStr =
                    px mark.cornerRadius
            in
                ( A.rx rStr, A.ry rStr )

        ( x, width ) =
            Position.xWidth mark.x

        ( y, height ) =
            Position.yHeight mark.y

        fillAttrs =
            Fill.fill mark.fill

        strokeAttrs =
            Stroke.stroke mark.stroke

        cursor =
            Cursor.cursor mark.cursor

        attrs =
            [ Just x
            , Just width
            , Just y
            , Just height
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
                |> List.filterMap identity
    in
        rect attrs []
