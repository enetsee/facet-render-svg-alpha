module Facet.Render.Svg.Path exposing (render)

import Facet.Scenegraph.Mark as Mark
import Facet.Render.Svg.Cursor as Cursor
import Facet.Render.Svg.Fill as Fill
import Facet.Render.Svg.Stroke as Stroke
import Facet.Render.Svg.Transform as Transform exposing (Transform(Translate))
import Path.LowLevel as LowLevel exposing (SubPath)
import Svg exposing (Svg, path, g, Attribute)
import Svg.Attributes as A


render : Mark.Path -> Svg msg
render mark =
    let
        pathDef =
            A.d <| LowLevel.toString [ mark.path ]

        fillAttrs =
            Fill.fill mark.fill

        strokeAttrs =
            Stroke.stroke mark.stroke

        cursor =
            Cursor.cursor mark.cursor

        attrs =
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
                |> List.filterMap identity

        transform =
            Transform.translate mark.x mark.y
    in
        g [ transform ] [ path attrs [] ]
