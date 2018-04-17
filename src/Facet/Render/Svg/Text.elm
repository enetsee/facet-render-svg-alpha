module Facet.Render.Svg.Text exposing (render)

import Facet.Scenegraph.Mark as Mark
import Facet.Render.Svg.Cursor as Cursor
import Facet.Render.Svg.Fill as Fill
import Facet.Render.Svg.Font as Font
import Facet.Render.Svg.Helpers exposing (em)
import Facet.Render.Svg.Stroke as Stroke
import Facet.Render.Svg.Transform as Transform
import Svg exposing (Svg)
import Svg.Attributes as A


render : Mark.Text -> Svg msg
render mark =
    let
        baseline =
            A.alignmentBaseline <| textBaselineToString mark.baseline

        cursor =
            Cursor.cursor mark.cursor

        writingMode =
            A.writingMode <| textDirectionToString mark.direction

        dx =
            A.dx <| em mark.dx

        dy =
            A.dy <| em mark.dy

        x =
            A.x <| toString mark.x

        y =
            A.y <| toString mark.y

        transform =
            Transform.rotateAround mark.angle mark.x mark.y

        fillAttrs =
            Fill.fill mark.fill

        strokeAttrs =
            Stroke.stroke mark.stroke

        fontAttrs =
            Font.font mark.font

        textAnchor =
            A.textAnchor <| textAlignToString mark.align

        styles =
            A.style "user-select: none"

        attrs =
            [ Just baseline
            , Just textAnchor
            , Just cursor
            , Just writingMode
            , Just styles
            , Just dx
            , Just dy
            , Just x
            , Just y
            , Just transform
            , Just fontAttrs.fontFamily
            , Just fontAttrs.fontSize
            , Just fontAttrs.fontWeight
            , Just fontAttrs.fontStyle
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
        Svg.text_ attrs [ Svg.text mark.text ]



-- Internal helpers ------------------------------------------------------------


textAlignToString : Mark.Align -> String
textAlignToString align =
    case align of
        Mark.Center ->
            "middle"

        Mark.Left ->
            "end"

        Mark.Right ->
            "start"


textBaselineToString : Mark.Baseline -> String
textBaselineToString baseline =
    case baseline of
        Mark.Top ->
            "hanging"

        Mark.Middle ->
            "middle"

        Mark.Bottom ->
            "baseline"

        Mark.Alphabetic ->
            "alphabetic"


textDirectionToString : Mark.Direction -> String
textDirectionToString direction =
    case direction of
        Mark.LeftToRight ->
            "lr"

        Mark.RightToLeft ->
            "rl"
