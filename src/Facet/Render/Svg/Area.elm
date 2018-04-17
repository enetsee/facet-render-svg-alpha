module Facet.Render.Svg.Area exposing (render)

import Facet.Scenegraph.Mark as Mark exposing (Behaviour(..))
import Facet.Scenegraph.Position exposing (Position(..))
import Facet.Render.Svg.Cursor as Cursor
import Facet.Render.Svg.LineLike exposing (points3, curve)
import Facet.Render.Svg.Stroke as Stroke
import Facet.Render.Svg.Fill as Fill
import SubPath exposing (SubPath)
import Path
import Svg exposing (Svg, Attribute)
import Svg.Attributes as A


render : Mark.Area -> Svg msg
render mark =
    let
        strokeAttrs =
            Stroke.stroke mark.stroke

        fillAttrs =
            Fill.fill mark.fill

        cursor =
            Cursor.cursor mark.cursor

        ( y0, y1 ) =
            mark.y
                |> List.map fromPosition
                |> List.unzip

        pathDef =
            points3 mark.behaviour mark.x y0 y1
                |> List.map
                    (\xys ->
                        let
                            ( upper, lower ) =
                                xys
                                    |> List.map (\( x, y0, y1 ) -> ( ( x, y0 ), ( x, y1 ) ))
                                    |> List.unzip
                        in
                            SubPath.connect
                                (curve mark.interpolate upper)
                                (curve mark.interpolate <| List.reverse lower)
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


fromPosition : Maybe Position -> ( Maybe Float, Maybe Float )
fromPosition maybePos =
    case maybePos of
        Just p ->
            let
                ( y0, y1 ) =
                    positionHelper p
            in
                ( Just y0, Just y1 )

        _ ->
            ( Nothing, Nothing )


positionHelper : Position -> ( Float, Float )
positionHelper position =
    case position of
        PrimarySecondary primary secondary ->
            ( primary
            , secondary
            )

        PrimaryExtent primary extent ->
            ( primary
            , (primary + extent)
            )

        SecondaryExtent secondary extent ->
            ( (secondary - extent)
            , secondary
            )

        CenterExtent center extent ->
            let
                halfExtent =
                    extent / 2
            in
                ( (center - halfExtent)
                , (center + halfExtent)
                )
