module Facet.Render.Svg.Trail exposing (render)

import Facet.Scenegraph.Mark as Mark
import Svg exposing (Svg)
import Facet.Scenegraph.Mark as Mark exposing (Behaviour(..))
import Facet.Render.Svg.Cursor as Cursor
import Facet.Render.Svg.Fill as Fill
import Facet.Render.Svg.Helpers exposing (norm, groupSome, pairs)
import Facet.Render.Svg.LineLike exposing (points3)
import Path.LowLevel as LowLevel exposing (MoveTo(..), DrawTo(..), EllipticalArcArgument, Mode(..), ArcFlag(..), Direction(..))
import Path exposing (Path)
import SubPath exposing (SubPath)
import Segment exposing (Segment)
import Svg exposing (Svg, Attribute)
import Svg.Attributes as A


render : Mark.Trail -> Svg msg
render mark =
    let
        fillAttrs =
            Fill.fill mark.fill

        cursor =
            Cursor.cursor mark.cursor

        path =
            points3 mark.behaviour mark.x mark.y mark.width
                |> List.concatMap (pairs >> List.map section)
                |> Path.toString
                |> A.d

        attrs =
            [ Just cursor, Just fillAttrs.fill, Just path, fillAttrs.fillOpacity ]
                |> List.filterMap identity
    in
        Svg.path attrs []


section : ( ( Float, Float, Float ), ( Float, Float, Float ) ) -> SubPath
section ( ( x1, y1, w1 ), ( x2, y2, w2 ) ) =
    let
        r1 =
            w1 / 2

        r2 =
            w2 / 2

        ux =
            y1 - y2

        uy =
            x2 - x1
    in
        if ux /= 0 || uy /= 0 then
            let
                ud =
                    sqrt <| ux * ux + uy * uy

                uxn =
                    ux / ud

                rx =
                    uxn * r1

                uyn =
                    uy / ud

                ry =
                    uyn * r1

                t =
                    atan2 uyn uxn
            in
                [ Segment.line ( x1 - rx, y1 - ry ) ( x2 - uxn * r2, y2 - uyn * r2 )
                , arc ( x2, y2 ) ( r2, r2 ) (t - pi) pi 0
                , Segment.line ( x2 + uxn * r2, y2 + uyn * r2 ) ( x1 + rx, y1 + ry )
                , arc ( x1, y1 ) ( r1, r1 ) t pi 0
                ]
                    |> SubPath.fromSegments
        else
            [ arc ( x2, y2 ) ( r2, r2 ) 0 (2 * pi) 0 ]
                |> SubPath.fromSegments


arc : ( Float, Float ) -> ( Float, Float ) -> Float -> Float -> Float -> Segment
arc center radii startAngle deltaTheta xAxisRotate =
    let
        conversion =
            conversionMatrix xAxisRotate

        endAngle =
            startAngle + deltaTheta

        ( rx, ry ) =
            radii

        p1 =
            ( rx * cos startAngle, ry * sin startAngle )
                |> mulVector conversion
                |> add center

        p2 =
            ( rx * cos endAngle, ry * sin endAngle )
                |> mulVector conversion
                |> add center

        ( arcFlag, direction ) =
            decodeFlags
                ( if abs deltaTheta > pi then
                    1
                  else
                    0
                , if deltaTheta > 0 then
                    1
                  else
                    0
                )
                |> Maybe.withDefault ( SmallestArc, CounterClockwise )
    in
        Segment.Arc { start = p1, end = p2, radii = radii, arcFlag = arcFlag, direction = direction, xAxisRotate = xAxisRotate }


conversionMatrix : Float -> ( ( Float, Float ), ( Float, Float ) )
conversionMatrix xAxisRotate =
    ( ( cos xAxisRotate, -1 * sin xAxisRotate )
    , ( sin xAxisRotate, cos xAxisRotate )
    )


mulVector :
    ( ( number, number ), ( number, number ) )
    -> ( number, number )
    -> ( number, number )
mulVector ( v1, v2 ) v =
    ( dot v1 v, dot v2 v )


dot : ( number, number ) -> ( number, number ) -> number
dot ( x1, y1 ) ( x2, y2 ) =
    x1 * x2 + y1 * y2


add : ( number, number1 ) -> ( number, number1 ) -> ( number, number1 )
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


decodeFlags : ( number, number1 ) -> Maybe ( ArcFlag, Direction )
decodeFlags ( arcFlag, sweepFlag ) =
    case ( arcFlag, sweepFlag ) of
        ( 1, 0 ) ->
            Just ( LargestArc, Clockwise )

        ( 0, 0 ) ->
            Just ( SmallestArc, Clockwise )

        ( 1, 1 ) ->
            Just ( LargestArc, CounterClockwise )

        ( 0, 1 ) ->
            Just ( SmallestArc, CounterClockwise )

        _ ->
            Nothing
