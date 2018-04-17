module Facet.Render.Svg.Stroke exposing (StrokeAttributes, stroke)

import Facet.Render.Svg.Helpers exposing (toRGBString, parens)
import Facet.Scenegraph.Stroke as Stroke exposing (Stroke, StrokeLineCap(..), StrokeLineJoin(..), StrokeDash)
import Svg exposing (Attribute)
import Svg.Attributes as A


type alias StrokeAttributes msg =
    { stroke : Attribute msg
    , strokeOpacity : Maybe (Attribute msg)
    , strokeWidth : Maybe (Attribute msg)
    , strokeLineCap : Maybe (Attribute msg)
    , strokeDashArray : Maybe (Attribute msg)
    , strokeDashOffset : Maybe (Attribute msg)
    , strokeLineJoin : Maybe (Attribute msg)
    , strokeMiterLimit : Maybe (Attribute msg)
    }


stroke : Stroke -> StrokeAttributes msg
stroke stroke =
    let
        lineCap =
            Maybe.map strokeLineCap stroke.strokeLineCap

        ( lineJoin, miterLimit ) =
            case Maybe.map strokeLineJoin stroke.strokeLineJoin of
                Just ( join, limit ) ->
                    ( Just join, limit )

                _ ->
                    ( Nothing, Nothing )

        ( dashArray, dashOffset ) =
            Maybe.map strokeDash stroke.strokeDash
                |> Maybe.withDefault ( Nothing, Nothing )
    in
        StrokeAttributes
            (Maybe.map toRGBString stroke.stroke |> Maybe.withDefault "none" |> A.stroke)
            (Maybe.map (toString >> A.strokeOpacity) stroke.strokeOpacity)
            (Maybe.map (toString >> A.strokeWidth) stroke.strokeWidth)
            lineCap
            dashArray
            dashOffset
            lineJoin
            miterLimit


strokeLineCap : StrokeLineCap -> Attribute msg
strokeLineCap strokeLineCap =
    case strokeLineCap of
        CapButt ->
            A.strokeLinecap "butt"

        CapRound ->
            A.strokeLinecap "round"

        CapSquare ->
            A.strokeLinecap "square"


strokeLineJoin : StrokeLineJoin -> ( Attribute msg, Maybe (Attribute msg) )
strokeLineJoin strokeLineJoin =
    case strokeLineJoin of
        JoinMiter miterLimit ->
            ( A.strokeLinejoin "miter"
            , Just <| A.strokeMiterlimit <| toString miterLimit
            )

        JoinRound ->
            ( A.strokeLinejoin "round"
            , Nothing
            )

        JoinBevel ->
            ( A.strokeLinejoin "bevel", Nothing )


strokeDash : StrokeDash -> ( Maybe (Attribute msg), Maybe (Attribute msg) )
strokeDash strokeDash =
    let
        strokeDashArray =
            case Stroke.strokeDashArray strokeDash of
                [] ->
                    Nothing

                xs ->
                    xs
                        |> List.map toString
                        |> String.join ","
                        |> A.strokeDasharray
                        |> Just

        strokeDashOffset =
            strokeDash
                |> Stroke.strokeDashOffset
                |> Maybe.map (toString >> A.strokeDashoffset)
    in
        ( strokeDashArray, strokeDashOffset )
