module Facet.Render.Svg.Transform exposing (Transform(..), transform, translate, scale, rotate, rotateAround, skewX, skewY)

import Svg exposing (Attribute)
import Svg.Attributes as A
import Facet.Render.Svg.Helpers exposing (norm)


type Transform
    = Translate Float Float
    | Scale Float Float
    | Rotate Float
    | RotateAround Float Float Float
    | SkewX Float
    | SkewY Float


transform : List Transform -> Attribute msg
transform transforms =
    transforms
        |> List.map transformInternal
        |> String.join " "
        |> A.transform


translate : Float -> Float -> Attribute msg
translate x y =
    transform [ Translate x y ]


scale : Float -> Float -> Attribute msg
scale x y =
    transform [ Scale x y ]


rotate : Float -> Attribute msg
rotate angle =
    transform [ Rotate angle ]


rotateAround : Float -> Float -> Float -> Attribute msg
rotateAround angle x y =
    transform [ RotateAround angle x y ]


skewX : Float -> Attribute msg
skewX x =
    transform [ SkewX x ]


skewY : Float -> Attribute msg
skewY y =
    transform [ SkewY y ]


transformInternal : Transform -> String
transformInternal transform =
    case transform of
        Translate x y ->
            "translate(" ++ toString x ++ " " ++ toString y ++ ")"

        Scale x y ->
            "scale(" ++ toString x ++ " " ++ toString y ++ ")"

        Rotate angle ->
            let
                angleInDegrees =
                    radToDeg angle
            in
                "rotate(" ++ toString angleInDegrees ++ ")"

        RotateAround angle x y ->
            let
                angleInDegrees =
                    radToDeg angle
            in
                "rotate(" ++ toString angleInDegrees ++ " " ++ toString x ++ " " ++ toString y ++ ")"

        SkewX x ->
            "skewX(" ++ toString x ++ ")"

        SkewY y ->
            "skewY(" ++ toString y ++ ")"


radToDeg : Float -> Float
radToDeg rads =
    (norm rads) * 180.0 / pi
