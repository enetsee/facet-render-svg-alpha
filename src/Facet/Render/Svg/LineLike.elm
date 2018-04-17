module Facet.Render.Svg.LineLike exposing (points, points3, curve)

import Curve
import Facet.Scenegraph.Interpolate exposing (Interpolate(..))
import Facet.Scenegraph.Mark as Mark exposing (Behaviour(..))
import SubPath exposing (SubPath)
import Facet.Render.Svg.Helpers exposing (groupSome)


curve : Interpolate -> List ( Float, Float ) -> SubPath
curve interpolate =
    case interpolate of
        Linear ->
            Curve.linear

        Basis ->
            Curve.basis

        Bundle tension ->
            Curve.bundle tension

        Cardinal tension ->
            Curve.cardinal tension

        CatmullRom tension ->
            Curve.catmullRom tension

        Monotone ->
            Curve.monotoneX

        Natural ->
            Curve.natural

        Step ->
            Curve.step 0.5

        StepAfter ->
            Curve.stepAfter

        StepBefore ->
            Curve.stepBefore


points : Behaviour -> List (Maybe a) -> List (Maybe b) -> List (List ( a, b ))
points behaviour xs ys =
    List.map2 (Maybe.map2 (,)) xs ys
        |> withBehaviour behaviour


points3 :
    Behaviour
    -> List (Maybe a)
    -> List (Maybe b)
    -> List (Maybe c)
    -> List (List ( a, b, c ))
points3 behaviour xs ys ws =
    List.map3 (Maybe.map3 (,,)) xs ys ws
        |> withBehaviour behaviour


withBehaviour : Behaviour -> List (Maybe a) -> List (List a)
withBehaviour behaviour =
    case behaviour of
        SkipMissing ->
            List.filterMap identity
                >> List.singleton

        BeginNew ->
            groupSome [] []
