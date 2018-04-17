module Facet.Render.Svg.Helpers exposing (viewBoxToString, px, em, toRGBString, parens, norm, groupSome, pairs)

import Color exposing (Color)
import Facet.Scenegraph as Scenegraph exposing (ViewBox)


viewBoxToString : ViewBox -> String
viewBoxToString { x, y, width, height } =
    [ x, y, width, height ]
        |> List.map toString
        |> String.join ","


px : a -> String
px x =
    toString x ++ "px"


em : Float -> String
em size =
    (toString size) ++ "em"


parens : String -> String
parens x =
    "(" ++ x ++ ")"


toRGBString : Color -> String
toRGBString color =
    let
        rgb =
            Color.toRgb color
    in
        "rgb(" ++ toString rgb.red ++ "," ++ toString rgb.green ++ "," ++ toString rgb.blue ++ ")"


norm : Float -> Float
norm rads =
    rads - turns (toFloat (floor (rads / (2 * pi))))


groupSome : List a -> List (List a) -> List (Maybe a) -> List (List a)
groupSome currentPath allPaths points =
    case points of
        [] ->
            (List.reverse currentPath)
                :: allPaths
                |> List.reverse

        (Just next) :: rest ->
            groupSome (next :: currentPath) allPaths rest

        _ :: rest ->
            let
                path =
                    List.reverse currentPath
            in
                groupSome [] (path :: allPaths) rest


pairs : List a -> List ( a, a )
pairs xs =
    case List.tail xs of
        Nothing ->
            []

        Just tail ->
            List.map2 (,) xs tail
