module Facet.Render.Svg.Group exposing (render)

import Facet.Render.Svg.Transform as Transform
import Facet.Scenegraph.Mark as Mark
import Svg exposing (Svg, g)
import Svg.Attributes as A


{-| TODO: record extents for cliiping
-}
render : Mark.Group -> (List (Svg msg) -> Svg msg)
render mark =
    let
        clip =
            if mark.clip then
                Just <| clipPath mark.x mark.width mark.y mark.height
            else
                Nothing

        transform =
            Transform.translate mark.x mark.y
    in
        case clip of
            Just ( name, clipPath ) ->
                \xs ->
                    g [ transform ]
                        [ clipPath
                        , g [ A.clipPath <| "url(#" ++ name ++ ")" ]
                            xs
                        ]

            _ ->
                g [ transform ]


clipPath : a -> b -> c -> d -> ( String, Svg msg )
clipPath x width y height =
    let
        rect =
            Svg.rect
                [ A.x <| toString x
                , A.width <| toString width
                , A.y <| toString y
                , A.height <| toString height
                ]
                []

        name =
            "_" ++ toString x ++ "_" ++ toString width ++ "_" ++ toString y ++ "_" ++ toString height ++ "_"
    in
        ( name, Svg.clipPath [ A.id name ] [ rect ] )
