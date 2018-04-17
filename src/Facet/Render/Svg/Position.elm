module Facet.Render.Svg.Position exposing (xWidth, yHeight, xPrimarySecondary, yPrimarySecondary, xCenter, yCenter)

import Facet.Scenegraph.Position exposing (Position(..))
import Svg exposing (Svg, Attribute)
import Svg.Attributes as A


xWidth : Position -> ( Attribute msg, Attribute msg )
xWidth =
    primaryExtent A.x A.width


xPrimarySecondary : Position -> ( Attribute msg, Attribute msg )
xPrimarySecondary =
    primarySecondary A.x1 A.x2


xCenter : Position -> Attribute msg
xCenter =
    center A.cx


yHeight : Position -> ( Attribute msg, Attribute msg )
yHeight =
    primaryExtent A.y A.height


yPrimarySecondary : Position -> ( Attribute msg, Attribute msg )
yPrimarySecondary =
    primarySecondary A.y1 A.y2


yCenter : Position -> Attribute msg
yCenter =
    center A.cy



-- Internal helpers ------------------------------------------------------------


primaryExtent : (String -> a) -> (String -> b) -> Position -> ( a, b )
primaryExtent primaryAttr extentAttr position =
    case position of
        PrimarySecondary primary secondary ->
            let
                x0 =
                    min primary secondary

                extent =
                    abs (secondary - primary)
            in
                ( primaryAttr <| toString x0
                , extentAttr <| toString extent
                )

        PrimaryExtent primary extent ->
            ( primaryAttr <| toString primary
            , extentAttr <| toString extent
            )

        SecondaryExtent secondary extent ->
            ( primaryAttr <| toString (secondary - extent)
            , extentAttr <| toString extent
            )

        CenterExtent center extent ->
            ( primaryAttr <| toString (center - (extent / 2))
            , extentAttr <| toString extent
            )


primarySecondary : (String -> a) -> (String -> b) -> Position -> ( a, b )
primarySecondary primaryAttr secondaryAttr position =
    case position of
        PrimarySecondary primary secondary ->
            ( primaryAttr <| toString primary
            , secondaryAttr <| toString secondary
            )

        PrimaryExtent primary extent ->
            ( primaryAttr <| toString primary
            , secondaryAttr <| toString (primary + extent)
            )

        SecondaryExtent secondary extent ->
            ( primaryAttr <| toString (secondary - extent)
            , secondaryAttr <| toString secondary
            )

        CenterExtent center extent ->
            let
                halfExtent =
                    extent / 2
            in
                ( primaryAttr <| toString (center - halfExtent)
                , secondaryAttr <| toString (center + halfExtent)
                )


center : (String -> a) -> Position -> a
center coordinateAttr position =
    case position of
        PrimarySecondary primary secondary ->
            coordinateAttr <| toString ((secondary - primary) / 2)

        PrimaryExtent primary extent ->
            coordinateAttr <| toString (primary + (extent / 2))

        SecondaryExtent secondary extent ->
            coordinateAttr <| toString (secondary - (extent / 2))

        CenterExtent center extent ->
            coordinateAttr <| toString center
