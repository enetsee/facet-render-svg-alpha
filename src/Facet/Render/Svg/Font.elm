module Facet.Render.Svg.Font exposing (FontAttributes, font)

import Facet.Scenegraph.Font as Font exposing (Font, FontWeight(..), FontStyle(..))
import Svg exposing (Attribute)
import Svg.Attributes as A


type alias FontAttributes msg =
    { fontFamily : Attribute msg
    , fontSize : Attribute msg
    , fontWeight : Attribute msg
    , fontStyle : Attribute msg
    }


font : Font -> FontAttributes msg
font { font, fontSize, fontWeight, fontStyle } =
    FontAttributes
        (A.fontFamily font)
        (A.fontSize <| toString fontSize)
        (A.fontWeight <| fontWeightToString fontWeight)
        (A.fontStyle <| fontStyleToString fontStyle)


{-| -}
fontWeightToString : FontWeight -> String
fontWeightToString weight =
    case weight of
        WeightNormal ->
            "normal"

        WeightBold ->
            "bold"


{-| -}
fontStyleToString : FontStyle -> String
fontStyleToString style =
    case style of
        StyleNormal ->
            "normal"

        StyleItalic ->
            "italic"
