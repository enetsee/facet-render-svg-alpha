module Facet.Render.Svg.Fill exposing (FillAttributes, fill)

import Facet.Scenegraph.Fill exposing (Fill)
import Facet.Render.Svg.Helpers exposing (toRGBString)
import Svg exposing (Attribute)
import Svg.Attributes as A


type alias FillAttributes msg =
    { fill : Attribute msg
    , fillOpacity : Maybe (Attribute msg)
    }


fill : Fill -> FillAttributes msg
fill { fill, fillOpacity } =
    FillAttributes
        (Maybe.map toRGBString fill |> Maybe.withDefault "none" |> A.fill)
        (Maybe.map (toString >> A.fillOpacity) fillOpacity)
