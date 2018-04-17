module Facet.Render.Svg.Symbol exposing (render)

import Facet.Scenegraph.Mark as Mark
import Facet.Scenegraph.Shape exposing (Shape(..))
import Facet.Render.Svg.Cursor as Cursor
import Facet.Render.Svg.Fill as Fill
import Facet.Render.Svg.Stroke as Stroke
import Facet.Render.Svg.Transform as Transform exposing (Transform(Translate, Rotate, Scale))
import Path.LowLevel as LowLevel exposing (SubPath, Mode(..), MoveTo(..), DrawTo(..), EllipticalArcArgument, Direction(..), ArcFlag(..))
import Svg exposing (Svg, Attribute, g, path)
import Svg.Attributes as A


render : Mark.Symbol -> Svg msg
render mark =
    case mark.shape of
        Circle ->
            renderAsPath (circlePath mark.size) Nothing mark

        Square ->
            renderAsPath (squarePath mark.size) Nothing mark

        Cross ->
            renderAsPath (crossPath mark.size) Nothing mark

        Diamond ->
            renderAsPath (diamondPath mark.size) Nothing mark

        TriangleUp ->
            renderAsPath (triangleUpPath mark.size) Nothing mark

        TriangleDown ->
            renderAsPath (triangleDownPath mark.size) Nothing mark

        TriangleRight ->
            renderAsPath (triangleRightPath mark.size) Nothing mark

        TriangleLeft ->
            renderAsPath (triangleLeftPath mark.size) Nothing mark

        Arrow ->
            renderAsPath (arrowPath mark.size) Nothing mark

        Custom path ->
            renderAsPath path (Just mark.size) mark



-- Internal helpers ------------------------------------------------------------


renderAsPath : SubPath -> Maybe Float -> Mark.Symbol -> Svg msg
renderAsPath path maybeSize mark =
    let
        pathDef =
            A.d <| LowLevel.toString [ path ]

        fillAttrs =
            Fill.fill mark.fill

        strokeAttrs =
            Stroke.stroke mark.stroke

        cursor =
            Cursor.cursor mark.cursor

        attrs =
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
                |> List.filterMap identity

        transform =
            Transform.transform <|
                List.filterMap identity
                    [ Just <| Translate mark.x mark.y
                    , Just <| Rotate mark.angle
                    , Maybe.map (\scale -> Scale scale scale) maybeSize
                    ]
    in
        g [ transform ] [ Svg.path attrs [] ]



-- Named shapes ----------------------------------------------------------------


squarePath : Float -> SubPath
squarePath size =
    let
        r =
            (sqrt size) / 2
    in
        SubPath
            (MoveTo Absolute ( -r, -r ))
            [ LineTo Absolute [ ( -r, r ), ( r, r ), ( r, -r ) ]
            , ClosePath
            ]


arrowPath : Float -> SubPath
arrowPath size =
    let
        newSize =
            sqrt size

        r =
            newSize / 2

        r2 =
            r / 2

        r3 =
            r / 3

        r4 =
            r / 4
    in
        SubPath
            (MoveTo Absolute ( -r4, r ))
            [ LineTo Absolute
                [ ( -r4, -r4 ), ( -r2, -r4 ), ( 0, -r ), ( r2, -r4 ), ( r4, -r4 ), ( r4, r ) ]
            , ClosePath
            ]


circlePath : Float -> SubPath
circlePath size =
    let
        r =
            (sqrt size) / 2

        newSize =
            sqrt size

        arc1 =
            EllipticalArcArgument
                ( r, r )
                0
                LargestArc
                Clockwise
                ( newSize, 0 )

        arc2 =
            EllipticalArcArgument
                ( r, r )
                0
                LargestArc
                Clockwise
                ( -newSize, 0 )
    in
        SubPath
            (MoveTo Absolute ( -r, 0 ))
            [ EllipticalArc Relative [ arc1, arc2 ] ]


crossPath : Float -> SubPath
crossPath size =
    let
        r =
            (sqrt size) / 2

        s =
            r / 2.5
    in
        SubPath
            (MoveTo Absolute ( -r, -s ))
            [ LineTo Absolute
                [ ( -r, s )
                , ( -s, s )
                , ( -s, r )
                , ( s, r )
                , ( s, s )
                , ( r, s )
                , ( r, -s )
                , ( s, -s )
                , ( s, -r )
                , ( -s, -r )
                , ( -s, -s )
                ]
            , ClosePath
            ]


diamondPath : Float -> SubPath
diamondPath size =
    let
        r =
            (sqrt size) / 2
    in
        SubPath
            (MoveTo Absolute ( -r, 0 ))
            [ LineTo Absolute [ ( 0, -r ), ( r, 0 ), ( 0, r ) ]
            , ClosePath
            ]


triangleUpPath : Float -> SubPath
triangleUpPath size =
    let
        r =
            (sqrt size) / 2

        h =
            halfSqrt3 * r
    in
        SubPath
            (MoveTo Absolute ( 0, -h ))
            [ LineTo Absolute [ ( -r, h ), ( r, h ) ]
            , ClosePath
            ]


triangleDownPath : Float -> SubPath
triangleDownPath size =
    let
        r =
            (sqrt size) / 2

        h =
            halfSqrt3 * r
    in
        SubPath
            (MoveTo Absolute ( 0, h ))
            [ LineTo Absolute [ ( -r, -h ), ( r, -h ) ]
            , ClosePath
            ]


triangleRightPath : Float -> SubPath
triangleRightPath size =
    let
        r =
            (sqrt size) / 2

        h =
            halfSqrt3 * r
    in
        SubPath
            (MoveTo Absolute ( h, 0 ))
            [ LineTo Absolute [ ( -h, -r ), ( -h, r ) ]
            , ClosePath
            ]


triangleLeftPath : Float -> SubPath
triangleLeftPath size =
    let
        r =
            (sqrt size) / 2

        h =
            halfSqrt3 * r
    in
        SubPath
            (MoveTo Absolute ( -h, 0 ))
            [ LineTo Absolute [ ( h, -r ), ( h, r ) ]
            , ClosePath
            ]


halfSqrt3 : Float
halfSqrt3 =
    (sqrt 3.0) / 2


tau : Float
tau =
    2 * Basics.pi
