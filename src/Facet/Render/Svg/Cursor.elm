module Facet.Render.Svg.Cursor exposing (cursor)

import Facet.Scenegraph.Cursor exposing (Cursor(..))
import Svg exposing (Attribute)
import Svg.Attributes as A


cursor : Cursor -> Attribute msg
cursor cursor =
    case cursor of
        CursorAuto ->
            A.cursor "auto"

        CursorDefault ->
            A.cursor "default"

        CursorNone ->
            A.cursor "none"

        CursorContextMenu ->
            A.cursor "context-menu"

        CursorHelp ->
            A.cursor "help"

        CursorPointer ->
            A.cursor "pointer"

        CursorProgress ->
            A.cursor "progress"

        CursorWait ->
            A.cursor "wait"

        CursorCell ->
            A.cursor "cell"

        CursorCrosshair ->
            A.cursor "crosshair"

        CursorText ->
            A.cursor "text"

        CursorVerticalText ->
            A.cursor "vertical-text"

        CursorAlias ->
            A.cursor "alias"

        CursorCopy ->
            A.cursor "copy"

        CursorMove ->
            A.cursor "move"

        CursorNoDrop ->
            A.cursor "no-drop"

        CursorNotAllowed ->
            A.cursor "not-allowed"

        CursorEResize ->
            A.cursor "e-resize"

        CursorNResize ->
            A.cursor "n-resize"

        CursorNEResize ->
            A.cursor "ne-resize"

        CursorNWResize ->
            A.cursor "nw-resize"

        CursorSResize ->
            A.cursor "s-resize"

        CursorSEResize ->
            A.cursor "se-resize"

        CursorSWResize ->
            A.cursor "sw-resize"

        CursorWResize ->
            A.cursor "w-resize"

        CursorEWResize ->
            A.cursor "ew-resize"

        CursorNSResize ->
            A.cursor "ns-resize"

        CursorNESWResize ->
            A.cursor "nesw-resize"

        CursorNWSEResize ->
            A.cursor "nwse-resize"

        CursorColResize ->
            A.cursor "col-resize"

        CursorRowResize ->
            A.cursor "row-resize"

        CursorAllScroll ->
            A.cursor "all-scroll"

        CursorZoomIn ->
            A.cursor "zoom-in"

        CursorZoomOut ->
            A.cursor "zoom-out"

        CursorGrab ->
            A.cursor "grab"

        CursorGrabbing ->
            A.cursor "grabbing"

        CursorUrl url x y ->
            A.cursor <|
                "url("
                    ++ url
                    ++ ")"
                    ++ (Maybe.withDefault "" <|
                            Maybe.map2 (\x y -> " " ++ toString x ++ " " ++ toString y)
                                x
                                y
                       )
