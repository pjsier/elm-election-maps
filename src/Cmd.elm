port module Cmd exposing (Point, projectPoint, projectedPoint)

import Json.Encode as E
import LngLat exposing (LngLat)


type alias Point =
    { x : Float, y : Float }


port projectPoint : LngLat -> Cmd a


port projectedPoint : (Point -> msg) -> Sub msg
