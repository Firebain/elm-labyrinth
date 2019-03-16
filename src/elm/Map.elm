module Map exposing (Map, Terrain(..), initMap, renderMap)

import Canvas exposing (Point, Renderable, fill, rect, shapes)
import Color


type Terrain
    = Ground
    | Wall


type alias Map =
    List (List Terrain)


initMap : Int -> Terrain -> Map
initMap size terr =
    List.repeat size (List.repeat size terr)


renderMap : Map -> Float -> List Renderable
renderMap map elementSize =
    List.indexedMap (renderRow elementSize) map
        |> List.concat


renderRow : Float -> Int -> List Terrain -> List Renderable
renderRow elementSize index row =
    List.indexedMap (renderElement elementSize index) row


renderElement : Float -> Int -> Int -> Terrain -> Renderable
renderElement elementSize row col terr =
    let
        color =
            case terr of
                Wall ->
                    Color.black

                Ground ->
                    Color.white
    in
    shapes [ fill color ]
        [ rect (calculateGlobalPoint elementSize row col) elementSize elementSize ]


calculateGlobalPoint : Float -> Int -> Int -> Point
calculateGlobalPoint elementSize x y =
    ( toFloat x * elementSize, toFloat y * elementSize )
