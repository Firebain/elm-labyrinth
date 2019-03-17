module Map exposing (Map, Terrain(..), init, render, set)

import Canvas exposing (Point, Renderable, fill, rect, shapes)
import Color
import List.Extra


type Terrain
    = Ground
    | Wall


type alias NeighborsInfo =
    List
        { coords :
            { x : Int
            , y : Int
            }
        , terrain : Maybe Terrain
        }


type alias Map =
    List Terrain


init : Int -> Terrain -> Result String Map
init size terr =
    if modBy 2 size == 0 then
        Ok <| List.repeat (size * size) terr

    else
        Err "The size should be even"



-- FUNCTIONS


set : Int -> Int -> Terrain -> Map -> Map
set x y terr map =
    List.Extra.setAt (x + y * getSize map) terr map


getSize : Map -> Int
getSize map =
    round <| sqrt <| toFloat <| List.length map



-- RENDER


render : Float -> Map -> List Renderable
render elementSize map =
    let
        size =
            getSize map
    in
    List.indexedMap (renderElement elementSize size) map


renderElement : Float -> Int -> Int -> Terrain -> Renderable
renderElement elementSize size index terr =
    let
        x =
            modBy size index

        y =
            index // size

        color =
            case terr of
                Wall ->
                    Color.black

                Ground ->
                    Color.white
    in
    shapes [ fill color ]
        [ rect (calculateGlobalPoint elementSize x y) elementSize elementSize ]


calculateGlobalPoint : Float -> Int -> Int -> Point
calculateGlobalPoint elementSize x y =
    ( toFloat x * elementSize, toFloat y * elementSize )
