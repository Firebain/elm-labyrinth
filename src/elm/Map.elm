module Map exposing (Map, NeighborInfo, Terrain(..), getClosestNeighbors, getNextNeighbors, init, render, set)

import Array exposing (Array)
import Canvas exposing (Point, Renderable, fill, rect, shapes)
import Color


type Terrain
    = Ground
    | Wall


type alias NeighborInfo =
    { coords :
        { x : Int
        , y : Int
        }
    , terrain : Maybe Terrain
    }


type alias Map =
    Array Terrain


init : Int -> Terrain -> Result String Map
init size terr =
    if modBy 2 size == 1 then
        Ok <| Array.repeat (size * size) terr

    else
        Err "The size should be odd"



-- FUNCTIONS


set : Int -> Int -> Terrain -> Map -> Map
set x y terr map =
    Array.set (x + y * getSize map) terr map


getSize : Map -> Int
getSize map =
    round <| sqrt <| toFloat <| Array.length map


getClosestNeighbors : Int -> Int -> Map -> List NeighborInfo
getClosestNeighbors x y map =
    getNeighborsFromRange 1 x y map


getNextNeighbors : Int -> Int -> Map -> List NeighborInfo
getNextNeighbors x y map =
    getNeighborsFromRange 2 x y map


getNeighborsFromRange : Int -> Int -> Int -> Map -> List NeighborInfo
getNeighborsFromRange range x y map =
    getNeighbor (x - range) y map
        :: getNeighbor (x + range) y map
        :: getNeighbor x (y - range) map
        :: getNeighbor x (y + range) map
        :: []


getNeighbor : Int -> Int -> Map -> NeighborInfo
getNeighbor x y map =
    let
        coords =
            { x = x, y = y }

        isNegative =
            x < 0 || y < 0

        size =
            getSize map - 1

        isOutOfMap =
            x > size || y > size
    in
    if not isNegative && not isOutOfMap then
        case Array.get (x + y * getSize map) map of
            Just val ->
                NeighborInfo coords (Just val)

            Nothing ->
                NeighborInfo coords Nothing

    else
        NeighborInfo coords Nothing



-- RENDER


render : Float -> Int -> Int -> Map -> List Renderable
render elementSize x y map =
    Array.toList <| Array.indexedMap (renderElement elementSize (getSize map) x y) map


renderElement : Float -> Int -> Int -> Int -> Int -> Terrain -> Renderable
renderElement elementSize size currX currY index terr =
    let
        x =
            modBy size index

        y =
            index // size

        color =
            if currX == x && currY == y then
                Color.green

            else
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
