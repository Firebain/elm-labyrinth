module Map exposing (Map, NeighborInfo, Terrain(..), Vector2D, getClosestNeighbors, getNextNeighbors, init, render, set)

import Array exposing (Array)
import Canvas exposing (Point, Renderable, fill, rect, shapes)
import Color
import List.Extra


type alias Vector2D =
    { x : Int, y : Int }


type Terrain
    = Ground
    | Wall


type alias NeighborInfo =
    { coords : Vector2D
    , terrain : Maybe Terrain
    }


type alias Map =
    List Terrain


init : Int -> Terrain -> Result String Map
init size terr =
    if modBy 2 size == 1 then
        Ok <| List.repeat (size * size) terr

    else
        Err "The size should be odd"



-- FUNCTIONS


set : Int -> Int -> Terrain -> Map -> Map
set x y terr map =
    List.Extra.setAt (x + y * getSize map) terr map


getSize : Map -> Int
getSize map =
    round <| sqrt <| toFloat <| List.length map


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
        case List.Extra.getAt (x + y * getSize map) map of
            Just val ->
                NeighborInfo coords (Just val)

            Nothing ->
                NeighborInfo coords Nothing

    else
        NeighborInfo coords Nothing



-- RENDER


render : Float -> Maybe Vector2D -> Map -> List Renderable
render elementSize point map =
    List.indexedMap (renderElement elementSize (getSize map) point) map


renderElement : Float -> Int -> Maybe Vector2D -> Int -> Terrain -> Renderable
renderElement elementSize size current index terr =
    let
        x =
            modBy size index

        y =
            index // size

        currentEq =
            case current of
                Just curr ->
                    curr.x == x && curr.y == y

                Nothing ->
                    False

        color =
            if currentEq then
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
