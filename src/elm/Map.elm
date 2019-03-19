module Map exposing (Map, NeighborInfo, Terrain(..), getNeighbors, init, render, set)

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
    if modBy 2 size == 0 then
        Ok <| Array.repeat (size * size) terr

    else
        Err "The size should be even"



-- FUNCTIONS


set : Int -> Int -> Terrain -> Map -> Map
set x y terr map =
    Array.set (x + y * getSize map) terr map


getSize : Map -> Int
getSize map =
    round <| sqrt <| toFloat <| Array.length map


getNeighbors : Int -> Int -> Map -> List NeighborInfo
getNeighbors x y map =
    Array.empty
        |> Array.push (getNeighbor (x - 1) y map)
        |> Array.push (getNeighbor (x + 1) y map)
        |> Array.push (getNeighbor x (y - 1) map)
        |> Array.push (getNeighbor x (y + 1) map)
        |> Array.toList


getNeighbor : Int -> Int -> Map -> NeighborInfo
getNeighbor x y map =
    let
        coords =
            { x = x, y = y }

        isNegative =
            x < 0 || y < 0
    in
    if not isNegative then
        case Array.get (x + y * getSize map) map of
            Just val ->
                NeighborInfo coords (Just val)

            Nothing ->
                NeighborInfo coords Nothing

    else
        NeighborInfo coords Nothing



-- RENDER


render : Float -> Map -> List Renderable
render elementSize map =
    let
        size =
            getSize map
    in
    Array.toList <| Array.indexedMap (renderElement elementSize size) map


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
