module Labirinth exposing (LabirinthCreator, getNextDirectionGenerator, goToUnvisited, move)

import Array exposing (Array)
import Map exposing (Map, NeighborInfo, Terrain(..))
import Random exposing (Generator)
import Utilities


type alias LabirinthCreator =
    { current : { x : Int, y : Int }, neighbors : List NeighborInfo, unvisited : List NeighborInfo }


getNextDirectionGenerator : LabirinthCreator -> Map -> Generator ( Maybe NeighborInfo, List NeighborInfo )
getNextDirectionGenerator creator map =
    let
        neighbors =
            getCorrectNeighbors creator map
    in
    Random.map (\n -> ( Utilities.listGetAt n neighbors, neighbors )) (Random.int 0 (List.length neighbors - 1))


getCorrectNeighbors : LabirinthCreator -> Map -> List NeighborInfo
getCorrectNeighbors creator map =
    creator.neighbors
        |> List.filter (isCorrectWay map creator)


isCorrectWay : Map -> LabirinthCreator -> NeighborInfo -> Bool
isCorrectWay map creator neighbor =
    case neighbor.terrain of
        Just val ->
            case val of
                Wall ->
                    let
                        neighbors =
                            Map.getClosestNeighbors neighbor.coords.x neighbor.coords.y map
                                |> List.filter (\el -> el.coords.x /= creator.current.x || el.coords.y /= creator.current.y)
                                |> List.map (\el -> el.terrain)

                        hasEmptyTerrain =
                            List.member Nothing neighbors

                        hasVisitedTerrain =
                            List.member (Just Ground) neighbors
                    in
                    not hasEmptyTerrain && not hasVisitedTerrain

                Ground ->
                    False

        Nothing ->
            False


move : NeighborInfo -> List NeighborInfo -> Map -> LabirinthCreator -> ( LabirinthCreator, Map )
move destination neighbors map creator =
    let
        coords =
            destination.coords

        updatedMap =
            map
                |> Map.set coords.x coords.y Ground
                |> Map.set ((creator.current.x + coords.x) // 2) ((creator.current.y + coords.y) // 2) Ground

        filteredNeighbors =
            neighbors
                |> List.filter (\el -> el.coords.x /= coords.x || el.coords.y /= coords.y)
                |> List.filter (\el -> not (List.any (\unv -> el.coords.x == unv.coords.x && el.coords.y == unv.coords.y) creator.unvisited))
    in
    ( LabirinthCreator { x = coords.x, y = coords.y } (Map.getNextNeighbors coords.x coords.y map) (List.append creator.unvisited filteredNeighbors)
    , updatedMap
    )


goToUnvisited : Map -> LabirinthCreator -> Maybe LabirinthCreator
goToUnvisited map creator =
    case List.head creator.unvisited of
        Just unvisited ->
            let
                newUnvisited =
                    List.drop 1 creator.unvisited

                hasGround el =
                    case el.terrain of
                        Just terr ->
                            terr == Ground

                        Nothing ->
                            False

                current =
                    Map.getNextNeighbors unvisited.coords.x unvisited.coords.y map
                        |> List.filter hasGround
                        |> List.head
            in
            case current of
                Just curr ->
                    Just <| LabirinthCreator { x = curr.coords.x, y = curr.coords.y } (Map.getNextNeighbors curr.coords.x curr.coords.y map) newUnvisited

                Nothing ->
                    Nothing

        Nothing ->
            Nothing
