module Labirinth exposing (LabirinthCreator, getNextDirectionGenerator, goToUnvisited, move)

import Array exposing (Array)
import List.Extra
import Map exposing (Map, NeighborInfo, Terrain(..), Vector2D)
import Random exposing (Generator)


type alias LabirinthCreator =
    { current : Vector2D, map : Map, neighbors : List NeighborInfo, unvisited : List NeighborInfo }


getNextDirectionGenerator : LabirinthCreator -> Generator ( Maybe NeighborInfo, List NeighborInfo )
getNextDirectionGenerator creator =
    let
        neighbors =
            getCorrectNeighbors creator
    in
    Random.map (\n -> ( List.Extra.getAt n neighbors, neighbors )) (Random.int 0 (List.length neighbors - 1))


getCorrectNeighbors : LabirinthCreator -> List NeighborInfo
getCorrectNeighbors creator =
    creator.neighbors
        |> List.filter (isCorrectWay creator)


isCorrectWay : LabirinthCreator -> NeighborInfo -> Bool
isCorrectWay creator neighbor =
    case neighbor.terrain of
        Just val ->
            case val of
                Wall ->
                    let
                        neighbors =
                            Map.getClosestNeighbors neighbor.coords.x neighbor.coords.y creator.map
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


move : NeighborInfo -> List NeighborInfo -> LabirinthCreator -> LabirinthCreator
move destination neighbors creator =
    let
        coords =
            destination.coords

        updatedMap =
            creator.map
                |> Map.set coords.x coords.y Ground
                |> Map.set ((creator.current.x + coords.x) // 2) ((creator.current.y + coords.y) // 2) Ground

        filteredNeighbors =
            neighbors
                |> List.filter (\el -> el.coords.x /= coords.x || el.coords.y /= coords.y)
                |> List.filter (\el -> not (List.any (\unv -> el.coords.x == unv.coords.x && el.coords.y == unv.coords.y) creator.unvisited))
    in
    LabirinthCreator { x = coords.x, y = coords.y } updatedMap (Map.getNextNeighbors coords.x coords.y updatedMap) (List.append creator.unvisited filteredNeighbors)


goToUnvisited : LabirinthCreator -> Maybe LabirinthCreator
goToUnvisited creator =
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
                    Map.getNextNeighbors unvisited.coords.x unvisited.coords.y creator.map
                        |> List.filter hasGround
                        |> List.head
            in
            case current of
                Just curr ->
                    Just <| LabirinthCreator { x = curr.coords.x, y = curr.coords.y } creator.map (Map.getNextNeighbors curr.coords.x curr.coords.y creator.map) newUnvisited

                Nothing ->
                    Nothing

        Nothing ->
            Nothing
