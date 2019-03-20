module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Array exposing (Array)
import Browser
import Canvas
import Color
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Labirinth exposing (Direction(..))
import Map exposing (Map, NeighborInfo, Terrain(..))
import Random exposing (Generator)
import Time


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



--MODEL


type alias LabirinthCreator =
    { current : { x : Int, y : Int }, neighbors : List NeighborInfo, unvisited : List NeighborInfo }


type Status
    = Idle
    | Running LabirinthCreator
    | Error String


elementsCount =
    20


type Msg
    = NextStep LabirinthCreator Time.Posix
    | Move LabirinthCreator ( Maybe NeighborInfo, List NeighborInfo )


type alias Model =
    { map : Map, status : Status }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        x =
            1

        y =
            1
    in
    case Map.init elementsCount Wall of
        Ok map ->
            ( Model (map |> Map.set x y Ground) (Running <| LabirinthCreator { x = x, y = y } (Map.getNextNeighbors x y map) []), Cmd.none )

        Err val ->
            ( Model Array.empty (Error val), Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextStep creator time ->
            ( model
            , Random.generate (Move creator) (getNextDirection creator model.map)
            )

        Move creator next ->
            let
                ( info, list ) =
                    next
            in
            case info of
                Just val ->
                    let
                        coords =
                            val.coords

                        map =
                            model.map |> Map.set coords.x coords.y Ground |> Map.set ((creator.current.x + coords.x) // 2) ((creator.current.y + coords.y) // 2) Ground

                        filteredList =
                            list
                                |> List.filter (\el -> el.coords.x /= val.coords.x || el.coords.y /= val.coords.y)
                                |> List.filter (\el -> not (List.any (\unv -> el.coords.x == unv.coords.x && el.coords.y == unv.coords.y) creator.unvisited))

                        status =
                            Running <| LabirinthCreator { x = coords.x, y = coords.y } (Map.getNextNeighbors coords.x coords.y map) (List.append creator.unvisited filteredList)
                    in
                    ( { model | map = map, status = status }, Cmd.none )

                Nothing ->
                    case List.head creator.unvisited of
                        Just val ->
                            let
                                newUnvisited =
                                    List.drop 1 creator.unvisited

                                findGround el =
                                    case el.terrain of
                                        Just terr ->
                                            terr == Ground

                                        Nothing ->
                                            False

                                current =
                                    Map.getNextNeighbors val.coords.x val.coords.y model.map
                                        |> List.filter findGround
                                        |> List.head
                            in
                            case current of
                                Just curr ->
                                    ( { model | status = Running <| LabirinthCreator { x = curr.coords.x, y = curr.coords.y } (Map.getNextNeighbors curr.coords.x curr.coords.y model.map) newUnvisited }, Cmd.none )

                                Nothing ->
                                    ( { model | status = Idle }, Cmd.none )

                        Nothing ->
                            ( { model | status = Idle }, Cmd.none )


getNextDirection : LabirinthCreator -> Map -> Generator ( Maybe NeighborInfo, List NeighborInfo )
getNextDirection creator map =
    let
        neighbors =
            getCorrectNeighbors creator map
    in
    Random.map (\n -> ( Array.get n neighbors, Array.toList neighbors )) (Random.int 0 (Array.length neighbors - 1))


getCorrectNeighbors : LabirinthCreator -> Map -> Array NeighborInfo
getCorrectNeighbors creator map =
    creator.neighbors
        |> List.filter (isCorrectWay map creator)
        |> Array.fromList


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Running creator ->
            Time.every 100 (NextStep creator)

        _ ->
            Sub.none



-- VIEW


elementSize =
    15


mapSize =
    elementsCount * elementSize


view : Model -> Html Msg
view model =
    case model.status of
        Error val ->
            text val

        _ ->
            Canvas.toHtml ( mapSize, mapSize )
                []
                (Map.render elementSize model.map)
