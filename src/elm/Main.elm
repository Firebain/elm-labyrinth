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


directionGenerator : Generator Direction
directionGenerator =
    Random.map directionParser (Random.int 0 3)


directionParser : Int -> Direction
directionParser n =
    case n of
        0 ->
            Up

        1 ->
            Down

        2 ->
            Left

        _ ->
            Right


type alias LabirinthCreator =
    { current : { x : Int, y : Int }, neighbors : List NeighborInfo }


type Status
    = Idle
    | Running LabirinthCreator
    | Error String


elementsCount =
    20


type Msg
    = NextStep LabirinthCreator Time.Posix
    | Move LabirinthCreator (Maybe NeighborInfo)


type alias Model =
    { map : Map, status : Status }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        x =
            0

        y =
            1
    in
    case Map.init elementsCount Wall of
        Ok map ->
            ( Model (map |> Map.set x y Ground) (Running <| LabirinthCreator { x = x, y = y } (Map.getNeighbors x y map)), Cmd.none )

        Err val ->
            ( Model Array.empty (Error val), Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextStep creator time ->
            ( model
            , Random.generate (Move creator) (getNextDirection creator)
            )

        Move creator next ->
            case next of
                Just val ->
                    let
                        coords =
                            val.coords

                        map =
                            model.map |> Map.set coords.x coords.y Ground

                        status =
                            Running <| LabirinthCreator { x = coords.x, y = coords.y } (Map.getNeighbors coords.x coords.y map)
                    in
                    ( { model | map = map, status = status }, Cmd.none )

                Nothing ->
                    ( { model | status = Idle }, Cmd.none )


getNextDirection : LabirinthCreator -> Generator (Maybe NeighborInfo)
getNextDirection creator =
    let
        neighbors =
            getCorrectNeighbors creator
    in
    Random.map (\n -> Array.get n neighbors) (Random.int 0 (Array.length neighbors - 1))


getCorrectNeighbors : LabirinthCreator -> Array NeighborInfo
getCorrectNeighbors creator =
    creator.neighbors
        |> List.filter isCorrectWay
        |> Array.fromList


isCorrectWay : NeighborInfo -> Bool
isCorrectWay neighbor =
    case neighbor.terrain of
        Just val ->
            case val of
                Wall ->
                    True

                Ground ->
                    False

        Nothing ->
            False



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Running creator ->
            Time.every 1000 (NextStep creator)

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
