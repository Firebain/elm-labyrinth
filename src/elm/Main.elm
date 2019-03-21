module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Array exposing (Array)
import Browser
import Canvas
import Color
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Labirinth exposing (LabirinthCreator, getNextDirectionGenerator)
import Map exposing (Map, NeighborInfo, Terrain(..))
import Random exposing (Generator)
import Time


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



--MODEL


type Status
    = Idle
    | Running LabirinthCreator
    | Error String


elementsCount =
    41


type alias Model =
    { map : Map, status : Status, current : { x : Int, y : Int } }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( x, y ) =
            ( 1, 1 )
    in
    case Map.init elementsCount Wall of
        Ok map ->
            ( { map = map |> Map.set x y Ground
              , status = Running <| LabirinthCreator { x = x, y = y } (Map.getNextNeighbors x y map) []
              , current = { x = x, y = y }
              }
            , Cmd.none
            )

        Err val ->
            ( Model Array.empty (Error val) { x = x, y = y }, Cmd.none )



-- UPDATE


type Msg
    = NextStep LabirinthCreator Time.Posix
    | Move LabirinthCreator ( Maybe NeighborInfo, List NeighborInfo )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextStep creator time ->
            ( model
            , Random.generate (Move creator) (getNextDirectionGenerator creator model.map)
            )

        Move creator next ->
            let
                ( info, list ) =
                    next
            in
            case info of
                Just val ->
                    let
                        ( updatedCreator, map ) =
                            Labirinth.move val list model.map creator

                        status =
                            Running <| updatedCreator

                        current =
                            { x = updatedCreator.current.x, y = updatedCreator.current.y }
                    in
                    ( { model | map = map, status = status, current = current }, Cmd.none )

                Nothing ->
                    case Labirinth.goToUnvisited model.map creator of
                        Just updatedCreator ->
                            ( { model | status = Running <| updatedCreator, current = { x = updatedCreator.current.x, y = updatedCreator.current.y } }, Cmd.none )

                        Nothing ->
                            ( { model | status = Idle }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Running creator ->
            Time.every 5 (NextStep creator)

        _ ->
            Sub.none



-- VIEW


elementSize =
    10


mapSize =
    elementsCount * elementSize


view : Model -> Html Msg
view model =
    case model.status of
        Running creator ->
            renderCanvas model

        Idle ->
            renderCanvas model

        Error val ->
            text val


renderCanvas : Model -> Html Msg
renderCanvas model =
    Canvas.toHtml ( mapSize, mapSize )
        []
        (Map.render elementSize model.current.x model.current.y model.map)
