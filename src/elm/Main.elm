module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Array exposing (Array)
import Browser
import Canvas
import Color
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Labirinth exposing (LabirinthCreator, getNextDirectionGenerator)
import Map exposing (Map, NeighborInfo, Terrain(..), Vector2D)
import Random exposing (Generator)
import Time


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



--MODEL


type Status
    = Idle Map
    | Running LabirinthCreator
    | Error String


elementsCount =
    41


type alias Model =
    { status : Status }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( x, y ) =
            ( 1, 1 )
    in
    case Map.init elementsCount Wall of
        Ok map ->
            ( { status = Running <| LabirinthCreator { x = x, y = y } (map |> Map.set x y Ground) (Map.getNextNeighbors x y map) [] }
            , Cmd.none
            )

        Err val ->
            ( Model (Error val), Cmd.none )



-- UPDATE


type Msg
    = NextStep LabirinthCreator Time.Posix
    | Move LabirinthCreator ( Maybe NeighborInfo, List NeighborInfo )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextStep creator time ->
            ( model
            , Random.generate (Move creator) (getNextDirectionGenerator creator)
            )

        Move creator next ->
            let
                ( info, list ) =
                    next
            in
            case info of
                Just val ->
                    let
                        updatedCreator =
                            Labirinth.move val list creator

                        status =
                            Running <| updatedCreator
                    in
                    ( { model | status = status }, Cmd.none )

                Nothing ->
                    case Labirinth.goToUnvisited creator of
                        Just updatedCreator ->
                            ( { model | status = Running <| updatedCreator }, Cmd.none )

                        Nothing ->
                            ( { model | status = Idle creator.map }, Cmd.none )



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
            renderCanvas (Just creator.current) creator.map

        Idle map ->
            renderCanvas Nothing map

        Error val ->
            text val


renderCanvas : Maybe Vector2D -> Map -> Html Msg
renderCanvas point map =
    Canvas.toHtml ( mapSize, mapSize )
        []
        (Map.render elementSize point map)
