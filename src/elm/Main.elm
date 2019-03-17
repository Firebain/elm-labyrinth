module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Canvas exposing (..)
import Color
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Map exposing (Map, Terrain(..))
import Time


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



--MODEL


type alias LabirinthCreator =
    { current : { x : Int, y : Int } }


type Status
    = Idle
    | Running LabirinthCreator
    | Error String


elementsCount =
    20


type Msg
    = NextStep LabirinthCreator Time.Posix


type alias Model =
    { map : Map, status : Status }


clone : Int -> Map -> Map
clone val map =
    map


init : () -> ( Model, Cmd Msg )
init _ =
    case Map.init elementsCount Wall of
        Ok map ->
            ( Model map (Running <| LabirinthCreator <| { x = 0, y = 0 }), Cmd.none )

        Err val ->
            ( Model [] (Error val), Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextStep creator time ->
            let
                x =
                    creator.current.x

                y =
                    creator.current.y

                map =
                    model.map |> Map.set x y Ground

                status =
                    Running <| LabirinthCreator <| { x = x + 1, y = y + 1 }
            in
            ( { model | map = map, status = status }
            , Cmd.none
            )



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
