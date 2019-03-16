module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Canvas exposing (..)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Map exposing (..)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



--MODEL


elementsCount =
    10


type Msg
    = NoOp


type alias Model =
    { map : Map }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (initMap elementsCount Wall), Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


elementSize =
    25


mapSize =
    elementsCount * elementSize


view : Model -> Html Msg
view model =
    Canvas.toHtml ( mapSize, mapSize )
        []
        (renderMap model.map elementSize)
