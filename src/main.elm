module Main exposing (Model, init, main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type alias Model =
    { numbers : List Int
    , currentNumber : Int
    }


init : Model
init =
    { numbers = List.range 0 99
    , currentNumber = 0
    }


type Msg
    = Pick Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Pick num ->
            if num == model.currentNumber then
                { model | currentNumber = num + 1 }

            else
                model


numberButton currentNumber num =
    let
        color =
            if num < currentNumber then
                "#cccccc"

            else if num == currentNumber then
                "#00ee00"

            else
                "#ffffff"
    in
    button
        [ onClick (Pick num)
        , style "border" "1px outset black"
        , style "background-color" color
        , style "width" "50px"
        , style "padding" "10px 10px"
        ]
        [ text (String.fromInt num) ]


numberButtons currentNumber numbers =
    div [] (List.map (\n -> numberButton currentNumber n) numbers)


grid : Int -> List Int -> List (List Int)
grid size numbers =
    if List.isEmpty numbers then
        []

    else
        let
            row =
                List.take size numbers

            remaining =
                grid size (List.drop size numbers)
        in
        List.append [ row ] remaining


view : Model -> Html Msg
view model =
    let
        numberGrid =
            List.map (\row -> numberButtons model.currentNumber row) (grid 10 model.numbers)
    in
    div []
        [ text ("Current number: " ++ String.fromInt model.currentNumber)
        , div [] numberGrid
        ]


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
