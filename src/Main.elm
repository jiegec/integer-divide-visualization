module Main exposing (..)

import Bitwise
import Browser
import Common exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import NonRestoring
import Restoring


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { a : Int
    , b : Int
    , bits : Int
    }


init : Model
init =
    { a = 10
    , b = 3
    , bits = 5
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputA a ->
            { model | a = clampInput (Maybe.withDefault 0 (String.toInt a)) model.bits }

        InputB b ->
            { model | b = clampInput (Maybe.withDefault 0 (String.toInt b)) model.bits }

        InputBits input ->
            let
                bits =
                    clamp 1 15 (Maybe.withDefault 0 (String.toInt input))
            in
            { a = clampInput model.a bits, b = clampInput model.b bits, bits = bits }


clampInput : Int -> Int -> Int
clampInput input bits =
    clamp 0 (Bitwise.shiftLeftBy bits 1 - 1) input


view : Model -> Html Msg
view model =
    div []
        ([ h1 [] [ text "Visualization of Integer Division" ]
         , div []
            [ text "A = "
            , input [ onInput InputA, value (String.fromInt model.a) ] []
            ]
         , showInt model.a model.bits
         , div []
            [ text "B = "
            , input [ onInput InputB, value (String.fromInt model.b) ] []
            ]
         , showInt model.b model.bits
         , div []
            [ text "Bits = "
            , input [ onInput InputBits, value (String.fromInt model.bits) ] []
            ]
         ]
            ++ NonRestoring.view model.a model.b model.bits
            ++ Restoring.view model.a model.b model.bits
        )
