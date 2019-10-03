module Common exposing (..)

import Bitwise
import Html exposing (..)


type Msg
    = InputA String
    | InputB String
    | InputBits String


showInt : Int -> Int -> Html Msg
showInt number bits =
    div [] (List.map (\i -> text (String.fromInt (getBit number i))) (List.reverse (List.range 0 (bits - 1))))


getBit : Int -> Int -> Int
getBit number bit =
    if Bitwise.and number (Bitwise.shiftLeftBy bit 1) /= 0 then
        1

    else
        0
