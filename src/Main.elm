module Main exposing (..)

import Bitwise
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { a : Int
    , b : Int
    , bits : Int
    }


type Action
    = Init
    | Sub
    | Add
    | Final


type alias Step =
    { step : Int
    , action : Action
    , quotient : Int
    , remainder : Int
    , bits : Int
    }


init : Model
init =
    { a = 10
    , b = 3
    , bits = 5
    }


type Msg
    = InputA String
    | InputB String
    | InputBits String


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
    let
        initStep =
            { step = 0
            , action = Init
            , remainder = model.a
            , quotient = 0
            , bits = model.bits
            }
    in
    div []
        [ h1 [] [ text "Visualization of Integer Division" ]
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
        , h2 [] [ text "Non restoring division" ]
        , showSteps initStep model.b model.bits
        ]


showInt : Int -> Int -> Html Msg
showInt number bits =
    div [] (List.map (\i -> text (String.fromInt (getBit number i))) (List.reverse (List.range 0 (bits - 1))))


showSteps : Step -> Int -> Int -> Html Msg
showSteps initStep b bits =
    let
        steps =
            allSteps initStep (Bitwise.shiftLeftBy bits b) bits

        lastStep =
            List.head (List.drop (List.length steps - 1) steps)

        quotient =
            Maybe.withDefault 0 (Maybe.map (\step -> step.quotient) lastStep)

        remainder =
            Bitwise.shiftRightBy bits (Maybe.withDefault 0 (Maybe.map (\step -> step.remainder) lastStep))
    in
    div []
        [ table []
            [ thead []
                [ tr []
                    [ th [] [ text "Step" ]
                    , th [] [ text "Action" ]
                    , th [] [ text "Remainder" ]
                    , th [] [ text "Quotient" ]
                    ]
                ]
            , tbody [] (List.map (\s -> showStep s) steps)
            ]
        , text "Answer is "
        , text (String.fromInt quotient)
        , text " ... "
        , text (String.fromInt remainder)
        ]


allSteps : Step -> Int -> Int -> List Step
allSteps initStep b bits =
    if initStep.step == bits then
        [ initStep, finalStep initStep b bits ]

    else
        initStep :: allSteps (nextStep initStep b) b bits


showStep : Step -> Html Msg
showStep step =
    tr []
        [ td [] [ text (String.fromInt step.step) ]
        , td [] [ text (toString step.action) ]
        , td [] [ showInt step.remainder (step.bits * 2) ]
        , td [] [ showInt step.quotient (step.bits * 2) ]
        ]


toString : Action -> String
toString action =
    case action of
        Init ->
            "Init"

        Sub ->
            "Sub"

        Add ->
            "Add"

        Final ->
            "Final"


getBit : Int -> Int -> Int
getBit number bit =
    if Bitwise.and number (Bitwise.shiftLeftBy bit 1) /= 0 then
        1

    else
        0


nextStep : Step -> Int -> Step
nextStep prev b =
    if prev.remainder >= 0 then
        { step = prev.step + 1
        , action = Add
        , remainder = prev.remainder * 2 - b
        , quotient = prev.quotient * 2 + 1
        , bits = prev.bits
        }

    else
        { step = prev.step + 1
        , action = Sub
        , remainder = prev.remainder * 2 + b
        , quotient = prev.quotient * 2
        , bits = prev.bits
        }


finalStep : Step -> Int -> Int -> Step
finalStep prev b bits =
    if prev.remainder >= 0 then
        { step = prev.step + 1
        , action = Final
        , remainder = prev.remainder
        , quotient = prev.quotient - Bitwise.xor prev.quotient (Bitwise.shiftLeftBy bits 1 - 1)
        , bits = prev.bits
        }

    else
        { step = prev.step + 1
        , action = Final
        , remainder = prev.remainder + b
        , quotient = prev.quotient - Bitwise.xor prev.quotient (Bitwise.shiftLeftBy bits 1 - 1) - 1
        , bits = prev.bits
        }
