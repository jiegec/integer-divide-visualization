module NonRestoring exposing (..)

import Bitwise
import Browser
import Common exposing (..)
import Html exposing (..)


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


view a b bits =
    let
        initStep =
            { step = 0
            , action = Init
            , remainder = a
            , quotient = 0
            , bits = bits
            }
    in
    [ h2 [] [ text "Non restoring division" ]
    , showSteps initStep b bits
    ]


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
