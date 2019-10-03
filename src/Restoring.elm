module Restoring exposing (..)

import Bitwise
import Browser
import Common exposing (..)
import Html exposing (..)


type Action
    = Init
    | Sub
    | Shift


type alias Step =
    { step : Int
    , shiftSteps : Int
    , action : Action
    , quotient : Int
    , remainder : Int
    , bits : Int
    }


view a b bits =
    let
        initStep =
            { step = 0
            , shiftSteps = 0
            , action = Init
            , remainder = 0
            , quotient = a
            , bits = bits
            }
    in
    [ h2 [] [ text "Restoring division" ]
    , showSteps initStep b bits
    ]


showSteps : Step -> Int -> Int -> Html Msg
showSteps initStep b bits =
    let
        steps =
            allSteps initStep b bits

        lastStep =
            List.head (List.drop (List.length steps - 1) steps)

        quotient =
            Maybe.withDefault 0 (Maybe.map (\step -> step.quotient) lastStep)

        remainder =
            Maybe.withDefault 0 (Maybe.map (\step -> step.remainder) lastStep)
    in
    div []
        [ table []
            [ thead []
                [ tr []
                    [ th [] [ text "Step" ]
                    , th [] [ text "Substep" ]
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
    if initStep.shiftSteps == bits && initStep.action == Sub then
        [ initStep ]

    else
        initStep :: allSteps (nextStep initStep b) b bits


showStep : Step -> Html Msg
showStep step =
    tr []
        [ td [] [ text (String.fromInt step.shiftSteps) ]
        , td [] [ text (String.fromInt step.step) ]
        , td [] [ text (toString step.action) ]
        , td [] [ showInt step.remainder step.bits ]
        , td [] [ showInt step.quotient step.bits ]
        ]


toString : Action -> String
toString action =
    case action of
        Init ->
            "Init"

        Sub ->
            "Sub"

        Shift ->
            "Shift"


nextStep : Step -> Int -> Step
nextStep prev b =
    case prev.action of
        Init ->
            shift prev b

        Shift ->
            sub prev b

        Sub ->
            shift prev b


sub prev b =
    if prev.remainder >= b then
        { step = prev.step + 1
        , shiftSteps = prev.shiftSteps
        , action = Sub
        , remainder = prev.remainder - b
        , quotient = prev.quotient + 1
        , bits = prev.bits
        }

    else
        { step = prev.step + 1
        , shiftSteps = prev.shiftSteps
        , action = Sub
        , remainder = prev.remainder
        , quotient = prev.quotient
        , bits = prev.bits
        }


shift prev b =
    { step = prev.step + 1
    , shiftSteps = prev.shiftSteps + 1
    , action = Shift
    , remainder = prev.remainder * 2 + Bitwise.shiftRightBy (prev.bits - 1) (Bitwise.and prev.quotient (Bitwise.shiftLeftBy (prev.bits - 1) 1))
    , quotient = Bitwise.and (prev.quotient * 2) (Bitwise.shiftLeftBy prev.bits 1 - 1)
    , bits = prev.bits
    }
