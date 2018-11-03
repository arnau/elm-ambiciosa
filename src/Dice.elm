module Dice exposing (Face(..), Score, roll, toInt, toScore, toString)

import Random


type alias Score =
    Int


type Face
    = Ace
    | King
    | Three
    | Four
    | Five
    | Six


roll : Random.Generator Face
roll =
    Random.uniform Ace [ King, Three, Four, Five, Six ]


toString : Face -> String
toString face =
    case face of
        Ace ->
            "⚀"

        King ->
            "⚁"

        Three ->
            "⚂"

        Four ->
            "⚃"

        Five ->
            "⚄"

        Six ->
            "⚅"


toInt : Face -> Int
toInt face =
    case face of
        Ace ->
            1

        King ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6


toScore : Face -> Score
toScore face =
    case face of
        Ace ->
            10

        King ->
            5

        _ ->
            0
