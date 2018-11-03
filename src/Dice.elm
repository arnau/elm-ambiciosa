module Dice exposing (Face(..), Score, roll, toInt, toScore, toString)

import Random


type alias Score =
    Int


type Face
    = Ace
    | King
    | Queen
    | Jack
    | Nine
    | Eight


roll : Random.Generator Face
roll =
    Random.uniform Ace [ King, Queen, Jack, Nine, Eight ]


toString : Face -> String
toString face =
    case face of
        Ace ->
            -- "⚀"
            "🂡"

        King ->
            -- "⚁"
            "🂮"

        Queen ->
            -- "⚂"
            "🂭"

        Jack ->
            -- "⚃"
            "🂫"

        Nine ->
            -- "⚄"
            "🂩"

        Eight ->
            -- "⚅"
            "🂨"


toInt : Face -> Int
toInt face =
    case face of
        Ace ->
            1

        King ->
            2

        Queen ->
            3

        Jack ->
            4

        Nine ->
            5

        Eight ->
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
