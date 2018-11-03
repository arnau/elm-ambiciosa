module Hand exposing (Hand(..), empty, roll, toScore)

import Dice exposing (Face(..), Score)
import Random


type Hand
    = Hand Face Face Face
    | None


empty : Hand
empty =
    None


roll : Random.Generator Hand
roll =
    Random.map3 Hand Dice.roll Dice.roll Dice.roll


toScore : Hand -> Score
toScore hand =
    case hand of
        Hand Ace Ace Ace ->
            100

        Hand King King King ->
            50

        Hand Nine Nine Nine ->
            -10

        Hand Eight Eight Eight ->
            -30

        Hand a b c ->
            [ a, b, c ]
                |> List.map Dice.toScore
                |> List.sum

        None ->
            0
