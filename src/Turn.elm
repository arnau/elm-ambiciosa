module Turn exposing (Turn(..), add, empty, isOpen, score)

import Dice exposing (Score)
import Hand exposing (Hand)


type Turn
    = Closed (List Hand)
    | Open (List Hand)


empty : Turn
empty =
    Open []


add : Hand -> Turn -> Turn
add hand turn =
    case turn of
        Closed _ ->
            turn

        Open list ->
            if Hand.toScore hand > 0 then
                Open (hand :: list)

            else
                Closed (hand :: list)


isOpen : Turn -> Bool
isOpen turn =
    case turn of
        Closed _ ->
            False

        Open _ ->
            True


close : Turn -> Turn
close turn =
    case turn of
        Open list ->
            Closed list

        Closed _ ->
            turn


score : Turn -> Score
score turn =
    let
        sumif x acc =
            if x > 0 then
                x + acc

            else
                x
    in
    case turn of
        Open list ->
            list
                |> List.map Hand.toScore
                |> List.foldr sumif 0

        Closed list ->
            list
                |> List.map Hand.toScore
                |> List.foldr sumif 0
