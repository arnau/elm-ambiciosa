module PlayerList exposing
    ( PlayerList(..)
    , activePlayer
    , add
    , empty
    , next
    , recycle
    , toList
    )

import Dice exposing (Score)
import Player exposing (Player)


type PlayerList
    = EmptyList
    | PlayerList State


type alias State =
    { prevList : List Player
    , current : Player
    , nextList : List Player
    }


activePlayer : PlayerList -> Maybe Player
activePlayer list =
    case list of
        EmptyList ->
            Nothing

        PlayerList { current } ->
            Just current


empty : PlayerList
empty =
    EmptyList


add : Player -> PlayerList -> PlayerList
add player list =
    case list of
        EmptyList ->
            PlayerList { prevList = [], current = player, nextList = [] }

        PlayerList state ->
            PlayerList { state | nextList = state.nextList ++ [ player ] }


next : PlayerList -> PlayerList
next list =
    case list of
        EmptyList ->
            EmptyList

        PlayerList state ->
            PlayerList (nextState state)


toList : PlayerList -> List ( Player, Bool )
toList list =
    let
        toTuple =
            \p -> ( p, False )
    in
    case list of
        EmptyList ->
            []

        PlayerList { prevList, current, nextList } ->
            List.map toTuple prevList ++ (( current, True ) :: List.map toTuple nextList)


recycle : PlayerList -> PlayerList
recycle list =
    case list of
        EmptyList ->
            EmptyList

        PlayerList state ->
            PlayerList (recycleState state)



-- Inner state


nextState : State -> State
nextState state =
    case state.nextList of
        [] ->
            recycleState state

        newCurr :: newNext ->
            { prevList = state.current :: state.prevList, current = newCurr, nextList = newNext }


recycleState : State -> State
recycleState { prevList, current, nextList } =
    case nextList ++ List.reverse prevList of
        [] ->
            { prevList = [], current = current, nextList = [] }

        newCurr :: newNext ->
            { prevList = [], current = newCurr, nextList = newNext }
