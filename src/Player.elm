module Player exposing (Player, init, name, record, score)

import Dice exposing (Score)


type alias Trail =
    List Score


type Player
    = Player
        { name : String
        , trail : Trail
        }


init : String -> Player
init name_ =
    Player { name = name_, trail = [] }


name : Player -> String
name (Player state) =
    state.name


score : Player -> Score
score (Player { trail }) =
    let
        sumif x acc =
            if x >= 0 then
                x + acc

            else
                x
    in
    List.foldr sumif 0 trail


record : Score -> Player -> Player
record newScore (Player state) =
    Player { state | trail = newScore :: state.trail }
