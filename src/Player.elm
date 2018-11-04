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
    List.sum trail


record : Score -> Player -> Player
record newScore (Player state) =
    Player { state | trail = newScore :: state.trail }
