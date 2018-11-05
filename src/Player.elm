module Player exposing (Player, init, name, record, score)

import Dice exposing (Score)
import Turn exposing (Turn(..))


type alias Trail =
    List Turn


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
    trail
        |> List.map Turn.score
        |> List.sum


record : Turn -> Player -> Player
record newTurn (Player state) =
    Player { state | trail = newTurn :: state.trail }
