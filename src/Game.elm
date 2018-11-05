module Game exposing
    ( Game(..)
    , activePlayer
    , addPlayer
    , addTurn
    , endTurn
    , init
    , playerList
    )

import Dice exposing (Score)
import Dict exposing (Dict)
import Hand exposing (Hand)
import Player exposing (Player)
import PlayerList exposing (PlayerList)
import Turn exposing (Turn)


type Game
    = Game
        { players : PlayerList
        }


init : PlayerList -> Game
init players =
    Game
        { players = players
        }


activePlayer : Game -> Maybe Player
activePlayer (Game { players }) =
    PlayerList.activePlayer players


addPlayer : String -> Game -> Game
addPlayer name (Game state) =
    Game { state | players = PlayerList.add (Player.init name) state.players }


endTurn : Game -> Game
endTurn (Game state) =
    Game { state | players = PlayerList.next state.players }


addTurn : Turn -> Game -> Game
addTurn turn (Game state) =
    Game { state | players = PlayerList.addTurn turn state.players }


playerList : Game -> List ( Player, Bool )
playerList (Game { players }) =
    PlayerList.toList players
