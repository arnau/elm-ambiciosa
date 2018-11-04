module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Dice exposing (Face(..))
import Game exposing (Game(..))
import Hand exposing (Hand)
import Html exposing (..)
import Html.Attributes as At
import Html.Events exposing (..)
import Player exposing (Player(..))
import PlayerList exposing (PlayerList)
import Random



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { hand : Hand
    , game : Game
    , players : PlayerList
    , playerInput : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { hand = Hand.empty
      , game = Game.init PlayerList.empty
      , players = PlayerList.empty
      , playerInput = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewHand Hand.Hand
    | NewPlayer
    | InputPlayer String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewHand Hand.roll
            )

        NewHand newHand ->
            ( { model | hand = newHand }
            , Cmd.none
            )

        NewPlayer ->
            ( { model | game = Game.addPlayer model.playerInput model.game }
            , Cmd.none
            )

        InputPlayer input ->
            ( { model | playerInput = input }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ handView model.hand
        , button [ onClick Roll ] [ text "Roll" ]
        , boardView model
        ]


handView : Hand -> Html Msg
handView hand =
    div [ At.style "font-size" "100px" ]
        (case hand of
            Hand.None ->
                [ text "No hand" ]

            Hand.Hand a b c ->
                [ text (Dice.toString a)
                , text (Dice.toString b)
                , text (Dice.toString c)
                , text " = "
                , text (String.fromInt (Hand.toScore hand))
                ]
        )


boardView : Model -> Html Msg
boardView model =
    div []
        [ input [ At.value model.playerInput, onInput InputPlayer ] []
        , button [ onClick NewPlayer ] [ text "Add player" ]
        , scoreView model.game
        ]


scoreView : Game -> Html Msg
scoreView game =
    table []
        [ thead []
            [ th [] [ text "Name" ]
            , th [] [ text "Score" ]
            , th [] [ text "Active" ]
            ]
        , tbody [] (rowsView game)
        ]


rowsView : Game -> List (Html Msg)
rowsView game =
    game
        |> Game.playerList
        |> List.map
            (\( player, active ) ->
                tr []
                    [ td [] [ text (Player.name player) ]
                    , td [] [ text (String.fromInt (Player.score player)) ]
                    , td []
                        [ text
                            (if active then
                                "Active"

                             else
                                ""
                            )
                        ]
                    ]
            )
