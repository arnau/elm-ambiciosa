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
import Turn exposing (Turn)



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
    { turn : Turn
    , game : Game
    , gameStarted : Bool
    , playerInput : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { turn = Turn.empty
      , game = Game.init PlayerList.empty
      , gameStarted = False
      , playerInput = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | EndTurn
    | StartGame
    | NewHand Hand.Hand
    | NewPlayer
    | InputPlayer String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( { model | gameStarted = True }
            , Cmd.none
            )

        Roll ->
            ( model
            , Random.generate NewHand Hand.roll
            )

        EndTurn ->
            let
                game_ =
                    Game.addTurn model.turn model.game
            in
            ( { model | game = Game.endTurn game_, turn = Turn.empty }
            , Cmd.none
            )

        NewHand newHand ->
            ( { model
                | turn = Turn.add newHand model.turn
              }
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
    if model.gameStarted then
        ongoingView model

    else
        setupView model


ongoingView : Model -> Html Msg
ongoingView model =
    div []
        [ turnView model
        , button [ onClick Roll ] [ text "Roll" ]
        , button [ onClick EndTurn ] [ text "End turn" ]
        , boardView model
        ]


setupView : Model -> Html Msg
setupView model =
    div []
        [ input [ At.value model.playerInput, onInput InputPlayer ] []
        , button [ onClick NewPlayer ] [ text "Add player" ]
        , button [ onClick StartGame ] [ text "Start game" ]
        , boardView model
        ]


turnView : Model -> Html Msg
turnView model =
    let
        playerName =
            model.game
                |> Game.activePlayer
                |> Maybe.map Player.name
                |> Maybe.withDefault "Anon"
    in
    div [ At.style "font-size" "100px" ]
        (case model.turn of
            Turn.Open [] ->
                [ text playerName
                , text " it is your turn"
                ]

            Turn.Open (hand :: hands) ->
                handView hand ++ totalTurnView model.turn

            Turn.Closed [] ->
                [ text "Empty closed" ]

            Turn.Closed (hand :: hands) ->
                [ div [ At.style "color" "tomato" ]
                    (handView hand ++ totalTurnView model.turn)
                ]
        )


totalTurnView : Turn -> List (Html Msg)
totalTurnView turn =
    [ div [ At.style "font-size" "50px" ]
        [ text " total ="
        , text (String.fromInt (Turn.score turn))
        ]
    ]


handView : Hand -> List (Html Msg)
handView hand =
    case hand of
        Hand.Hand a b c ->
            [ text (Dice.toString a)
            , text (Dice.toString b)
            , text (Dice.toString c)
            , text " = "
            , text (String.fromInt (Hand.toScore hand))
            ]


boardView : Model -> Html Msg
boardView model =
    div []
        [ table []
            [ thead []
                [ th [] [ text "Name" ]
                , th [] [ text "Score" ]
                , th [] [ text "Active" ]
                ]
            , tbody [] (rowsView model.game)
            ]
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
