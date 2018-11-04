module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Dice exposing (Face(..))
import Hand exposing (Hand)
import Html exposing (..)
import Html.Attributes as At
import Html.Events exposing (..)
import Player exposing (Player)
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
    , players : PlayerList
    , playerInput : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { hand = Hand.empty
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
            ( { model | players = PlayerList.add (Player model.playerInput) model.players }
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
        , playerListView model
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


playerListView : Model -> Html Msg
playerListView model =
    div []
        [ input [ At.value model.playerInput, onInput InputPlayer ] []
        , button [ onClick NewPlayer ] [ text "Add player" ]
        , playersView model.players
        ]


playersView : PlayerList -> Html Msg
playersView list =
    ul []
        (list
            |> PlayerList.toList
            |> List.map (\player -> li [] [ text player.name ])
        )
