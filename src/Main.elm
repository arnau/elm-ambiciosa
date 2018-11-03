module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Dice exposing (Face(..))
import Hand exposing (Hand)
import Html exposing (..)
import Html.Attributes as At
import Html.Events exposing (..)
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { hand = Hand.empty }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewHand Hand.Hand


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
