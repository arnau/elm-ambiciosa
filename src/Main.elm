module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Dice exposing (Face(..))
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
    { dieFace : Face
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Ace
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewFace Dice.Face


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewFace Dice.roll
            )

        NewFace newFace ->
            ( Model newFace
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
        [ div [ At.style "font-size" "100px" ] [ text (Dice.toString model.dieFace) ]
        , button [ onClick Roll ] [ text "Roll" ]
        ]
