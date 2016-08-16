module ItemList exposing (Msg, update, view)

import Dict
import Html exposing (div)


type Msg
    = NoOp


type alias Model =
    List (Dict.Dict String String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view model =
    div [] []
