module Main exposing (..)

import Html exposing (Html, div)
import Html.App as App
import Form as Form
import Material.Scheme


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { questionForm : Form.Model
    , answerForm : Form.Model
    }


question =
    Form.init "http://localhost:8000/poll/question/"


answer =
    Form.init "http://localhost:8000/poll/answer/"


init =
    ( { questionForm = fst question
      , answerForm = fst answer
      }
    , Cmd.batch
        [ snd question |> Cmd.map QuestionFormMsg
        , snd answer |> Cmd.map AnswerFormMsg
        ]
    )



-- UPDATE


type Msg
    = QuestionFormMsg Form.Msg
    | AnswerFormMsg Form.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QuestionFormMsg msg' ->
            let
                action =
                    Form.update msg' model.questionForm
            in
                ( { model | questionForm = fst action }, snd action |> Cmd.map QuestionFormMsg )

        AnswerFormMsg msg' ->
            let
                action =
                    Form.update msg' model.answerForm
            in
                ( { model | answerForm = fst action }, snd action |> Cmd.map AnswerFormMsg )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ App.map QuestionFormMsg (Form.view model.questionForm)
        , App.map AnswerFormMsg (Form.view model.answerForm)
        ]
        |> Material.Scheme.top



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
