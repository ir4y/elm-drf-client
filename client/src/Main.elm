module Main exposing (..)

import Html exposing (Html, div, h2, a, text)
import Html.Attributes exposing (href)
import Html.App as App
import Form as Form
import Material.Scheme
import Navigation
import Routing


main =
    Navigation.program Routing.parser
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { questionForm : Form.Model
    , answerForm : Form.Model
    , route : Routing.Route
    }


question =
    Form.init "http://localhost:8000/poll/question/"


answer =
    Form.init "http://localhost:8000/poll/answer/"


initModel route =
    ( { questionForm = fst question
      , answerForm = fst answer
      , route = route
      }
    , Cmd.batch
        [ snd question |> Cmd.map QuestionFormMsg
        , snd answer |> Cmd.map AnswerFormMsg
        ]
    )


init result =
    let
        currentRoute =
            Routing.routeFromResult result
    in
        initModel currentRoute


urlUpdate : Result String Routing.Route -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    let
        currentRoute =
            Routing.routeFromResult result
    in
        ( { model | route = currentRoute }, Cmd.none )



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
    let
        header =
            div []
                [ a [ href "#add/questions" ] [ text "Questions" ]
                , a [ href "#add/answers" ] [ text "Answers" ]
                ]
    in
        (case model.route of
            Routing.AddForm "questions" ->
                div []
                    [ header
                    , h2 [] [ text "Questions" ]
                    , App.map QuestionFormMsg (Form.view model.questionForm)
                    ]

            Routing.AddForm "answers" ->
                div []
                    [ header
                    , h2 [] [ text "Answers" ]
                    , App.map AnswerFormMsg (Form.view model.answerForm)
                    ]

            _ ->
                header
        )
            |> Material.Scheme.top



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
