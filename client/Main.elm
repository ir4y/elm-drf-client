module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import HttpBuilder
import Form.Data as FormData
import Form.Form as Form
import Form.Http exposing (..)
import Dict
import Material
import Material.Scheme
import Material.Table as Table
import Material.Textfield as Textfield
import Material.Button as Button exposing (..)
import Material.Spinner as Loading
import Maybe
import Task


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { mdl : Material.Model
    , form : Form.Model
    , formInfo : FormData.FormInfo
    , preloader : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { mdl = Material.model
      , form = Form.init
      , formInfo = []
      , preloader = True
      }
    , getQustionInfo
    )



-- UPDATE


type Msg
    = MDL (Material.Msg Msg)
    | FormMsg Form.Msg
    | FetchFail (HttpBuilder.Error String)
    | FetchSucceed (HttpBuilder.Response FormData.FormInfo)
    | UploadFail (HttpBuilder.Error FormData.FormErrors)
    | UploadSucceed (HttpBuilder.Response String)
    | SubmitForm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MDL action' ->
            Material.update action' model

        FormMsg msg' ->
            ( { model | form = Form.update msg' model.form }, Cmd.none )

        FetchSucceed response ->
            ( { model | formInfo = response.data, preloader = False }, Cmd.none )

        FetchFail error ->
            ( { model | preloader = False }, Cmd.none )

        UploadSucceed response ->
            ( { model | form = Form.cleanup model.form, preloader = False }, Cmd.none )

        UploadFail error ->
            let
                model' =
                    { model | preloader = False }
            in
                case error of
                    HttpBuilder.UnexpectedPayload _ ->
                        ( model', Cmd.none )

                    HttpBuilder.NetworkError ->
                        ( model', Cmd.none )

                    HttpBuilder.Timeout ->
                        ( model', Cmd.none )

                    HttpBuilder.BadResponse response ->
                        ( { model' | form = Form.setFormErrors model'.form response.data }, Cmd.none )

        SubmitForm ->
            ( { model | preloader = True }, sendQuestionToServer model.form.formData )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ App.map FormMsg (Form.view model.formInfo model.form)
        , if model.preloader then
            Loading.spinner [ Loading.active model.preloader ]
          else
            Button.render MDL
                [ 0 ]
                model.mdl
                [ Button.raised
                , Button.onClick SubmitForm
                ]
                [ text "Submit" ]
        ]
        |> Material.Scheme.top



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getQustionInfo : Cmd Msg
getQustionInfo =
    getFormInfoTask "http://localhost:8000/poll/question/"
        |> Task.perform FetchFail FetchSucceed


sendQuestionToServer : FormData.FormData -> Cmd Msg
sendQuestionToServer data =
    sendFormToServerTask "http://localhost:8000/poll/question/" data
        |> Task.perform UploadFail UploadSucceed
