module Form
    exposing
        ( Model
        , Msg
        , init
        , update
        , view
        )

import Html exposing (Html, div, text)
import Html.App as App
import HttpBuilder
import Form.Types as FormTypes
import Form.Form as Form
import Form.Services exposing (..)
import Form.Validate as Validate
import Material
import Material.Button as Button
import Material.Spinner as Loading
import Task


-- MODEL


type alias Model =
    { mdl : Material.Model
    , form : Form.Model
    , formInfo : FormTypes.FormInfo
    , preloader : Bool
    , validate : FormTypes.FormData -> FormTypes.FormErrors
    , formErrors : FormTypes.FormErrors
    , url : String
    }


init : String -> ( Model, Cmd Msg )
init url =
    ( { mdl = Material.model
      , form = Form.init
      , formInfo = []
      , preloader = True
      , validate = (\_ -> FormTypes.emptyFormErrors)
      , formErrors = FormTypes.emptyFormErrors
      , url = url
      }
    , getQustionInfo url
    )



-- UPDATE


type Msg
    = MDL (Material.Msg Msg)
    | FormMsg Form.Msg
    | FetchFail (HttpBuilder.Error String)
    | FetchSucceed (HttpBuilder.Response FormTypes.FormInfo)
    | UploadFail (HttpBuilder.Error FormTypes.FormErrors)
    | UploadSucceed (HttpBuilder.Response String)
    | SubmitForm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MDL action' ->
            Material.update action' model

        FormMsg msg' ->
            ( { model | form = Form.update model.validate msg' model.form }, Cmd.none )

        FetchSucceed response ->
            let
                validate =
                    Validate.getValidator response.data
            in
                ( { model
                    | formInfo = response.data
                    , validate = validate
                    , preloader = False
                  }
                , Cmd.none
                )

        FetchFail error ->
            ( { model | preloader = False }, Cmd.none )

        UploadSucceed response ->
            ( { model
                | form = Form.cleanup model.form
                , formErrors = FormTypes.emptyFormErrors
                , preloader = False
              }
            , Cmd.none
            )

        UploadFail error ->
            let
                model' =
                    { model | preloader = False, form = Form.cleanDirtyState model.form }
            in
                case error of
                    HttpBuilder.UnexpectedPayload _ ->
                        ( model', Cmd.none )

                    HttpBuilder.NetworkError ->
                        ( model', Cmd.none )

                    HttpBuilder.Timeout ->
                        ( model', Cmd.none )

                    HttpBuilder.BadResponse response ->
                        ( { model' | formErrors = response.data }, Cmd.none )

        SubmitForm ->
            ( { model | preloader = True }, sendQuestionToServer model.url model.form.formData )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ App.map FormMsg (Form.view model.formInfo model.formErrors model.form)
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getQustionInfo : String -> Cmd Msg
getQustionInfo url =
    getFormInfoTask url
        |> Task.perform FetchFail FetchSucceed


sendQuestionToServer : String -> FormTypes.FormData -> Cmd Msg
sendQuestionToServer url data =
    sendFormToServerTask url data
        |> Task.perform UploadFail UploadSucceed
