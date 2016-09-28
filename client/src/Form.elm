module Form
    exposing
        ( Model
        , Msg
        , init
        , initEmpty
        , initEditForm
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
import Types
import Dict


-- MODEL


type alias FormState =
    Types.RemoteData String
        { formInfo : FormTypes.FormInfo
        , validate : FormTypes.FormData -> FormTypes.FormErrors
        }


type alias Model =
    { mdl : Material.Model
    , form : Form.Model
    , formState : FormState
    , formErrors : FormTypes.FormErrors
    , url : String
    , preloader : Bool
    }


init : String -> ( Model, Cmd Msg )
init url =
    ( { mdl = Material.model
      , form = Form.init
      , formState = Types.Loading
      , formErrors = FormTypes.emptyFormErrors
      , url = url
      , preloader = False
      }
    , getFormInfo url
    )


initEmpty : Model
initEmpty =
    { mdl = Material.model
    , form = Form.init
    , formState = Types.NotAsked
    , formErrors = FormTypes.emptyFormErrors
    , url = ""
    , preloader = False
    }


initEditForm : String -> FormState -> FormTypes.FormData -> Model
initEditForm url formState formData =
    { mdl = Material.model
    , form = Form.initWithData formData
    , formState = formState
    , formErrors = FormTypes.emptyFormErrors
    , url = url
    , preloader = False
    }



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
            case model.formState of
                Types.Success formState ->
                    ( { model | form = Form.update formState.validate msg' model.form }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FetchSucceed response ->
            let
                validate =
                    Validate.getValidator response.data
            in
                ( { model
                    | formState =
                        Types.Success
                            { formInfo = response.data
                            , validate = validate
                            }
                  }
                , Cmd.none
                )

        FetchFail error ->
            ( { model
                | formState =
                    error
                        |> toString
                        |> Types.Failure
              }
            , Cmd.none
            )

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
            ( { model | preloader = True }
            , case Dict.get "id" model.form.formData of
                Nothing ->
                    sendFormToServer model.url model.form.formData

                Just _ ->
                    updateFormAtServer model.url model.form.formData
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ case model.formState of
            Types.NotAsked ->
                div [] []

            Types.Loading ->
                Loading.spinner [ Loading.active model.preloader ]

            Types.Failure message ->
                div [] [ text message ]

            Types.Success formState ->
                if model.preloader then
                    Loading.spinner [ Loading.active model.preloader ]
                else
                    div []
                        [ App.map FormMsg (Form.view formState.formInfo model.formErrors model.form)
                        , Button.render MDL
                            [ 0 ]
                            model.mdl
                            [ Button.raised
                            , Button.onClick SubmitForm
                            ]
                            [ text "Submit" ]
                        ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getFormInfo : String -> Cmd Msg
getFormInfo url =
    getFormInfoTask url
        |> Task.perform FetchFail FetchSucceed


sendFormToServer : String -> FormTypes.FormData -> Cmd Msg
sendFormToServer url data =
    sendFormToServerTask url data
        |> Task.perform UploadFail UploadSucceed


updateFormAtServer : String -> FormTypes.FormData -> Cmd Msg
updateFormAtServer url data =
    updateFormAtServerTask url data
        |> Task.perform UploadFail UploadSucceed
