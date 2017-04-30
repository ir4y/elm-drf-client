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

import Form.Form as Form
import Form.Services exposing (..)
import Form.Types as FormTypes
import Form.Validate as Validate
import Types
import Http
import Dict
import Html exposing (Html, div, text)
import HttpBuilder
import Material
import Material.Button as Button
import Material.Spinner as Loading
import Material.Options as Options
import Task


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
    | FetchFail Http.Error
    | FetchSucceed FormTypes.FormInfo
    | UploadFail Http.Error
    | UploadSucceed
    | SubmitForm
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        MDL action_ ->
            Material.update (\_ -> NoOp) action_ model

        FormMsg msg_ ->
            case model.formState of
                Types.Success formState ->
                    ( { model | form = Form.update formState.validate msg_ model.form }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FetchSucceed formInfo ->
            let
                validate =
                    Validate.getValidator formInfo
            in
                ( { model
                    | formState =
                        Types.Success
                            { formInfo = formInfo
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

        UploadSucceed ->
            ( { model
                | form = Form.cleanup model.form
                , formErrors = FormTypes.emptyFormErrors
                , preloader = False
              }
            , Cmd.none
            )

        UploadFail error ->
            let
                model_ =
                    { model | preloader = False, form = Form.cleanDirtyState model.form }
            in
                ( model_, Cmd.none )
                -- case error of
                --     HttpBuilder.UnexpectedPayload _ ->
                --         ( model_, Cmd.none )

                --     HttpBuilder.NetworkError ->
                --         ( model_, Cmd.none )

                --     HttpBuilder.Timeout ->
                --         ( model_, Cmd.none )

                --     HttpBuilder.BadResponse response ->
                --         ( { model_ | formErrors = response.data }, Cmd.none )

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
                        [ Html.map FormMsg (Form.view formState.formInfo model.formErrors model.form)
                        , Button.render MDL
                            [ 0 ]
                            model.mdl
                            [ Button.raised
                            , Options.onClick SubmitForm
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
        (\result ->
            case result of
                Result.Ok formInfo ->
                    FetchSucceed formInfo

                Result.Err error ->
                    FetchFail error
        )


sendFormToServer : String -> FormTypes.FormData -> Cmd Msg
sendFormToServer url data =
    sendFormToServerTask url data
        (\result ->
            case result of
                Result.Ok _ ->
                    UploadSucceed

                Result.Err error ->
                    UploadFail error
        )


updateFormAtServer : String -> FormTypes.FormData -> Cmd Msg
updateFormAtServer url data =
    updateFormAtServerTask url data
        (\result ->
            case result of
                Result.Ok _ ->
                    UploadSucceed

                Result.Err error ->
                    UploadFail error
        )
