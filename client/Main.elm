import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import HttpBuilder
import Json.Decode as Decode
import Json.Encode as Encode
import Form
import Dict
import Material
import Material.Scheme
import Material.Table as Table
import Material.Textfield as Textfield
import Material.Button as Button exposing (..)
import Material.Spinner as Loading
import String
import Maybe
import Result
import Task
import Http


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { fieldInfos : Form.FormInfo
  , mdl: Material.Model
  , formData: Form.FormData
  , formErrors: Form.FormErrors
  , preloader: Bool
  }

init : (Model, Cmd Msg)
init =
  ( { fieldInfos = []
    , mdl = Material.model
    , formData = Dict.empty
    , formErrors = Dict.empty
    , preloader = True
    }
  , getQustionInfo
  )



-- UPDATE


type Msg
  = NoOp
  | MDL (Material.Msg Msg)
  | UserInput String String
  | FetchFail (HttpBuilder.Error String)
  | FetchSucceed (HttpBuilder.Response Form.FormInfo)
  | UploadFail (HttpBuilder.Error Form.FormErrors)
  | UploadSucceed (HttpBuilder.Response String)
  | SubmitForm


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
    MDL action' ->
      Material.update action' model
    FetchSucceed response ->
      ({ model | fieldInfos = response.data, preloader = False } , Cmd.none)
    FetchFail error ->
      ({model | preloader = False},  Cmd.none)
    UserInput key data ->
      ({ model | formData = Dict.insert key data model.formData }, Cmd.none)
    SubmitForm ->
      ({model | preloader = True}, sendQuestionToServer model.formData)
    UploadSucceed response ->
      ({model | formErrors = Dict.empty, formData = Dict.empty, preloader=False}, Cmd.none)
    UploadFail error ->
      let model' = {model | preloader = False}
      in
        case error of
          HttpBuilder.UnexpectedPayload _  -> (model',  Cmd.none)
          HttpBuilder.NetworkError -> (model',  Cmd.none)
          HttpBuilder.Timeout  -> (model',  Cmd.none)
          HttpBuilder.BadResponse response -> ({model' | formErrors = response.data}, Cmd.none)




-- VIEW


tableItemView : Model -> (String, Form.FieldInfo) -> Int -> Html Msg
tableItemView model (name, fieldInfo) index =
    Table.tr [] [ Table.td [] [text fieldInfo.label]
                , Table.td [] [Textfield.render MDL [index] model.mdl
                                                (List.concat
                                                  [ [ Textfield.label fieldInfo.label
                                                    , Textfield.value (Maybe.withDefault "" (Dict.get name model.formData))
                                                    , Textfield.onInput (UserInput name)
                                                    ]
                                                  , if fieldInfo.readOnly then [Textfield.disabled] else []
                                                  ])
                              ]
                , Table.td [] (List.map text (Maybe.withDefault [] (Dict.get name model.formErrors)))
                ]

view : Model -> Html Msg
view model =
  div []
    (List.concat
    [ [ h2 [] [text "Question model information"]
      , Table.table [] (List.map2 (tableItemView model) model.fieldInfos [1 .. List.length model.fieldInfos])
      ]
    , if model.preloader then [Loading.spinner [Loading.active model.preloader]] else [
        Button.render MDL [0] model.mdl
          [ Button.raised
          , Button.onClick SubmitForm ]
          [text "Submit"]
        ]
    ])
  |> Material.Scheme.top



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP

getQustionInfo : Cmd Msg
getQustionInfo =
  Task.perform FetchFail FetchSucceed getQustionInfoTask

getQustionInfoTask : Task.Task (HttpBuilder.Error String) (HttpBuilder.Response Form.FormInfo)
getQustionInfoTask =
  HttpBuilder.options "http://localhost:8000/poll/question/"
    |> HttpBuilder.send (HttpBuilder.jsonReader questionOptionDecoder) HttpBuilder.stringReader

questionOptionDecoder : Decode.Decoder Form.FormInfo
questionOptionDecoder =
    Decode.at ["actions", "POST"] (Decode.keyValuePairs Form.fieldInfoDecoder)

sendQuestionToServer : Form.FormData -> Cmd Msg
sendQuestionToServer data =
  Task.perform UploadFail UploadSucceed (sendQuestionToServerTask data)


sendQuestionToServerTask : Form.FormData -> Task.Task (HttpBuilder.Error Form.FormErrors) (HttpBuilder.Response String)
sendQuestionToServerTask data =
  HttpBuilder.post "http://localhost:8000/poll/question/"
    |> HttpBuilder.withJsonBody (encodeFormData data)
    |> HttpBuilder.withHeader "Content-Type" "application/json"
    |> HttpBuilder.send HttpBuilder.stringReader (HttpBuilder.jsonReader (Decode.dict (Decode.list Decode.string)))

--wrapValues :
wrapValues : (String, String) -> (String, Encode.Value)
wrapValues (k,v) =
  (k, Encode.string v)

encodeFormData : Form.FormData -> Encode.Value
encodeFormData data =
  List.map wrapValues (Dict.toList data)
  |> Encode.object
