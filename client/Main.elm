import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Json.Decode exposing (..)
import Json.Encode as JsonEncode
import Task
import FormField
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
import Task exposing (Task, andThen, mapError, succeed, fail)



main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { fieldInfos : List (String, FormField.FieldInfo)
  , mdl: Material.Model
  , formData: Dict.Dict String String
  , formErrors: Dict.Dict String (List String)
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
  | FetchFail Http.Error
  | FetchSucceed (List (String, FormField.FieldInfo))
  | UploadFail MyHttpError
  | UploadSucceed String
  | SubmitForm


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
    MDL action' ->
      Material.update action' model
    FetchSucceed fieldInfos ->
      ({ model | fieldInfos = fieldInfos, preloader = False } , Cmd.none)
    FetchFail error ->
      ({model | preloader = False},  Cmd.none)
    UserInput key data ->
      ({ model | formData = Dict.insert key data model.formData }, Cmd.none)
    SubmitForm ->
      ({model | preloader = True}, sendQuestionToServer model.formData)
    UploadSucceed text ->
      ({model | formErrors = Dict.empty, formData = Dict.empty, preloader=False}, Cmd.none)
    UploadFail error ->
      let model' = {model | preloader = False}
      in
        case error of
          Timeout  -> (model',  Cmd.none)
          NetworkError -> (model',  Cmd.none)
          UnexpectedPayload _  -> (model',  Cmd.none)
          BadResponse 400 payload -> ({model' | formErrors = extractErrors payload}, Cmd.none)
          BadResponse _ _ -> (model', Cmd.none)




-- VIEW

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
    let url = "http://localhost:8000/poll/question/"
        requestParams = { verb = "OPTIONS"
                        , url = url
                        , body = Http.empty
                        , headers = []
                        }
        request = Http.send Http.defaultSettings requestParams
    in Task.perform FetchFail FetchSucceed (Http.fromJson questionOptionDecoder request)

questionOptionDecoder : Json.Decoder (List (String, FormField.FieldInfo))
questionOptionDecoder =
    Json.at ["actions", "POST"] (Json.keyValuePairs FormField.fieldInfoDecoder)

sendQuestionToServer data =
    let url = "http://localhost:8000/poll/question/"
        dataList = encodeData data
        encodedData = Http.string (JsonEncode.encode 0 (JsonEncode.object dataList))
        requestParams = { verb = "POST"
                        , url = url
                        , body = encodedData
                        , headers = [("Content-Type", "application/json")]
                        }
        request = Http.send Http.defaultSettings requestParams
    in Task.perform UploadFail UploadSucceed (handleUpload request)

wrapValues (k,v) =
  (k, JsonEncode.string v)

encodeData data =
  List.map wrapValues (Dict.toList data)


extractErrors payload = Result.withDefault Dict.empty (Json.decodeString (Json.dict (Json.list Json.string)) payload)


type MyHttpError
  = Timeout
  | NetworkError
  | UnexpectedPayload String
  | BadResponse Int String


handleUpload : Task Http.RawError Http.Response -> Task MyHttpError String
handleUpload response =
  mapError promoteError response `Task.andThen` handleResponse

handleResponse : Http.Response -> Task MyHttpError String
handleResponse response =
  if 200 <= response.status && response.status < 300 then
      case response.value of
                Http.Text str ->
                  Task.succeed str
                _ ->
                  Task.fail (UnexpectedPayload "Response body is a blob, expecting a string.")
  else

      case response.value of
          Http.Text str ->
            Task.fail (BadResponse response.status str)
          _ ->
            Task.fail (UnexpectedPayload "Response body is a blob, expecting a string.")



promoteError : Http.RawError -> MyHttpError
promoteError rawError =
  case rawError of
    Http.RawTimeout -> Timeout
    Http.RawNetworkError -> NetworkError
