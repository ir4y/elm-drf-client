import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Json.Decode exposing (..)
import Task
import Debug
import FormField
import Debug



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
  }

init : (Model, Cmd Msg)
init =
  ( Model []
  , getQustionInfo
  )



-- UPDATE


type Msg
  = NoOp
  | FetchFail Http.Error
  | FetchSucceed (List (String, FormField.FieldInfo))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp -> (model, Cmd.none)
    FetchSucceed fieldInfos -> (Model fieldInfos, Cmd.none)
    FetchFail error -> case (Debug.log "error" error) of _ -> (Model [],  Cmd.none)



-- VIEW

tableItemView (name, fieldInfo) =
    tr [] [ td [] [text name]
          , td [] [text fieldInfo.label]
          ]

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text "Question model information"]
    , table [] (List.map tableItemView model.fieldInfos)
    ]



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
