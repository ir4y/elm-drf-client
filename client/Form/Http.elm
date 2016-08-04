module Form.Http exposing
  ( getFormInfoTask
  , sendFormToServerTask
  )

import HttpBuilder
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Form.Data as FormData


questionOptionDecoder : Decode.Decoder FormData.FormInfo
questionOptionDecoder =
    Decode.at ["actions", "POST"] (Decode.keyValuePairs FormData.fieldInfoDecoder)

getFormInfoTask : String -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response FormData.FormInfo)
getFormInfoTask url =
  HttpBuilder.options url
    |> HttpBuilder.send (HttpBuilder.jsonReader questionOptionDecoder) HttpBuilder.stringReader

sendFormToServerTask : String -> FormData.FormData -> Task.Task (HttpBuilder.Error FormData.FormErrors) (HttpBuilder.Response String)
sendFormToServerTask url data =
  HttpBuilder.post url
    |> HttpBuilder.withJsonBody (FormData.formDataEncoder data)
    |> HttpBuilder.withHeader "Content-Type" "application/json"
    |> HttpBuilder.send HttpBuilder.stringReader (HttpBuilder.jsonReader (Decode.dict (Decode.list Decode.string)))
