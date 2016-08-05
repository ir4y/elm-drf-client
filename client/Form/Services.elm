module Form.Services
    exposing
        ( getFormInfoTask
        , sendFormToServerTask
        )

import HttpBuilder
import Json.Decode as Decode
import Json.Decode exposing ((:=))
import Json.Encode as Encode
import Task
import Form.Types as FormTypes
import Dict


questionOptionDecoder : Decode.Decoder FormTypes.FormInfo
questionOptionDecoder =
    Decode.at [ "actions", "POST" ] (Decode.keyValuePairs fieldInfoDecoder)


getFormInfoTask : String -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response FormTypes.FormInfo)
getFormInfoTask url =
    HttpBuilder.options url
        |> HttpBuilder.send (HttpBuilder.jsonReader questionOptionDecoder) HttpBuilder.stringReader


sendFormToServerTask : String -> FormTypes.FormData -> Task.Task (HttpBuilder.Error FormTypes.FormErrors) (HttpBuilder.Response String)
sendFormToServerTask url data =
    HttpBuilder.post url
        |> HttpBuilder.withJsonBody (formDataEncoder data)
        |> HttpBuilder.withHeader "Content-Type" "application/json"
        |> HttpBuilder.send HttpBuilder.stringReader (HttpBuilder.jsonReader (Decode.dict (Decode.list Decode.string)))


fieldInfoDecoder : Decode.Decoder FormTypes.FieldInfo
fieldInfoDecoder =
    Decode.object4 FormTypes.FieldInfo
        ("type" := Decode.string)
        ("required" := Decode.bool)
        ("read_only" := Decode.bool)
        ("label" := Decode.string)


wrapValues : ( String, String ) -> ( String, Encode.Value )
wrapValues ( k, v ) =
    ( k, Encode.string v )


formDataEncoder : FormTypes.FormData -> Encode.Value
formDataEncoder data =
    List.map wrapValues (Dict.toList data)
        |> Encode.object
