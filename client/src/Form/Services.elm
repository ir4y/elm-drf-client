module Form.Services
    exposing
        ( getFormInfoTask
        , sendFormToServerTask
        , updateFormAtServerTask
        )

import Form.Types as FormTypes
import Debug
import Dict
import HttpBuilder
import Json.Decode as Decode
import Json.Decode exposing ((:=))
import Json.Encode as Encode
import Result
import Task


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


updateFormAtServerTask : String -> FormTypes.FormData -> Task.Task (HttpBuilder.Error FormTypes.FormErrors) (HttpBuilder.Response String)
updateFormAtServerTask url data =
    HttpBuilder.put url
        |> HttpBuilder.withJsonBody (formDataEncoder data)
        |> HttpBuilder.withHeader "Content-Type" "application/json"
        |> HttpBuilder.send HttpBuilder.stringReader (HttpBuilder.jsonReader (Decode.dict (Decode.list Decode.string)))


fieldInfoDecoder : Decode.Decoder FormTypes.FieldInfo
fieldInfoDecoder =
    Decode.object5 FormTypes.FieldInfo
        ("type" := typeDecoder)
        ("required" := Decode.bool)
        ("read_only" := Decode.bool)
        ("label" := Decode.string)
        (Decode.maybe ("choices" := (Decode.list choiceDecoder)))


typeDecoder : Decode.Decoder FormTypes.FieldType
typeDecoder =
    let
        decodeType string =
            case string of
                "integer" ->
                    Result.Ok FormTypes.Integer

                "datetime" ->
                    Result.Ok FormTypes.Datetime

                "string" ->
                    Result.Ok FormTypes.String

                "field" ->
                    Result.Ok FormTypes.Field

                _ ->
                    Result.Err ("Not valid field type: " ++ string)
    in
        Decode.customDecoder Decode.string decodeType


choiceDecoder : Decode.Decoder FormTypes.Choice
choiceDecoder =
    Decode.object2 FormTypes.Choice
        ("value" := Decode.string)
        ("display_name" := Decode.string)


wrapValues : ( String, String ) -> ( String, Encode.Value )
wrapValues ( k, v ) =
    ( k, Encode.string v )


formDataEncoder : FormTypes.FormData -> Encode.Value
formDataEncoder data =
    List.map wrapValues (Dict.toList data)
        |> Encode.object
