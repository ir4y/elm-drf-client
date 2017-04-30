module Form.Services
    exposing
        ( getFormInfoTask
        , sendFormToServerTask
        , updateFormAtServerTask
        )

import Form.Types as FormTypes
import Debug
import Http
import Dict
import HttpBuilder
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode as Encode
import Result
import Task


questionOptionDecoder : Decode.Decoder FormTypes.FormInfo
questionOptionDecoder =
    Decode.at [ "actions", "POST" ] (Decode.keyValuePairs fieldInfoDecoder)


getFormInfoTask : String -> (Result Http.Error FormTypes.FormInfo -> msg) -> Cmd msg
getFormInfoTask url handler =
    HttpBuilder.options url
        |> HttpBuilder.withExpect (Http.expectJson questionOptionDecoder)
        |> HttpBuilder.send handler


sendFormToServerTask : String -> FormTypes.FormData -> (Result Http.Error () -> msg) -> Cmd msg
sendFormToServerTask url data handler =
    HttpBuilder.post url
        |> HttpBuilder.withJsonBody (formDataEncoder data)
        |> HttpBuilder.send handler


updateFormAtServerTask : String -> FormTypes.FormData -> (Result Http.Error () -> msg) -> Cmd msg
updateFormAtServerTask url data handler =
    HttpBuilder.put url
        |> HttpBuilder.withJsonBody (formDataEncoder data)
        |> HttpBuilder.send handler


fieldInfoDecoder : Decode.Decoder FormTypes.FieldInfo
fieldInfoDecoder =
    decode FormTypes.FieldInfo
        |> required "type" typeDecoder
        |> required "required" Decode.bool
        |> required "read_only" Decode.bool
        |> required "label" Decode.string
        |> optional "choices" ((Decode.list choiceDecoder) |> Decode.map Just) Nothing


typeDecoder : Decode.Decoder FormTypes.FieldType
typeDecoder =
    let
        decodeType string =
            case string of
                "integer" ->
                    Decode.succeed FormTypes.Integer

                "datetime" ->
                    Decode.succeed FormTypes.Datetime

                "string" ->
                    Decode.succeed FormTypes.String

                "field" ->
                    Decode.succeed FormTypes.Field

                _ ->
                    Decode.fail ("Not valid field type: " ++ string)
    in
        Decode.string |> Decode.andThen decodeType


choiceDecoder : Decode.Decoder FormTypes.Choice
choiceDecoder =
    decode FormTypes.Choice
        |> required "value" Decode.string
        |> required "display_name" Decode.string


wrapValues : ( String, String ) -> ( String, Encode.Value )
wrapValues ( k, v ) =
    ( k, Encode.string v )


formDataEncoder : FormTypes.FormData -> Encode.Value
formDataEncoder data =
    List.map wrapValues (Dict.toList data)
        |> Encode.object
