module Services exposing (getResourcesInfoTask, getResourceTask)

import Dict
import Http
import HttpBuilder
import Json.Decode as Decode
import Task


getResourcesInfoTask : String -> (Result Http.Error (Dict.Dict String String) -> msg) -> Cmd msg
getResourcesInfoTask url handler =
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.dict Decode.string))
        |> HttpBuilder.send handler


resourse : Decode.Decoder String
resourse =
    Decode.oneOf
        [ Decode.string
        , Decode.map toString Decode.int
        ]


getResourceTask : String -> (Result Http.Error (List (Dict.Dict String String)) -> msg) -> Cmd msg
getResourceTask url handler =
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list (Decode.dict resourse)))
        |> HttpBuilder.send handler

