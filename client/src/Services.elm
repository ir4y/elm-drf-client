module Services exposing (getResourcesInfoTask, getResourceTask)

import HttpBuilder
import Json.Decode as Decode
import Task
import Dict


getResourcesInfoTask : String -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response (Dict.Dict String String))
getResourcesInfoTask url =
    HttpBuilder.get url
        |> HttpBuilder.send (HttpBuilder.jsonReader (Decode.dict Decode.string)) HttpBuilder.stringReader


resourse : Decode.Decoder String
resourse =
    Decode.oneOf
        [ Decode.string
        , Decode.map toString Decode.int
        ]


getResourceTask : String -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response (List (Dict.Dict String String)))
getResourceTask url =
    HttpBuilder.get url
        |> HttpBuilder.send (HttpBuilder.jsonReader (Decode.list (Decode.dict resourse))) HttpBuilder.stringReader
