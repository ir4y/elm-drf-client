module Services exposing (getResourcesInfoTask)

import HttpBuilder
import Json.Decode as Decode
import Task
import Dict


getResourcesInfoTask : String -> Task.Task (HttpBuilder.Error String) (HttpBuilder.Response (Dict.Dict String String))
getResourcesInfoTask url =
    HttpBuilder.get url
        |> HttpBuilder.send (HttpBuilder.jsonReader (Decode.dict Decode.string)) HttpBuilder.stringReader
