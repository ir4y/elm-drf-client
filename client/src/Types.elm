module Types exposing (..)

import Maybe
import Http
import Dict exposing (Dict)
import Json.Decode exposing (Decoder, dict, list, string, map, decodeString)


type RemoteData e a
    = NotAsked
    | Loading
    | Success a
    | Failure e


asMaybe : RemoteData e a -> Maybe a
asMaybe remoteData =
    case remoteData of
        Success data ->
            Just data

        _ ->
            Nothing


type DrfError
    = DrfError (Dict String (List String))
    | NonDrfError Http.Error


drfErrorDecode : Decoder DrfError
drfErrorDecode =
    (dict (list string)) |> map DrfError


fromHttpErrorToDrfError : Http.Error -> DrfError
fromHttpErrorToDrfError error =
    case error of
        Http.BadStatus response ->
            decodeString drfErrorDecode response.body
                |> Result.withDefault (NonDrfError error)
        _ ->
            NonDrfError error
