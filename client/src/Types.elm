module Types exposing (..)

import Maybe


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
