module Types exposing (..)


type RemoteData e a
    = NotAsked
    | Loading
    | Success a
    | Failure e
