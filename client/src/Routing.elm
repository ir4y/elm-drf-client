module Routing exposing (..)

import String
import Navigation
import UrlParser exposing (..)

type Route
    = Index
    | List String
    | Change String Int
    | Add String


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ format Index (s "")
        , format Change (string </> int </> (s "change"))
        , format Add (string </> (s "add"))
        , format List string
        ]


hashParser : Navigation.Location -> Result String Route
hashParser location =
    location.hash
        |> String.dropLeft 1
        |> parse identity matchers


parser : Navigation.Parser (Result String Route)
parser =
    Navigation.makeParser hashParser


routeFromResult : Result String Route -> Route
routeFromResult result =
    case result of
        Ok route ->
            route

        Err string ->
            Index
