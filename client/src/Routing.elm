module Routing exposing (..)

import Navigation
import String
import UrlParser exposing (..)


type Route
    = Index
    | List String
    | Change String Int
    | Add String


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map Index (s "")
        , map Change (string </> int </> (s "change"))
        , map Add (string </> (s "add"))
        , map List string
        ]


hashParser : Navigation.Location -> Result String Route
hashParser location =
    parseHash matchers location |> Result.fromMaybe "parse error"


routeFromResult : Result String Route -> Route
routeFromResult result =
    case result of
        Ok route ->
            route

        Err string ->
            Index
