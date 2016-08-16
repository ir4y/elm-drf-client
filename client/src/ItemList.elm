module ItemList exposing (Msg, update, view)

import Dict
import Debug
import Material.Table exposing (..)
import Material.Spinner as Loading
import Html exposing (text)


type Msg
    = NoOp


type alias Model =
    List (Dict.Dict String String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view model =
    let
        maybeHead =
            List.head model
    in
        case maybeHead of
            Nothing ->
                Loading.spinner [ Loading.active True ]

            Just head ->
                table []
                    [ thead []
                        [ tr []
                            (head
                                |> Dict.keys
                                |> List.map (\value -> th [] [ text value ])
                            )
                        ]
                    , tbody []
                        (model
                            |> List.map
                                (\item ->
                                    tr []
                                        (item
                                            |> Dict.values
                                            |> List.map (\value -> td [] [ text value ])
                                        )
                                )
                        )
                    ]
