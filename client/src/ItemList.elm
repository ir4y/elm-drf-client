module ItemList exposing (Msg, Model, DataList, update, view, init)

import Dict
import Debug
import Material.Table exposing (..)
import Material.Spinner as Loading
import Html exposing (text, div, a)
import Html.Attributes exposing (href)
import Types
import Maybe


type alias Id =
    String


type Msg
    = NoOp
    | Clicked Id


type alias DataList =
    List (Dict.Dict String String)


type alias Model =
    Types.RemoteData String DataList


init : Model
init =
    Types.NotAsked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view name model =
    case model of
        Types.NotAsked ->
            div [] [ text "not asked yet" ]

        Types.Loading ->
            Loading.spinner [ Loading.active True ]

        Types.Failure error ->
            div [] [ text error ]

        Types.Success dataList ->
            let
                maybeHead =
                    List.head dataList
            in
                case maybeHead of
                    Nothing ->
                        div [] [ text "There are no data at the database" ]

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
                                (dataList
                                    |> List.map
                                        (\item ->
                                            let
                                                id =
                                                    (Dict.get "id" item) |> Maybe.withDefault ""

                                                item' =
                                                    (Dict.remove "id" item)
                                            in
                                                tr []
                                                    ((td []
                                                        [ a [ href ("#" ++ name ++ "/" ++ id ++ "/change") ]
                                                            [ text id ]
                                                        ]
                                                     )
                                                        :: (item'
                                                                |> Dict.values
                                                                |> List.map (\value -> td [] [ text value ])
                                                           )
                                                    )
                                        )
                                )
                            ]
