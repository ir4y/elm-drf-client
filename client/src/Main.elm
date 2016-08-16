module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Html.App as App
import Form
import Material.Scheme
import Navigation
import Routing
import Dict
import Services exposing (getResourcesInfoTask)
import HttpBuilder
import Maybe
import Task
import ItemList


main =
    Navigation.program Routing.parser
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }



-- MODEL


type alias PageModel =
    { url : String
    , dataList : List (Dict.Dict String String)
    , form : Form.Model
    }


initPageModel : String -> ( PageModel, Cmd Form.Msg )
initPageModel url =
    let
        ( form, cmd ) =
            Form.init url
    in
        ( { url = url
          , dataList = []
          , form = form
          }
        , cmd
        )


type alias Model =
    { schema : Dict.Dict String PageModel
    , route : Routing.Route
    }


init routeResult =
    ( { schema = Dict.empty
      , route = Routing.routeFromResult routeResult
      }
    , getQustionInfo
    )


urlUpdate : Result String Routing.Route -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    let
        currentRoute =
            Routing.routeFromResult result
    in
        ( { model | route = currentRoute }, Cmd.none )



-- UPDATE


type Msg
    = FormMsg String Form.Msg
    | ListMsg String ItemList.Msg
    | FetchFail (HttpBuilder.Error String)
    | FetchSucceed (HttpBuilder.Response (Dict.Dict String String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormMsg name msg' ->
            apply_update_or_noting model
                name
                msg'
                Form.update
                .form
                (\pageModel form -> { pageModel | form = form })
                (FormMsg name)

        ListMsg name msg' ->
            apply_update_or_noting model
                name
                msg'
                ItemList.update
                .dataList
                (\pageModel dataList -> { pageModel | dataList = dataList })
                (ListMsg name)

        FetchSucceed response ->
            let
                initSchema =
                    Dict.map (\key value -> initPageModel value) response.data

                schema =
                    Dict.map (\key value -> fst value) initSchema

                cmds =
                    Dict.map (\key value -> Cmd.map (FormMsg key) (snd value)) initSchema
                        |> Dict.values
                        |> Cmd.batch
            in
                ( { model | schema = schema }, cmds )

        _ ->
            ( model, Cmd.none )


apply_update_or_noting model name msg' update getSubState setSubState msgWrap =
    case (Dict.get name model.schema) of
        Nothing ->
            ( model, Cmd.none )

        Just pageModel ->
            let
                ( newSubState, cmd' ) =
                    update msg' (getSubState pageModel)

                pageModel' =
                    setSubState pageModel newSubState

                schema =
                    Dict.insert name pageModel' model.schema
            in
                ( { model | schema = schema }, cmd' |> Cmd.map msgWrap )



-- VIEW


view : Model -> Html Msg
view model =
    let
        header =
            div []
                (List.concat
                    [ (List.map
                        (\path -> a [ href ("#" ++ path) ] [ text (path ++ " ") ])
                        (Dict.keys model.schema)
                      )
                    , [ br [] [] ]
                    , (List.map
                        (\path -> a [ href ("#" ++ path ++ "/add") ] [ text ("add " ++ " " ++ path ++ " ") ])
                        (Dict.keys model.schema)
                      )
                    ]
                )

        formView =
            case model.route of
                Routing.Add name ->
                    get_view_or_empy_div name
                        (Dict.get name model.schema)
                        (FormMsg name)
                        (\page -> Form.view page.form)

                Routing.List name ->
                    get_view_or_empy_div name
                        (Dict.get name model.schema)
                        (ListMsg name)
                        (\page -> ItemList.view page.dataList)

                _ ->
                    div [] []
    in
        div [] [ header, formView ] |> Material.Scheme.top


get_view_or_empy_div : String -> Maybe PageModel -> (m -> Msg) -> (PageModel -> Html m) -> Html Msg
get_view_or_empy_div name maybePageModel msgWrap view =
    case maybePageModel of
        Nothing ->
            div [] []

        Just page ->
            (App.map msgWrap (view page))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


getQustionInfo : Cmd Msg
getQustionInfo =
    getResourcesInfoTask "http://localhost:8000/poll/"
        |> Task.perform FetchFail FetchSucceed
