module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Html.App as App
import Form
import Material
import Material.Color as Color
import Material.Scheme
import Material.Layout as Layout
import Navigation
import Routing
import Navigation
import Dict
import Services exposing (getResourcesInfoTask, getResourceTask)
import HttpBuilder
import Maybe
import Task
import ItemList
import Array


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
    , mdl : Material.Model
    }


init routeResult =
    ( { schema = Dict.empty
      , route = Routing.routeFromResult routeResult
      , mdl = Material.model
      }
    , getQustionInfo
    )


urlUpdate : Result String Routing.Route -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    let
        currentRoute =
            Routing.routeFromResult result

        cmd =
            case currentRoute of
                Routing.List name ->
                    case Dict.get name model.schema of
                        Nothing ->
                            Cmd.none

                        Just pageModel ->
                            getResource name pageModel.url

                _ ->
                    Cmd.none
    in
        ( { model | route = currentRoute }, cmd )



-- UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | SelectTab Int
    | FormMsg String Form.Msg
    | ListMsg String ItemList.Msg
    | FetchFail (HttpBuilder.Error String)
    | FetchSucceed (HttpBuilder.Response (Dict.Dict String String))
    | FetchResourceFail (HttpBuilder.Error String)
    | FetchResourceSucceed String (HttpBuilder.Response (List (Dict.Dict String String)))


getUrlByIndex : Model -> Int -> String
getUrlByIndex model index =
    let
        keys =
            Dict.keys model.schema |> Array.fromList

        url =
            Array.get index keys |> Maybe.withDefault ""
    in
        "#" ++ url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab tab ->
            ( model, getUrlByIndex model tab |> Navigation.newUrl )

        Mdl msg' ->
            Material.update msg' model

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

        FetchResourceSucceed name response ->
            case (Dict.get name model.schema) of
                Nothing ->
                    ( model, Cmd.none )

                Just pageModel ->
                    let
                        pageModel' =
                            { pageModel | dataList = response.data }

                        schema' =
                            Dict.insert name pageModel' model.schema

                        model' =
                            { model | schema = schema' }
                    in
                        ( model', Cmd.none )

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


type alias Mdl =
    Material.Model


getIndexByResourceName : Model -> String -> Int
getIndexByResourceName model resourceName =
    Dict.keys model.schema
        |> Array.fromList
        |> Array.toIndexedList
        |> List.filter (\( index, name ) -> name == resourceName)
        |> List.head
        |> Maybe.withDefault ( -1, "" )
        |> fst


getIndexByRoute : Model -> Int
getIndexByRoute model =
    case model.route of
        Routing.Index ->
            -1

        Routing.List name ->
            getIndexByResourceName model name

        Routing.Change name _ ->
            getIndexByResourceName model name

        Routing.Add name ->
            getIndexByResourceName model name


view model =
    let
        tabTitles =
            Dict.keys model.schema |> List.map text
    in
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            , Layout.selectedTab (getIndexByRoute model)
            , Layout.onSelectTab SelectTab
            ]
            { header = []
            , drawer = []
            , tabs = ( tabTitles, [] )
            , main = [ view' model ]
            }
            |> Material.Scheme.topWithScheme Color.Blue Color.LightBlue


view' : Model -> Html Msg
view' model =
    let
        getHeader name =
            a [ href ("#" ++ name ++ "/add") ] [ text "Add" ]

        ( header, subView ) =
            case model.route of
                Routing.Add name ->
                    ( getHeader name
                    , get_view_or_empy_div name
                        (Dict.get name model.schema)
                        (FormMsg name)
                        (\page -> Form.view page.form)
                    )

                Routing.List name ->
                    ( getHeader name
                    , get_view_or_empy_div name
                        (Dict.get name model.schema)
                        (ListMsg name)
                        (\page -> ItemList.view page.dataList)
                    )

                _ ->
                    ( div [] [], div [] [] )
    in
        div [] [ header, subView ]


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


getResource : String -> String -> Cmd Msg
getResource name url =
    getResourceTask url
        |> Task.perform FetchResourceFail (FetchResourceSucceed name)
