module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Html.App as App
import Form
import Material
import Material.Color as Color
import Material.Scheme
import Material.Layout as Layout
import Material.Button as Button
import Material.Grid as Grid
import Material.Spinner as Loading
import Navigation
import Routing
import Navigation
import Dict
import Services exposing (getResourcesInfoTask, getResourceTask)
import HttpBuilder
import Maybe exposing (..)
import Task
import ItemList
import Array
import Types exposing (asMaybe)


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
    , dataList : ItemList.Model
    , form : Form.Model
    }


initPageModel : String -> ( PageModel, Cmd Form.Msg )
initPageModel url =
    let
        ( form, cmd ) =
            Form.init url
    in
        ( { url = url
          , dataList = Types.NotAsked
          , form = form
          }
        , cmd
        )


type alias Schema =
    Dict.Dict String PageModel


type alias Model =
    { schema : Types.RemoteData String Schema
    , route : Routing.Route
    , mdl : Material.Model
    , editForm : Form.Model
    }


init routeResult =
    ( { schema = Types.Loading
      , route = Routing.routeFromResult routeResult
      , mdl = Material.model
      , editForm = Form.initEmpty
      }
    , getQustionInfo
    )


urlUpdate : Result String Routing.Route -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    let
        currentRoute =
            Routing.routeFromResult result

        handleUrlWithParam name =
            (asMaybe model.schema)
                `andThen` (Dict.get name)
                |> map .url
                |> map (getResource name)
                |> withDefault gotoRoot

        cmd =
            case currentRoute of
                Routing.List name ->
                    handleUrlWithParam name

                Routing.Add name ->
                    handleUrlWithParam name

                _ ->
                    Cmd.none

        model' =
            case currentRoute of
                Routing.Change name id ->
                    (asMaybe model.schema)
                        `andThen` (Dict.get name)
                        `andThen`
                            (\pageModel ->
                                (asMaybe pageModel.dataList)
                                    |> map
                                        (\dataList ->
                                            { model
                                                | editForm =
                                                    Form.initEditForm
                                                        (pageModel.form.url ++ (toString id) ++ "/")
                                                        pageModel.form.formState
                                                        (dataList
                                                            |> List.filter
                                                                (\item ->
                                                                    (Dict.get "id" item) == (Maybe.Just (toString id))
                                                                )
                                                            |> List.head
                                                            |> Maybe.withDefault (Dict.insert "id" "none" Dict.empty)
                                                        )
                                            }
                                        )
                            )
                        |> withDefault model

                _ ->
                    model
    in
        ( { model' | route = currentRoute }, cmd )



-- UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | OpenAddPage String
    | SelectTab Int
    | FormMsg String Form.Msg
    | EditFormMsg Form.Msg
    | ListMsg String ItemList.Msg
    | FetchFail (HttpBuilder.Error String)
    | FetchSucceed (HttpBuilder.Response (Dict.Dict String String))
    | FetchResourceFail String (HttpBuilder.Error String)
    | FetchResourceSucceed String (HttpBuilder.Response ItemList.DataList)


getUrlByIndex : Schema -> Int -> String
getUrlByIndex schema index =
    let
        keys =
            Dict.keys schema |> Array.fromList

        url =
            Array.get index keys |> Maybe.withDefault ""
    in
        "#" ++ url


pageMap model name fn =
    (asMaybe model.schema)
        `andThen`
            (\schema ->
                (Dict.get name schema)
                    |> map
                        (\pageModel -> fn schema pageModel)
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenAddPage name ->
            ( model, Navigation.newUrl <| "#" ++ name ++ "/add" )

        SelectTab tab ->
            ( model
            , (asMaybe model.schema)
                |> map (\schema -> (getUrlByIndex schema tab) |> Navigation.newUrl)
                |> withDefault Cmd.none
            )

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

        EditFormMsg msg' ->
            let
                ( editForm, cmd ) =
                    Form.update msg' model.editForm
            in
                ( { model | editForm = editForm }, Cmd.map EditFormMsg cmd )

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

                dataLoadCmd =
                    (case model.route of
                        Routing.Add name ->
                            (Dict.get name schema) `andThen` (\_ -> Just Cmd.none)

                        Routing.List name ->
                            (Dict.get name schema)
                                |> map .url
                                |> map (getResource name)

                        _ ->
                            Just Cmd.none
                    )
                        |> withDefault gotoRoot

                cmds =
                    Dict.map (\key value -> Cmd.map (FormMsg key) (snd value)) initSchema
                        |> Dict.values
                        |> List.append [ dataLoadCmd ]
                        |> Cmd.batch
            in
                ( { model | schema = Types.Success schema }, cmds )

        FetchFail error ->
            ( { model
                | schema =
                    error
                        |> toString
                        |> Types.Failure
              }
            , Cmd.none
            )

        FetchResourceSucceed name response ->
            ( pageMap model
                name
                (\schema pageModel ->
                    let
                        pageModel' =
                            { pageModel | dataList = Types.Success response.data }

                        schema' =
                            Dict.insert name pageModel' schema

                        model' =
                            { model | schema = Types.Success schema' }
                    in
                        model'
                )
                |> withDefault model
            , Cmd.none
            )

        FetchResourceFail name error ->
            ( pageMap model
                name
                (\schema pageModel ->
                    let
                        pageModel' =
                            { pageModel | dataList = Types.Failure (toString error) }

                        schema' =
                            Dict.insert name pageModel' schema

                        model' =
                            { model | schema = Types.Success schema' }
                    in
                        model'
                )
                |> withDefault model
            , Cmd.none
            )


apply_update_or_noting model name msg' update getSubState setSubState msgWrap =
    pageMap model
        name
        (\schema pageModel ->
            let
                ( newSubState, cmd' ) =
                    update msg' (getSubState pageModel)

                pageModel' =
                    setSubState pageModel newSubState

                schema' =
                    Dict.insert name pageModel' schema
            in
                ( { model | schema = Types.Success schema' }, cmd' |> Cmd.map msgWrap )
        )
        |> withDefault ( model, Cmd.none )



-- VIEW


type alias Mdl =
    Material.Model


getIndexByResourceName : Schema -> String -> Int
getIndexByResourceName schema resourceName =
    Dict.keys schema
        |> Array.fromList
        |> Array.toIndexedList
        |> List.filter (\( index, name ) -> name == resourceName)
        |> List.head
        |> Maybe.withDefault ( -1, "" )
        |> fst


getIndexByRoute : Routing.Route -> Schema -> Int
getIndexByRoute route schema =
    case route of
        Routing.Index ->
            -1

        Routing.List name ->
            getIndexByResourceName schema name

        Routing.Change name _ ->
            getIndexByResourceName schema name

        Routing.Add name ->
            getIndexByResourceName schema name


view model =
    Material.Scheme.topWithScheme
        Color.Blue
        Color.LightBlue
        (case model.schema of
            Types.Success schema ->
                let
                    tabTitles =
                        Dict.keys schema |> List.map text
                in
                    Layout.render Mdl
                        model.mdl
                        [ Layout.fixedHeader
                        , Layout.selectedTab (getIndexByRoute model.route schema)
                        , Layout.onSelectTab SelectTab
                        ]
                        { header = []
                        , drawer = []
                        , tabs = ( tabTitles, [] )
                        , main = [ view' schema model ]
                        }

            Types.Failure error ->
                div [] [ text error ]

            Types.Loading ->
                div [] [ Loading.spinner [ Loading.active True ] ]

            Types.NotAsked ->
                div [] [ text "not asked yet" ]
        )


view' : Schema -> Model -> Html Msg
view' schema model =
    let
        getHeader name =
            Button.render Mdl
                [ 0 ]
                model.mdl
                [ Button.raised
                , Button.onClick (OpenAddPage name)
                ]
                [ text <| "Add " ++ name ]

        ( header, subView ) =
            case model.route of
                Routing.Add name ->
                    ( getHeader name
                    , get_view_or_empy_div name
                        (Dict.get name schema)
                        (FormMsg name)
                        (\page -> Form.view page.form)
                    )

                Routing.List name ->
                    ( getHeader name
                    , get_view_or_empy_div name
                        (Dict.get name schema)
                        (ListMsg name)
                        (\page -> ItemList.view name page.dataList)
                    )

                Routing.Change name id ->
                    ( div [] []
                    , get_view_or_empy_div name
                        (Dict.get name schema)
                        EditFormMsg
                        (\page -> Form.view model.editForm)
                    )

                _ ->
                    ( div [] [], div [] [] )
    in
        div []
            [ Grid.grid []
                [ Grid.cell [ Grid.size Grid.All 12 ] [ header ] ]
            , Grid.grid []
                [ Grid.cell [ Grid.size Grid.All 12 ] [ subView ] ]
            ]


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



-- COMMAND HELPERS


gotoRoot : Cmd Msg
gotoRoot =
    Navigation.newUrl "#"


getQustionInfo : Cmd Msg
getQustionInfo =
    getResourcesInfoTask "http://localhost:8000/poll/"
        |> Task.perform FetchFail FetchSucceed


getResource : String -> String -> Cmd Msg
getResource name url =
    getResourceTask url
        |> Task.perform (FetchResourceFail name) (FetchResourceSucceed name)
