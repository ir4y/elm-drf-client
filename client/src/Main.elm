module Main exposing (..)

import Form
import ItemList
import Routing
import Services exposing (getResourcesInfoTask, getResourceTask)
import Types exposing (asMaybe)
import Array
import Http
import Dict
import Html exposing (..)
import Material
import Material.Button as Button
import Material.Color as Color
import Material.Grid as Grid
import Material.Layout as Layout
import Material.Scheme
import Material.Spinner as Loading
import Material.Options as Options
import Maybe exposing (map, andThen, withDefault)
import Navigation


handleRoute : Navigation.Location -> Msg
handleRoute location =
    Routing.hashParser location
        |> Result.map ChangeRoute
        |> Result.withDefault NoOp


main : Program Never Model Msg
main =
    Navigation.program
        handleRoute
        { init = init
        , view = view
        , update = update
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


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( { schema = Types.Loading
      , route = Routing.routeFromResult (Routing.hashParser location)
      , mdl = Material.model
      , editForm = Form.initEmpty
      }
    , getQustionInfo
    )



-- UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | NoOp
    | ChangeRoute Routing.Route
    | OpenAddPage String
    | SelectTab Int
    | FormMsg String Form.Msg
    | EditFormMsg Form.Msg
    | ListMsg String ItemList.Msg
    | FetchFail Http.Error
    | FetchSucceed (Dict.Dict String String)
    | FetchResourceFail String Http.Error
    | FetchResourceSucceed String ItemList.DataList


getUrlByIndex : Schema -> Int -> String
getUrlByIndex schema index =
    let
        keys =
            Dict.keys schema |> Array.fromList

        url =
            Array.get index keys |> Maybe.withDefault ""
    in
        "#" ++ url


pageMap : Model -> String -> (Schema -> PageModel -> a) -> Maybe a
pageMap model name fn =
    (asMaybe model.schema)
        |> andThen
            (\schema ->
                (Dict.get name schema)
                    |> Maybe.map
                        (\pageModel -> fn schema pageModel)
            )


updateFromRoute : Model -> Routing.Route -> ( Model, Cmd Msg )
updateFromRoute model route =
    let
        model_ =
            { model | route = route }
    in
        case route of
            Routing.List name ->
                let
                    schema =
                        Types.asMaybe model.schema

                    cmd =
                        schema
                            |> Maybe.andThen (Dict.get name)
                            |> Maybe.map .url
                            |> Maybe.map (getResource name)
                            |> Maybe.withDefault Cmd.none
                in
                    ( model_, cmd )

            Routing.Change name id ->
                let
                    schema =
                        Types.asMaybe model.schema

                    pageModel =
                        schema
                            |> Maybe.andThen (Dict.get name)

                    dataList =
                        Types.asMaybe
                            (pageModel
                                |> Maybe.map .dataList
                                |> Maybe.withDefault Types.NotAsked
                            )
                            |> Maybe.withDefault []

                    item =
                        dataList
                            |> List.filter
                                (\item ->
                                    (Dict.get "id" item) == (Maybe.Just <| toString id)
                                )
                            |> List.head
                            |> Maybe.withDefault (Dict.insert "id" "none" Dict.empty)

                    model__ =
                        case pageModel of
                            Just pageModel ->
                                { model_
                                    | editForm =
                                        Form.initEditForm
                                            (pageModel.form.url ++ (toString id) ++ "/")
                                            pageModel.form.formState
                                            item
                                }

                            Nothing ->
                                model_
                in
                    ( model__, Cmd.none )

            _ ->
                ( model_, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeRoute route ->
            updateFromRoute model route

        OpenAddPage name ->
            ( model, Navigation.newUrl <| "#" ++ name ++ "/add" )

        SelectTab tab ->
            ( model
            , (asMaybe model.schema)
                |> Maybe.map (\schema -> (getUrlByIndex schema tab) |> Navigation.newUrl)
                |> withDefault Cmd.none
            )

        Mdl msg_ ->
            Material.update (\_ -> NoOp) msg_ model

        FormMsg name msg_ ->
            let
                ( model_, cmd ) =
                    apply_update_or_nothing model
                        name
                        msg_
                        Form.update
                        .form
                        (\pageModel form -> { pageModel | form = form })
                        (FormMsg name)
            in
                ( model_
                , Cmd.batch
                    [ cmd
                    , case msg_ of
                        Form.UploadSucceed ->
                            (changeRouteAfterFormSucceed model_.route)

                        _ ->
                            Cmd.none
                    ]
                )

        EditFormMsg msg_ ->
            let
                ( editForm, cmd ) =
                    Form.update msg_ model.editForm
            in
                ( { model | editForm = editForm }
                , Cmd.batch
                    [ Cmd.map EditFormMsg cmd
                    , case msg_ of
                        Form.UploadSucceed ->
                            (changeRouteAfterFormSucceed model.route)

                        _ ->
                            Cmd.none
                    ]
                )

        ListMsg name msg_ ->
            apply_update_or_nothing model
                name
                msg_
                ItemList.update
                .dataList
                (\pageModel dataList -> { pageModel | dataList = dataList })
                (ListMsg name)

        FetchSucceed response ->
            let
                initSchema =
                    Dict.map (\key value -> initPageModel value) response

                schema =
                    Dict.map (\key value -> Tuple.first value) initSchema

                dataLoadCmd =
                    (case model.route of
                        Routing.Add name ->
                            (Dict.get name schema) |> andThen (\_ -> Just Cmd.none)

                        Routing.List name ->
                            (Dict.get name schema)
                                |> Maybe.map .url
                                |> Maybe.map (getResource name)

                        _ ->
                            Just Cmd.none
                    )
                        |> withDefault gotoRoot

                cmds =
                    Dict.map (\key value -> Cmd.map (FormMsg key) (Tuple.second value)) initSchema
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
                        pageModel_ =
                            { pageModel | dataList = Types.Success response }

                        schema_ =
                            Dict.insert name pageModel_ schema

                        model_ =
                            { model | schema = Types.Success schema_ }
                    in
                        model_
                )
                |> withDefault model
            , Cmd.none
            )

        FetchResourceFail name error ->
            ( pageMap model
                name
                (\schema pageModel ->
                    let
                        pageModel_ =
                            { pageModel | dataList = Types.Failure (toString error) }

                        schema_ =
                            Dict.insert name pageModel_ schema

                        model_ =
                            { model | schema = Types.Success schema_ }
                    in
                        model_
                )
                |> withDefault model
            , Cmd.none
            )


apply_update_or_nothing :
    Model
    -> String
    -> msg
    -> (msg -> m -> ( m, Cmd cmd ))
    -> (PageModel -> m)
    -> (PageModel -> m -> PageModel)
    -> (cmd -> Msg)
    -> ( Model, Cmd Msg )
apply_update_or_nothing model name msg_ update getSubState setSubState msgWrap =
    pageMap model
        name
        (\schema pageModel ->
            let
                ( newSubState, cmd_ ) =
                    update msg_ (getSubState pageModel)

                pageModel_ =
                    setSubState pageModel newSubState

                schema_ =
                    Dict.insert name pageModel_ schema
            in
                ( { model | schema = Types.Success schema_ }, cmd_ |> Cmd.map msgWrap )
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
        |> Tuple.first


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


view : Model -> Html Msg
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
                        , main = [ view_ schema model ]
                        }

            Types.Failure error ->
                div [] [ text error ]

            Types.Loading ->
                div [] [ Loading.spinner [ Loading.active True ] ]

            Types.NotAsked ->
                div [] [ text "not asked yet" ]
        )


view_ : Schema -> Model -> Html Msg
view_ schema model =
    let
        getHeader name =
            Button.render Mdl
                [ 0 ]
                model.mdl
                [ Button.raised
                , Options.onClick (OpenAddPage name)
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
            (Html.map msgWrap (view page))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- COMMAND HELPERS


gotoRoot : Cmd Msg
gotoRoot =
    Navigation.newUrl "#"


changeRouteAfterFormSucceed : Routing.Route -> Cmd Msg
changeRouteAfterFormSucceed route =
    case route of
        Routing.Add name ->
            Navigation.newUrl ("#" ++ name)

        Routing.Change name _ ->
            Navigation.newUrl ("#" ++ name)

        _ ->
            Cmd.none


getQustionInfo : Cmd Msg
getQustionInfo =
    getResourcesInfoTask "http://localhost:8000/poll/"
        (\result ->
            case result of
                Result.Ok data ->
                    FetchSucceed data

                Result.Err error ->
                    FetchFail error
        )


getResource : String -> String -> Cmd Msg
getResource name url =
    getResourceTask url
        (\result ->
            case result of
                Result.Ok data ->
                    FetchResourceSucceed name data

                Result.Err error ->
                    FetchResourceFail name error
        )
