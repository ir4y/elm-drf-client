module Main exposing (..)

import Html exposing (Html, div, h2, a, text)
import Html.Attributes exposing (href)
import Html.App as App
import Form as Form
import Material.Scheme
import Navigation
import Routing
import Dict
import Services exposing (getResourcesInfoTask)
import HttpBuilder
import Maybe
import Task
import Debug


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
    | FetchFail (HttpBuilder.Error String)
    | FetchSucceed (HttpBuilder.Response (Dict.Dict String String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormMsg name msg' ->
            let
                maybePageModel =
                    Dict.get name model.schema

                result =
                    case maybePageModel of
                        Nothing ->
                            ( model, Cmd.none )

                        Just pageModel ->
                            let
                                ( form', cmd' ) =
                                    Form.update msg' pageModel.form

                                pageModel' =
                                    { pageModel | form = form' }

                                schema =
                                    Dict.insert name pageModel' model.schema
                            in
                                ( { model | schema = schema }, cmd' |> Cmd.map (FormMsg name) )
            in
                result

        FetchSucceed response ->
            ( { model | schema = Dict.map (\key value -> fst (initPageModel value)) response.data }, Cmd.none )

        _ ->
            ( model, Cmd.none )



--update msg model =
--case msg of
--QuestionFormMsg msg' ->
--let
--action =
--Form.update msg' model.questionForm
--in
--( { model | questionForm = fst action }, snd action |> Cmd.map QuestionFormMsg )
--AnswerFormMsg msg' ->
--let
--action =
--Form.update msg' model.answerForm
--in
--( { model | answerForm = fst action }, snd action |> Cmd.map AnswerFormMsg )
-- VIEW


view : Model -> Html Msg
view model =
    let
        header =
            div []
                (List.map
                    (\path -> a [ href ("#" ++ path) ] [ text path ])
                    (Dict.keys model.schema)
                )

        formView =
            case Debug.log "route" model.route of
                Routing.List name ->
                    let
                        maybePageModel =
                            Debug.log "page" (Dict.get name model.schema)
                    in
                        case maybePageModel of
                            Nothing ->
                                div [] []

                            Just page ->
                                App.map (FormMsg name) (Form.view page.form)

                _ ->
                    div [] []
    in
        div [] [ header, formView ] |> Material.Scheme.top



--let
--header =
--div []
--[ a [ href "#questions" ] [ text "Questions" ]
--, a [ href "#answers" ] [ text "Answers" ]
--]
--in
--(case model.route of
--Routing.List "questions" ->
--div []
--[ header
--, h2 [] [ text "Questions" ]
--, App.map QuestionFormMsg (Form.view model.questionForm)
--]
--Routing.List "answers" ->
--div []
--[ header
--, h2 [] [ text "Answers" ]
--, App.map AnswerFormMsg (Form.view model.answerForm)
--]
--_ ->
--header
--)
--|> Material.Scheme.top
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


getQustionInfo : Cmd Msg
getQustionInfo =
    getResourcesInfoTask "http://localhost:8000/poll/"
        |> Task.perform FetchFail FetchSucceed
