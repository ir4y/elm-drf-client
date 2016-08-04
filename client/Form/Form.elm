module Form.Form
    exposing
        ( Model
        , Msg
        , setFormErrors
        , cleanup
        , init
        , update
        , view
        )

import Html exposing (..)
import Form.Data as FormData
import Material
import Material.Table as Table
import Material.Textfield as Textfield
import Material.Button as Button exposing (..)
import Dict
import List


type alias Model =
    { mdl : Material.Model
    , formData : FormData.FormData
    , formErrors : FormData.FormErrors
    }


setFormErrors model formErrors =
    { model | formErrors = formErrors }


cleanup model =
    { model | formData = Dict.empty, formErrors = Dict.empty }


init : Model
init =
    { mdl = Material.model
    , formData = Dict.empty
    , formErrors = Dict.empty
    }


type Msg
    = MDL (Material.Msg Msg)
    | UserInput String String


update : Msg -> Model -> Model
update msg model =
    case msg of
        MDL action' ->
            Material.update action' model |> fst

        UserInput key data ->
            { model | formData = Dict.insert key data model.formData }


tableItemView : Model -> ( String, FormData.FieldInfo ) -> Int -> Html Msg
tableItemView model ( name, fieldInfo ) index =
    Table.tr []
        [ Table.td [] [ text fieldInfo.label ]
        , Table.td []
            [ Textfield.render MDL
                [ index ]
                model.mdl
                (List.concat
                    [ [ Textfield.label fieldInfo.label
                      , Textfield.value (Maybe.withDefault "" (Dict.get name model.formData))
                      , Textfield.onInput (UserInput name)
                      ]
                    , if fieldInfo.readOnly then
                        [ Textfield.disabled ]
                      else
                        []
                    ]
                )
            ]
        , Table.td [] (List.map text (Maybe.withDefault [] (Dict.get name model.formErrors)))
        ]


view : FormData.FormInfo -> Model -> Html Msg
view formInfo model =
    div []
        [ h2 [] [ text "Question model information" ]
        , Table.table [] (List.map2 (tableItemView model) formInfo [1..List.length formInfo])
        ]
