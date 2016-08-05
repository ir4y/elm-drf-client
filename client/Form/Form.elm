module Form.Form
    exposing
        ( Model
        , Msg
        , cleanDirtyState
        , cleanup
        , init
        , update
        , view
        )

import Html exposing (..)
import Form.Types as FormTypes
import Form.Validate as Validate
import Material
import Material.Table as Table
import Material.Textfield as Textfield
import Material.Button as Button exposing (..)
import Dict


type alias Model =
    { mdl : Material.Model
    , formData : FormTypes.FormData
    , formErrors : FormTypes.FormErrors
    , formDirtyState : FormTypes.FormDirtyState
    }


cleanDirtyState model =
    { model
        | formDirtyState = FormTypes.emptyFormDirtyState
    }


cleanup model =
    { model
        | formData = FormTypes.emptyFormData
        , formErrors = FormTypes.emptyFormErrors
        , formDirtyState = FormTypes.emptyFormDirtyState
    }


init : Model
init =
    { mdl = Material.model
    , formData = FormTypes.emptyFormData
    , formErrors = FormTypes.emptyFormErrors
    , formDirtyState = FormTypes.emptyFormDirtyState
    }


type Msg
    = MDL (Material.Msg Msg)
    | UserInput String String


update : (FormTypes.FormData -> FormTypes.FormErrors) -> Msg -> Model -> Model
update validate msg model =
    case msg of
        MDL action' ->
            Material.update action' model |> fst

        UserInput key data ->
            let
                formData =
                    Dict.insert key data model.formData

                formErrors =
                    validate formData

                formDirtyState =
                    Dict.insert key True model.formDirtyState
            in
                { model
                    | formData = formData
                    , formErrors = formErrors
                    , formDirtyState = formDirtyState
                }


tableItemView : FormTypes.FormErrors -> Model -> ( String, FormTypes.FieldInfo ) -> Int -> Html Msg
tableItemView formErrors model ( name, fieldInfo ) index =
    Table.tr []
        [ Table.td []
            [ text
                (fieldInfo.label
                    ++ if FormTypes.isFieldDirty name model.formDirtyState then
                        "*"
                       else
                        ""
                )
            ]
        , Table.td []
            [ Textfield.render MDL
                [ index ]
                model.mdl
                (List.concat
                    [ [ Textfield.label fieldInfo.label
                      , Textfield.value (FormTypes.getFormValue name model.formData)
                      , Textfield.onInput (UserInput name)
                      ]
                    , if fieldInfo.readOnly then
                        [ Textfield.disabled ]
                      else
                        []
                    ]
                )
            ]
        , Table.td []
            (List.map text
                (if FormTypes.isFieldDirty name model.formDirtyState then
                    FormTypes.getFormError name model.formErrors
                 else
                    FormTypes.getFormError name formErrors
                )
            )
        ]


view : FormTypes.FormInfo -> FormTypes.FormErrors -> Model -> Html Msg
view formInfo formErrors model =
    div []
        [ h2 [] [ text "Question model information" ]
        , Table.table [] (List.map2 (tableItemView formErrors model) formInfo [1..List.length formInfo])
        ]
