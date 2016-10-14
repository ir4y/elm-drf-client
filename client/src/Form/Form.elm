module Form.Form
    exposing
        ( Model
        , Msg
        , cleanDirtyState
        , cleanup
        , init
        , initWithData
        , update
        , view
        )

import Html exposing (Html, text, select, option)
import Html.Attributes exposing (value, selected)
import Html.Events exposing (onInput)
import Form.Types as FormTypes
import Form.Validate as Validate
import Material
import Material.Table as Table
import Material.Textfield as Textfield
import Material.Button as Button exposing (..)
import Dict
import Maybe


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


initWithData : FormTypes.FormData -> Model
initWithData formData =
    { mdl = Material.model
    , formData = formData
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


buildOptionParam : FormTypes.Choice -> String -> List (Html.Attribute b)
buildOptionParam o selectedOptionId =
    if o.value == selectedOptionId then
        [ value o.value
        , selected True
        ]
    else
        [ value o.value ]


buildOptions : FormTypes.FieldInfo -> String -> List (Html Msg)
buildOptions fieldInfo selected =
    fieldInfo.choices
        |> Maybe.withDefault []
        |> List.map (\o -> option (buildOptionParam o selected) [ text o.displayName ])


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
            [ case fieldInfo.fieldType of
                FormTypes.Field ->
                    select [ onInput (UserInput name) ]
                        (buildOptions fieldInfo (FormTypes.getFormValue name model.formData))

                _ ->
                    Textfield.render MDL
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
    Table.table [] (List.map2 (tableItemView formErrors model) formInfo [1..List.length formInfo])
