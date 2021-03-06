module Form.Types
    exposing
        ( FieldInfo
        , FieldType(..)
        , FormInfo
        , FormData
        , FormErrors
        , FormDirtyState
        , Choice
        , emptyFormData
        , getFormValue
        , emptyFormErrors
        , getFormError
        , emptyFormDirtyState
        , isFieldDirty
        )

import Dict
import List
import Maybe


type alias FormInfo =
    List ( String, FieldInfo )


type alias FormData =
    Dict.Dict String String


emptyFormData : FormData
emptyFormData =
    Dict.empty


getFormValue : String -> FormData -> String
getFormValue name formData =
    Dict.get name formData |> Maybe.withDefault ""


type alias FormErrors =
    Dict.Dict String (List String)


emptyFormErrors : FormErrors
emptyFormErrors =
    Dict.empty


getFormError : String -> FormErrors -> List String
getFormError name formErrors =
    Dict.get name formErrors |> Maybe.withDefault []


type alias FormDirtyState =
    Dict.Dict String Bool


emptyFormDirtyState : FormDirtyState
emptyFormDirtyState =
    Dict.empty


isFieldDirty : String -> FormDirtyState -> Bool
isFieldDirty name dirtyState =
    Dict.get name dirtyState |> Maybe.withDefault False


type alias FieldInfo =
    { fieldType : FieldType
    , required : Bool
    , readOnly : Bool
    , label : String
    , choices : Maybe (List Choice)
    }


type FieldType
    = Integer
    | Datetime
    | String
    | Field


type alias Choice =
    { value : String
    , displayName : String
    }
