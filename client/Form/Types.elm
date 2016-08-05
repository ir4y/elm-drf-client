module Form.Types
    exposing
        ( FieldInfo
        , FormInfo
        , FormData
        , FormErrors
        , emptyFormData
        , emptyFormErrors
        )

import Dict
import List


type alias FormInfo =
    List ( String, FieldInfo )


type alias FormData =
    Dict.Dict String String


emptyFormData : FormData
emptyFormData =
    Dict.empty


type alias FormErrors =
    Dict.Dict String (List String)


emptyFormErrors : FormErrors
emptyFormErrors =
    Dict.empty


type alias FieldInfo =
    { fieldType : String
    , required : Bool
    , readOnly : Bool
    , label : String
    }
