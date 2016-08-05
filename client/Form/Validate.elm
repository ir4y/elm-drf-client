module Form.Validate exposing (..)

import Form.Types as FormTypes
import Validate
import List
import Dict


buildValidator : ( String, FormTypes.FieldInfo ) -> Validate.Validator ( String, String ) FormTypes.FormData
buildValidator ( name, fieldInfo ) =
    (FormTypes.getFormValue name)
        >> if fieldInfo.required then
            (Validate.ifBlank ( name, "The filed is required" ))
           else
            (Validate.ifInvalid (\_ -> False) ( name, "newer show" ))


getValidator : FormTypes.FormInfo -> (FormTypes.FormData -> FormTypes.FormErrors)
getValidator formInfo data =
    (List.map buildValidator formInfo
        |> Validate.all
    )
        data
        |> Dict.fromList
        |> Dict.map (\k v -> [ v ])
