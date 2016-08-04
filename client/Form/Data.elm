module Form.Data exposing
  ( FieldInfo
  , FormInfo
  , FormData
  , FormErrors
  , fieldInfoDecoder
  , formDataEncoder
  )

import Json.Decode exposing (..)
import Json.Encode as Encode
import Dict

type alias FormInfo = List (String, FieldInfo)
type alias FormData =  Dict.Dict String String
type alias FormErrors = Dict.Dict String (List String)


type alias FieldInfo =
  { fieldType : String
  , required: Bool
  , readOnly: Bool
  , label : String
  }

fieldInfoDecoder : Decoder FieldInfo
fieldInfoDecoder =
  object4 FieldInfo ("type"      := string)
                    ("required"  := bool)
                    ("read_only" := bool)
                    ("label"     := string)

wrapValues : (String, String) -> (String, Encode.Value)
wrapValues (k,v) =
  (k, Encode.string v)

formDataEncoder : FormData -> Encode.Value
formDataEncoder data =
  List.map wrapValues (Dict.toList data)
  |> Encode.object
