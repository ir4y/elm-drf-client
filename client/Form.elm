module Form exposing
  ( FieldType
  , FieldInfo
  , fieldInfoDecoder
  , FormInfo
  , FormData
  , FormErrors
  )

import Json.Decode exposing (..)
import Dict

type FieldType
  = Integer
  | DateTime
  | String

type alias FormInfo = List (String, FieldInfo)
type alias FormData =  Dict.Dict String String
type alias FormErrors = Dict.Dict String (List String)


type alias FieldInfo =
  { fieldType : String
  , required: Bool
  , readOnly: Bool
  , label : String
  }


fieldInfoDecoder =
    object4 FieldInfo ("type"      := string)
                      ("required"  := bool)
                      ("read_only" := bool)
                      ("label"     := string)
