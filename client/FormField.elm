module FormField exposing
  ( FieldType
  , FieldInfo
  , fieldInfoDecoder
  )

import Json.Decode exposing (..)

type FieldType
  = Integer
  | DateTime
  | String

type alias FieldInfo =
  { fieldType : String
  , required: Bool
  , readOnly: Bool
  , label : String
  }


fieldInfoDecoder =
    object4 FieldInfo ("type" := string)
                      ("required" := bool)
                      ("read_only"  := bool)
                      ("label"     := string)

