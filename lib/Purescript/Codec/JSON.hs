module Purescript.Codec.JSON where

import Data.Aeson

instance ToJSON ExternDecl where
  toJSON (FunctionDecl n t)        = object ["name" .= n, "type" .= t]
  toJSON (ModuleDecl   n t)        = object ["name" .= n, "type" .= t]
  toJSON (DataDecl     n t)        = object ["name" .= n, "type" .= t]
  toJSON (Dependency   n names)    = object ["module" .= n, "names" .= names]
  toJSON (FixityDeclaration p i n) = object ["name" .= n, "fixity" .= show f, "precedence" .= p]
