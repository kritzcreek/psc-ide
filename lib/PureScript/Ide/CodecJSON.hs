module PureScript.Ide.CodecJSON where

import PureScript.Ide.Externs (ExternDecl(..))
import Data.Aeson

instance ToJSON ExternDecl where
  toJSON (FunctionDecl n t)        = object ["name" .= n, "type" .= t]
  toJSON (ModuleDecl   n t)        = object ["name" .= n, "type" .= t]
  toJSON (DataDecl     n t)        = object ["name" .= n, "type" .= t]
  toJSON (Dependency   n names)    = object ["module" .= n, "names" .= names]
  toJSON (FixityDeclaration f p n) = object ["name" .= n, "fixity" .= show f, "precedence" .= p]
