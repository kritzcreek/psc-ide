module PureScript.Ide.CodecJSON where

import PureScript.Ide.Externs (ExternDecl(..))
import Data.Aeson
import Data.Text (Text())
import Data.Text.Lazy (toStrict, fromStrict)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)

instance ToJSON ExternDecl where
  toJSON (FunctionDecl n t)        = object ["name" .= n, "type" .= t]
  toJSON (ModuleDecl   n t)        = object ["name" .= n, "type" .= t]
  toJSON (DataDecl     n t)        = object ["name" .= n, "type" .= t]
  toJSON (Dependency   n names)    = object ["module" .= n, "names" .= names]
  toJSON (FixityDeclaration f p n) = object ["name" .= n, "fixity" .= show f, "precedence" .= p]

encodeT :: (ToJSON a) => a -> Text
encodeT = toStrict . decodeUtf8 . encode

decodeT :: (FromJSON a) => Text -> Maybe a
decodeT = decode . encodeUtf8 . fromStrict

