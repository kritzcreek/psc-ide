module PureScript.Ide.Error
       (ErrorMsg, Error(..), textError)
       where

import           Data.Aeson
import           Data.Monoid
import           Data.Text              (Text, pack)
import           PureScript.Ide.Externs (ModuleIdent)
import qualified Text.Parsec.Error      as P

type ErrorMsg = String

data Error
    = GeneralError ErrorMsg
    | NotFound Text
    | ModuleNotFound ModuleIdent
    | ParseError P.ParseError ErrorMsg
    deriving (Show, Eq)

instance ToJSON Error where
  toJSON err = object
               [
                 "resultType" .= ("error" :: Text),
                 "result" .= textError err
               ]

textError :: Error -> Text
textError (GeneralError msg)           = pack msg
textError (NotFound ident)             = "Symbol '" <> ident <> "' not found."
textError (ModuleNotFound ident)       = "Module '" <> ident <> "' not found."
textError (ParseError parseError msg)  = pack $ msg <> ": " <> show (escape parseError)
    where
    -- escape newlines and other special chars so we can send the error over the socket as a single line
      escape :: P.ParseError -> String
      escape = show
