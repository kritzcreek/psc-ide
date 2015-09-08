{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module PureScript.Ide.Err
  (
    ErrMsg,
    Err (..),
    Ident,
    first,
    textErr,
    textResult,
    textResultNewFormat,
  ) where

import           Data.Monoid
import           Data.Text                (Text, pack, unpack)
import qualified Text.Parsec.Error        as P
import           PureScript.Ide.Externs   (ModuleIdent, DeclIdent)

type ErrMsg = String

data Err
    = GeneralErr ErrMsg
    | NotFound Ident
    | ModuleNotFound ModuleIdent
    | ParseErr P.ParseError ErrMsg
    deriving (Show, Eq)

type Ident = Text

textErr :: Err -> Text
textErr (GeneralErr msg)             = pack msg
textErr (NotFound ident)             = "Symbol '" <> ident <> "' not found."
textErr (ModuleNotFound ident)       = "Module '" <> ident <> "' not found."
textErr (ParseErr parseErr msg)      = pack $ msg <> ": " <> show (escape parseErr)
  where
    -- escape newlines and other special chars so we can send the error over the socket as a single line
    escape :: P.ParseError -> String
    escape = show

-- | This allows us to distinguish errors from success results in the output.
textResultNewFormat :: Either Err Text -> Text
textResultNewFormat (Right response) = pack $ "ok: "    <> unpack response
textResultNewFormat (Left err)       = pack "error: " <> textErr err

-- | Don't break compatibility for now
textResult :: Either Err Text -> Text
textResult (Right response) = response
textResult (Left err)       = textErr err

-- | Specialized version of `first` from `Data.Bifunctors`
first :: (a -> b) -> Either a r -> Either b r
first f (Left x)   = Left (f x)
first _ (Right r2) = Right r2
