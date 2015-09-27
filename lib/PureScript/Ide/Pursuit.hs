{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PureScript.Ide.Pursuit where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Maybe
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Network.Wreq
import           PureScript.Ide.Externs (typeParse)
import           PureScript.Ide.Types

instance FromJSON PursuitResponse where
    parseJSON (Object o) = do
        package <- o .: "package"
        info <- o .: "info"
        (type' :: String) <- info .: "type"
        case type' of
            "module" -> do
                name <- info .: "module"
                return
                    ModuleResponse
                    { moduleResponseName = name
                    , moduleResponsePackage = package
                    }
            "declaration" -> do
                moduleName <- info .: "module"
                Right (ident,declType) <- typeParse <$> o .: "text"
                return
                    DeclarationResponse
                    { declarationResponseType = declType
                    , declarationResponseModule = moduleName
                    , declarationResponseIdent = ident
                    , declarationResponsePackage = package
                    }
            _ -> mzero
    parseJSON _ = mzero

queryUrl :: Text
queryUrl = "http://pursuit.purescript.org/search?q="

jsonOpts :: Options
jsonOpts = defaults & header "Accept" .~ ["application/json"]

queryPursuit q = getWith jsonOpts (T.unpack $ queryUrl <> q)

searchPursuitForDeclarations :: Text -> IO [PursuitResponse]
searchPursuitForDeclarations query = do
    r <- queryPursuit query
    let results = map fromJSON (r ^.. responseBody . values)
    return (mapMaybe isDeclarationResponse results)
  where
    isDeclarationResponse (Success a@(DeclarationResponse{})) = Just a
    isDeclarationResponse _ = Nothing

findPackagesForModuleIdent :: Text -> IO [PursuitResponse]
findPackagesForModuleIdent query = do
    r <- queryPursuit query
    let results = map fromJSON (r ^.. responseBody . values)
    return (mapMaybe isModuleResponse results)
  where
    isModuleResponse (Success a@(ModuleResponse{})) = Just a
    isModuleResponse _ = Nothing
