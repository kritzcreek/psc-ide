{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PureScript.Ide.Pursuit where

import qualified Control.Exception      as E
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Network.HTTP.Client    (HttpException (StatusCodeException))
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

queryUrl :: String
queryUrl = "http://pursuit.purescript.org/search"

jsonOpts :: Text -> Options
jsonOpts q =
    defaults & header "Accept" .~ ["application/json"] & param "q" .~ [q]

-- We need to remove trailing dots because Pursuit will return a 400 otherwise
-- TODO: remove this when the issue is fixed at Pursuit
queryPursuit q = getWith (jsonOpts (T.dropWhileEnd (== '.') q)) queryUrl

handler :: HttpException -> IO [a]
handler (StatusCodeException{}) = return []
handler _ = return []

searchPursuitForDeclarations :: Text -> IO [PursuitResponse]
searchPursuitForDeclarations query =
    (do r <- queryPursuit query
        let results = map fromJSON (r ^.. responseBody . values)
        return (mapMaybe isDeclarationResponse results)) `E.catch`
    handler
  where
    isDeclarationResponse (Success a@(DeclarationResponse{})) = Just a
    isDeclarationResponse _ = Nothing

findPackagesForModuleIdent :: Text -> IO [PursuitResponse]
findPackagesForModuleIdent query =
    (do r <- queryPursuit query
        let results = map fromJSON (r ^.. responseBody . values)
        return (mapMaybe isModuleResponse results)) `E.catch`
    handler
  where
    isModuleResponse (Success a@(ModuleResponse{})) = Just a
    isModuleResponse _ = Nothing
