{-# LANGUAGE ScopedTypeVariables #-}
module PureScript.Ide.Command where

import           Control.Monad
import           Data.Aeson
import           Data.Maybe
import           PureScript.Ide.Completion
import           PureScript.Ide.Matcher
import           PureScript.Ide.Types

data Command
    = Load { loadModules      :: [ModuleIdent]
           , loadDependencies :: [ModuleIdent]}
    | Type { typeSearch  :: DeclIdent
           , typeFilters :: [Filter]}
    | Complete { completeFilters :: [Filter]
               , completeMatcher :: Matcher}
    | List
    | Cwd
    | Quit

instance FromJSON Command where
  parseJSON = withObject "command" $ \o -> do
    (command :: String) <- o .: "command"
    case command of
      "list" -> return List
      "cwd"  -> return Cwd
      "quit" -> return Quit
      "load" -> do
        params <- o .: "params"
        mods <- params .:? "modules"
        deps <- params .:? "dependencies"
        return $ Load (fromMaybe [] mods) (fromMaybe [] deps)
      "type" -> do
        params <- o .: "params"
        search <- params .: "search"
        filters <- params .: "filters"
        return $ Type search filters
      "complete" -> do
        params <- o .: "params"
        filters <- params .:? "filters"
        matcher <- params .:? "matcher"
        return $ Complete (fromMaybe [] filters) (fromMaybe (Matcher id) matcher)
      _ -> mzero

instance FromJSON Filter where
  parseJSON = withObject "filter" $ \o -> do
    (filter' :: String) <- o .: "filter"
    case filter' of
      "exact" -> do
        params <- o .: "params"
        search <- params .: "search"
        return $ equalityFilter search
      "prefix" -> do
        params <- o.: "params"
        search <- params .: "search"
        return $ prefixFilter search
      "modules" -> do
        params <- o .: "params"
        modules <- params .: "modules"
        return $ moduleFilter modules
      "dependencies" -> do
        params <- o .: "params"
        (deps :: [ModuleIdent]) <- params .: "modules"
        return $ error "Dependency Filter not implemented"
      _ -> mzero

instance FromJSON Matcher where
  parseJSON = withObject "matcher" $ \o -> do
    (matcher :: Maybe String) <- o .:? "matcher"
    case matcher of
      Just "flex" -> do
        params <- o .: "params"
        search <- params .: "search"
        return $ flexMatcher search
      Just "helm" -> error "Helm matcher not implemented yet."
      Just "distance" -> error "Distance matcher not implemented yet."
      Just _ -> mzero
      Nothing -> return $ Matcher id
