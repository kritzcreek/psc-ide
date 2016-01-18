{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports   #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}
module PureScript.Ide.State where

import           Control.Concurrent.STM
import           Control.Monad.Except
import           "monad-logger" Control.Monad.Logger
import           Control.Monad.Reader.Class
import qualified Data.Map.Lazy               as M
import           Data.Maybe                  (catMaybes)
import           Data.Monoid
import qualified Data.Text                   as T
import           Language.PureScript.Externs
import           Language.PureScript.Names
import           PureScript.Ide.Externs
import           PureScript.Ide.Reexports
import           PureScript.Ide.Types

getPscIdeState :: PscIde m =>
                  m (M.Map ModuleIdent [ExternDecl])
getPscIdeState = do
  stateVar <- envStateVar <$> ask
  liftIO $ pscStateModules <$> readTVarIO stateVar

getExternFiles :: (PscIde m) =>
                  m (M.Map ModuleName ExternsFile)
getExternFiles = do
  stateVar <- envStateVar <$> ask
  liftIO (externsFiles <$> readTVarIO stateVar)

getAllDecls :: PscIde m => m [ExternDecl]
getAllDecls = concat <$> getPscIdeState

getAllModules :: PscIde m => m [Module]
getAllModules = M.toList <$> getPscIdeState

getAllModulesWithReexports :: (PscIde m, MonadLogger m) =>
                              m [Module]
getAllModulesWithReexports = do
  mis <- M.keys <$> getPscIdeState
  ms  <- traverse getModuleWithReexports mis
  return (catMaybes ms)

getModule :: (PscIde m, MonadLogger m) =>
             ModuleIdent -> m (Maybe Module)
getModule m = do
  modules <- getPscIdeState
  return ((m,) <$> M.lookup m modules)

getModuleWithReexports :: (PscIde m, MonadLogger m) =>
                          ModuleIdent -> m (Maybe Module)
getModuleWithReexports mi = do
  m <- getModule mi
  modules <- getPscIdeState
  pure $ resolveReexports modules <$> m

insertModule ::(PscIde m, MonadLogger m) =>
               ExternsFile -> m ()
insertModule externsFile = do
  env <- ask
  let moduleName = efModuleName externsFile
  $(logDebug) $ "Inserting Module: " <> (T.pack (runModuleName moduleName))
  liftIO . atomically $ insertModule' (envStateVar env) externsFile

insertModule' :: TVar PscState -> ExternsFile -> STM ()
insertModule' st ef = do
    modifyTVar (st) $ \x ->
      x { externsFiles = M.insert (efModuleName ef) ef (externsFiles x)
          , pscStateModules = let (mn, decls ) = convertExterns ef
                              in M.insert mn decls (pscStateModules x)
        }
