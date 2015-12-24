module PureScript.Ide.Watcher where

import           Control.Concurrent     (threadDelay)
import           Control.Concurrent.STM
import           Control.Monad          (forever, when)
import qualified Data.Map               as M
import           Data.Maybe             (isJust)
import           PureScript.Ide.Externs
import           PureScript.Ide.Types
import           System.FilePath
import           System.FSNotify


reloadFile :: TVar PscState -> FilePath -> IO ()
reloadFile stateVar fp = do
  Right (name, decls) <- readExternFile fp
  reloaded <- atomically $ do
    st <- readTVar stateVar
    if isLoaded name st
      then
        loadModule name decls *> pure True
      else
        pure False
  when reloaded $ putStrLn $ "Reloaded File at: " ++ fp
  where
    isLoaded name st = isJust (M.lookup name (pscStateModules st))
    loadModule name decls = modifyTVar stateVar $ \x ->
          x { pscStateModules = M.insert name decls (pscStateModules x)}

watcher :: TVar PscState -> FilePath -> IO ()
watcher stateVar fp = withManager $ \mgr -> do
  _ <- watchTree mgr fp
    (\ev -> takeFileName (eventPath ev) == "externs.json")
    (reloadFile stateVar . eventPath)
  forever (threadDelay 10000)
