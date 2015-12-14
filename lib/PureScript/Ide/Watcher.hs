module PureScript.Ide.Watcher where

import           Control.Concurrent     (threadDelay)
import           Control.Concurrent.STM
import           Control.Monad          (forever, unless, when)
import qualified Data.Map               as M
import           Data.Maybe             (isJust)
import           PureScript.Ide.Externs
import           PureScript.Ide.Types
import           System.FilePath
import           System.FSNotify

modifyTVarIO a b = atomically $ modifyTVar a b

reloadFile :: TVar PscState -> FilePath -> IO ()
reloadFile stateVar fp = do
  Right (name, decls) <- readExternFile fp
  reloaded <- atomically $ do
    st <- readTVar stateVar
    if isLoaded name st
      then do
        modifyTVar stateVar $ \x ->
          x { pscStateModules = M.insert name decls (pscStateModules x)}
        return True
      else
        return False
  when reloaded $ putStrLn $ "Reloaded File at: " ++ fp
  where
    isLoaded name st = isJust (M.lookup name (pscStateModules st))

watcher :: TVar PscState -> FilePath -> IO ()
watcher stateVar fp = putStrLn fp >>= \_ -> withManager $ \mgr -> do
  _ <- watchTree mgr fp
    (\ev -> "externs.json" == takeFileName (eventPath ev))
    (reloadFile stateVar . eventPath)
  forever (threadDelay 10000)
