{-# LANGUAGE RecordWildCards #-}
module PureScript.Ide.Watcher where

import           Control.Concurrent          (threadDelay)
import           Control.Concurrent.STM
import           Control.Monad               (forever, when)
import qualified Data.Map                    as M
import           Data.Maybe                  (isJust)
import           Language.PureScript.Externs
import           PureScript.Ide.Externs
import           PureScript.Ide.State
import           PureScript.Ide.Types
import           System.FilePath
import           System.FSNotify


reloadFile :: TVar PscState -> FilePath -> IO ()
reloadFile stateVar fp = do
  (Right ef@ExternsFile{..}) <- readExternFile fp
  reloaded <- atomically $ do
    st <- readTVar stateVar
    if isLoaded efModuleName st
      then
        insertModule' stateVar ef *> pure True
      else
        pure False
  when reloaded $ putStrLn $ "Reloaded File at: " ++ fp
  where
    isLoaded name st = isJust (M.lookup name (externsFiles st))

watcher :: TVar PscState -> FilePath -> IO ()
watcher stateVar fp = withManager $ \mgr -> do
  _ <- watchTree mgr fp
    (\ev -> takeFileName (eventPath ev) == "externs.json")
    (reloadFile stateVar . eventPath)
  forever (threadDelay 10000)
