{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Concurrent       (forkFinally)
import           Control.Concurrent.STM
import           Control.Exception        (bracketOnError)
import           Control.Monad
import           "monad-logger" Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Data.Version             (showVersion)
import           Network                  hiding (socketPort)
import           Network.BSD              (getProtocolNumber)
import           Network.Socket           hiding (PortNumber, Type, accept,
                                           sClose)
import           Options.Applicative
import           PureScript.Ide
import           PureScript.Ide.CodecJSON
import           PureScript.Ide.Command
import           PureScript.Ide.Error
import           PureScript.Ide.Types
import           PureScript.Ide.Watcher
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO

import qualified Paths_psc_ide            as Paths

-- Copied from upstream impl of listenOn
-- bound to localhost interface instead of iNADDR_ANY
listenOnLocalhost :: PortID -> IO Socket
listenOnLocalhost (PortNumber port) = do
    proto <- getProtocolNumber "tcp"
    localhost <- inet_addr "127.0.0.1"
    bracketOnError
        (socket AF_INET Stream proto)
        sClose
        (\sock ->
              do setSocketOption sock ReuseAddr 1
                 bindSocket sock (SockAddrInet port localhost)
                 listen sock maxListenQueue
                 return sock)
listenOnLocalhost _ = error "Wrong Porttype"

data Options = Options
    { optionsDirectory :: Maybe FilePath
    , optionsPort      :: Maybe Int
    , optionsDebug     :: Bool
    }

main :: IO ()
main = do
    Options dir port debug <- execParser opts
    maybe (return ()) setCurrentDirectory dir
    serverState <- newTVarIO emptyPscState
    cwd <- getCurrentDirectory
    _ <- forkFinally (watcher serverState (cwd </> "output")) print
    startServer (PortNumber . fromIntegral $ fromMaybe 4242 port) debug serverState
  where
    parser =
        Options <$>
          optional (strOption (long "directory" <> short 'd')) <*>
          optional (option auto (long "port" <> short 'p')) <*>
          switch (long "debug")
    opts = info (version <*> parser) mempty
    version = abortOption (InfoMsg (showVersion Paths.version)) $ long "version" <> help "Show the version number" <> hidden

startServer :: PortID -> Bool -> TVar PscState -> IO ()
startServer port debug st_in =
    withSocketsDo $
    do sock <- listenOnLocalhost port
       runReaderT (runStdoutLoggingT $ forever (loop sock)) st_in
  where
    acceptCommand :: (MonadIO m, MonadLogger m) => Socket -> m (T.Text, Handle)
    acceptCommand sock = do
        (h,_,_) <- liftIO $ accept sock
        $(logDebug) "Accepted a connection"
        liftIO $ do
          hSetEncoding h utf8
          hSetBuffering h LineBuffering
          cmd <- T.hGetLine h
          when debug (T.putStrLn cmd)
          return (cmd, h)
    loop :: (PscIde m, MonadLogger m) =>Socket -> m ()
    loop sock = do
        (cmd,h) <- acceptCommand sock
        case decodeT cmd of
            Just cmd' -> do
                result <- handleCommand cmd'
                when debug $ liftIO $ T.putStrLn ("Answer was: " <> (T.pack . show $ result)) >> hFlush stdout
                case result of
                  -- What function can I use to clean this up?
                  Right r  -> liftIO $ T.hPutStrLn h (encodeT r)
                  Left err -> liftIO $ T.hPutStrLn h (encodeT err)
            Nothing ->
                liftIO $ T.hPutStrLn h (encodeT (GeneralError "Error parsing Command.")) >> hFlush stdout
        liftIO $ hClose h

handleCommand :: (PscIde m, MonadLogger m) =>
                 Command -> m (Either Error Success)
handleCommand (Load modules deps) =
    loadModulesAndDeps modules deps
handleCommand (Type search filters) =
    Right <$> findType search filters
handleCommand (Complete filters matcher) =
    Right <$> findCompletions filters matcher
handleCommand (Pursuit query Package) =
    Right <$> findPursuitPackages query
handleCommand (Pursuit query Identifier) =
    Right <$> findPursuitCompletions query
handleCommand (List LoadedModules) =
    Right <$> printModules
handleCommand (List AvailableModules) =
    Right <$> listAvailableModules
handleCommand (List (Imports fp)) =
    importsForFile fp
handleCommand Cwd =
    Right . TextResult . T.pack <$> liftIO getCurrentDirectory
handleCommand Quit = Right <$> liftIO exitSuccess
