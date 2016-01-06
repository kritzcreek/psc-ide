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
import           Control.Monad.Except
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

-- "Borrowed" from the Idris Compiler
-- Copied from upstream impl of listenOn
-- bound to localhost interface instead of iNADDR_ANY
listenOnLocalhost :: PortID -> IO Socket
listenOnLocalhost (PortNumber port) = do
  proto <- getProtocolNumber "tcp"
  localhost <- inet_addr "127.0.0.1"
  bracketOnError
    (socket AF_INET Stream proto)
    sClose
    (\sock -> do
      setSocketOption sock ReuseAddr 1
      bindSocket sock (SockAddrInet port localhost)
      listen sock maxListenQueue
      pure sock)
listenOnLocalhost _ = error "Wrong Porttype"

data Options = Options
  { optionsDirectory  :: Maybe FilePath
  , optionsOutputPath :: FilePath
  , optionsPort       :: Int
  , optionsDebug      :: Bool
  }

main :: IO ()
main = do
  Options dir outputPath port debug  <- execParser opts
  maybe (pure ()) setCurrentDirectory dir
  serverState <- newTVarIO emptyPscState
  cwd <- getCurrentDirectory
  _ <- forkFinally (watcher serverState (cwd </> outputPath)) print
  let conf =
        Configuration
        {
          confDebug = debug
        , confOutputPath = outputPath
        }
  let env =
        PscEnvironment
        {
          envStateVar = serverState
        , envConfiguration = conf
        }
  startServer (PortNumber (fromIntegral port)) env
  where
    parser =
      Options <$>
        optional (strOption (long "directory" <> short 'd')) <*>
        strOption (long "output-directory" <> value "output/") <*>
        option auto (long "port" <> short 'p' <> value 4242) <*>
        switch (long "debug")
    opts = info (version <*> parser) mempty
    version = abortOption
      (InfoMsg (showVersion Paths.version))
      (long "version" <> help "Show the version number")

startServer :: PortID -> PscEnvironment -> IO ()
startServer port env = withSocketsDo $ do
  sock <- listenOnLocalhost port
  runLogger (runReaderT (forever (loop sock)) env)
  where
    runLogger = runStdoutLoggingT . filterLogger (\_ _ -> confDebug (envConfiguration env))

    loop :: (PscIde m, MonadLogger m) => Socket -> m ()
    loop sock = do
      (cmd,h) <- acceptCommand sock
      case decodeT cmd of
        Just cmd' -> do
          result <- runExceptT (handleCommand cmd')
          $(logDebug) ("Answer was: " <> T.pack (show result))
          liftIO (hFlush stdout)
          case result of
            -- What function can I use to clean this up?
            Right r  -> liftIO $ T.hPutStrLn h (encodeT r)
            Left err -> liftIO $ T.hPutStrLn h (encodeT err)
        Nothing -> do
          $(logDebug) ("Parsing the command failed. Command: " <> cmd)
          liftIO $ T.hPutStrLn h (encodeT (GeneralError "Error parsing Command.")) >> hFlush stdout
      liftIO (hClose h)


acceptCommand :: (MonadIO m, MonadLogger m) => Socket -> m (T.Text, Handle)
acceptCommand sock = do
  h <- acceptConnection
  $(logDebug) "Accepted a connection"
  cmd <- liftIO (T.hGetLine h)
  $(logDebug) cmd
  pure (cmd, h)
  where
   acceptConnection = liftIO $ do
     (h,_,_) <- accept sock
     hSetEncoding h utf8
     hSetBuffering h LineBuffering
     pure h

handleCommand :: (PscIde m, MonadLogger m, MonadError PscIdeError m) =>
                 Command -> m Success
handleCommand (Load modules deps) =
    loadModulesAndDeps modules deps
handleCommand (Type search filters) =
    findType search filters
handleCommand (Complete filters matcher) =
    findCompletions filters matcher
handleCommand (Pursuit query Package) =
    findPursuitPackages query
handleCommand (Pursuit query Identifier) =
    findPursuitCompletions query
handleCommand (List LoadedModules) =
    printModules
handleCommand (List AvailableModules) =
    listAvailableModules
handleCommand (List (Imports fp)) =
    importsForFile fp
handleCommand (CaseSplit l b e t) =
    caseSplit l b e t
handleCommand Cwd =
    TextResult . T.pack <$> liftIO getCurrentDirectory
handleCommand Quit = liftIO exitSuccess
