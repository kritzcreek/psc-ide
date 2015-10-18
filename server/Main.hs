{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception        (bracketOnError)
import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
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
import           System.Directory
import           System.Exit
import           System.IO

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
    startServer (PortNumber . fromIntegral $ fromMaybe 4242 port) debug emptyPscState
  where
    parser =
        Options <$>
          optional (strOption (long "directory" <> short 'd')) <*>
          optional (option auto (long "port" <> short 'p')) <*>
          switch (long "debug")
    opts = info parser mempty


startServer :: PortID -> Bool -> PscState -> IO ()
startServer port debug st_in =
    withSocketsDo $
    do sock <- listenOnLocalhost port
       evalStateT (forever (loop sock)) st_in
  where
    acceptCommand sock = do
        (h,_,_) <- accept sock
        hSetEncoding h utf8
        cmd <- T.hGetLine h
        when debug (T.putStrLn cmd)
        return (cmd, h)
    loop :: Socket -> PscIde ()
    loop sock = do
        (cmd,h) <- liftIO $ acceptCommand sock
        case decodeT cmd of
            Just cmd' -> do
                result <- handleCommand cmd'
                when debug $ liftIO $ T.putStrLn ("Answer was: " <> (T.pack . show $ result))
                case result of
                  -- What function can I use to clean this up?
                  Right r  -> liftIO $ T.hPutStrLn h (encodeT r)
                  Left err -> liftIO $ T.hPutStrLn h (encodeT err)
            Nothing ->
                liftIO $ T.hPutStrLn h $ encodeT (GeneralError "Error parsing Command.")
        liftIO $ hClose h

handleCommand :: Command -> PscIde (Either Error Success)
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
handleCommand (List (Imports fp)) =
    importsForFile fp
handleCommand Cwd =
    Right . TextResult . T.pack <$> liftIO getCurrentDirectory
handleCommand Quit = Right <$> liftIO exitSuccess
