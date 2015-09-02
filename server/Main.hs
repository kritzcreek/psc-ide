{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception        (bracketOnError)
import           Control.Monad.State.Lazy
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Network                  hiding (socketPort)
import           Network.BSD              (getProtocolNumber)
import           Network.Socket           hiding (PortNumber, accept, sClose)
import           Options.Applicative
import           Purescript.Ide
import           System.Directory
import           System.Exit
import           System.FilePath
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

data Options = Options
    { optionsDirectory :: Maybe FilePath
    , optionsPort      :: Maybe Int
    }

main :: IO ()
main = do
    Options dir port <- execParser opts
    maybe (return ()) setCurrentDirectory dir
    startServer (PortNumber . fromIntegral $ fromMaybe 4242 port) emptyPscState
  where
    parser =
        Options <$>
          optional (strOption (long "directory" <> short 'd')) <*>
          optional (option auto (long "port" <> short 'p'))
    opts = info parser mempty


startServer :: PortID -> PscState -> IO ()
startServer port st_in =
    withSocketsDo $
    do sock <- listenOnLocalhost port
       evalStateT (forever (loop sock)) st_in
  where
    acceptCommand sock = do
        (h,_,_) <- accept sock
        hSetEncoding h utf8
        cmd <- T.hGetLine h
        return (cmd, h)
    loop :: Socket -> PscIde ()
    loop sock = do
        (cmd,h) <- liftIO $ acceptCommand sock
        case parseCommand cmd of
            Right cmd' -> do
                result <- handleCommand cmd'
                liftIO $ T.hPutStrLn h result
            Left err -> liftIO $ hPrint h err
        liftIO $ hClose h

handleCommand :: Command -> PscIde T.Text
handleCommand (TypeLookup ident) = fromMaybe "Not found" <$> findTypeForName ident
handleCommand (Completion stub) = T.intercalate ", " <$> findCompletion stub
handleCommand (Load moduleName) = do
  path <- liftIO $ filePathFromModule moduleName
  case path of
    Right p -> loadModule p >> return "Success"
    Left err -> return err
handleCommand Print = T.intercalate ", " <$> printModules
handleCommand Quit = liftIO exitSuccess

filePathFromModule :: T.Text -> IO (Either T.Text FilePath)
filePathFromModule moduleName = do
    cwd <- getCurrentDirectory
    let path = cwd </> "output" </> T.unpack moduleName </> "externs.purs"
    ex <- doesFileExist path
    return $
        if ex
            then Right path
            else Left "Module could not be found"
