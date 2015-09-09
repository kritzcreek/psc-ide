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
import           PureScript.Ide
import           PureScript.Ide.Command
import           PureScript.Ide.Err
import           PureScript.Ide.Externs   (ModuleIdent)
import           PureScript.Ide.Completion
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
listenOnLocalhost _ = error "Wrong Porttype"

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
                liftIO $ T.hPutStrLn h (textResult result)
            Left err ->
                liftIO $ T.hPutStrLn h (textErr err)
        liftIO $ hClose h

handleCommand :: Command -> PscIde (Either Err T.Text)
handleCommand (TypeLookup ident) =
    maybeToEither (NotFound ident) <$> findTypeForName ident
handleCommand (Complete prefix _ Nothing) =
    Right . T.intercalate ", " <$> map showCompletion <$>
    findCompletions [prefixFilter prefix]
handleCommand (Complete prefix _ (Just modules)) =
    Right . T.intercalate ", " <$> map showCompletion <$>
    findCompletions [prefixFilter prefix, moduleFilter modules]
handleCommand Print =
    Right . T.intercalate ", " <$> printModules
handleCommand Cwd =
    Right . T.pack <$> liftIO getCurrentDirectory
handleCommand (Load moduleName) =
    loadModule moduleName
handleCommand (LoadDependencies moduleName) =
    loadModuleDependencies' moduleName
handleCommand Quit =
    liftIO exitSuccess

loadModuleDependencies' :: ModuleIdent -> PscIde (Either Err T.Text)
loadModuleDependencies' moduleName = do
    _ <- loadModule moduleName
    mDeps <- getDependenciesForModule moduleName
    case mDeps of
        Just deps -> do
            mapM_ loadModule deps
            return (Right ("Dependencies for " <> moduleName <> " loaded."))
        Nothing -> return (Left (ModuleNotFound moduleName))

loadModule :: ModuleIdent -> PscIde (Either Err T.Text)
loadModule mn = do
    path <- liftIO $ filePathFromModule mn
    case path of
        Right p  -> loadExtern p >> return (Right $ "Loaded extern file at: " <> T.pack p)
        Left _ -> return (Left . GeneralErr $ "Could not load module " <> T.unpack mn)

filePathFromModule :: ModuleIdent -> IO (Either T.Text FilePath)
filePathFromModule moduleName = do
    cwd <- getCurrentDirectory
    let path = cwd </> "output" </> T.unpack moduleName </> "externs.purs"
    ex <- doesFileExist path
    return $
        if ex
            then Right path
            else Left ("Extern file for module " <> moduleName <>" could not be found")
