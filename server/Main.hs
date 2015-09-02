{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Purescript.Ide

import           Control.Exception        (bracketOnError)
import           Control.Monad.State.Lazy
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Network                  hiding (socketPort)
import           Network.BSD              (getProtocolNumber)
import           Network.Socket           hiding (PortNumber, accept, sClose)

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

main :: IO ()
main = startServer (PortNumber 4242) emptyPscState

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
        (cmd, h) <- liftIO $ acceptCommand sock
        case parseCommand cmd of
          Right cmd' -> do
            result <- handleCommand cmd'
            liftIO $ T.hPutStrLn h result
          Left err -> liftIO $ hPrint h err
        liftIO $ hClose h

handleCommand :: Command -> PscIde T.Text
handleCommand (TypeLookup ident) = fromMaybe "Not found" <$> findTypeForName ident
handleCommand (Completion stub) = T.intercalate ", " <$> findCompletion stub
handleCommand (Load fp) = loadModule fp >> return "Success"
handleCommand Print = T.intercalate ", " <$> printModules
handleCommand Quit = liftIO exitSuccess
