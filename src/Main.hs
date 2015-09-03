{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Network
import           Options.Applicative

data Options = Options
    { optionsPort :: Maybe Int
    }

main :: IO ()
main = do
    Options port <- execParser opts
    cmd <- T.getLine
    client cmd (PortNumber . fromIntegral $ fromMaybe 4242 port) >>= print
  where
    parser = Options <$> optional (option auto (long "port" <> short 'p'))
    opts = info parser mempty

client :: T.Text -> PortID -> IO T.Text
client cmd port = do
  h <- connectTo "localhost" port
  T.hPutStrLn h cmd
  T.hGetLine h
