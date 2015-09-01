{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Network

main :: IO ()
main = do
  cmd <- T.getLine
  client cmd >>= print

client :: T.Text -> IO T.Text
client cmd = do
  h <- connectTo "localhost" (PortNumber 4242)
  T.hPutStrLn h cmd
  T.hGetLine h
