{-# LANGUAGE OverloadedStrings #-}
module PureScript.Ide.Pursuit where

import           Control.Lens
import           Data.Aeson.Lens
import           Data.Either
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Network.Wreq
import           PureScript.Ide.Externs (typeParse)
import           PureScript.Ide.Completion (Completion())

queryUrl :: Text
queryUrl = "http://pursuit.purescript.org/search?q="

jsonOpts :: Options
jsonOpts = defaults & header "Accept" .~ ["application/json"]

myZip :: [a] -> [(b, c)] -> [(a, b, c)]
myZip = zipWith (\a (b, c) -> (a, b, c))

searchPursuit :: Text -> IO [Completion]
searchPursuit query = do
    r <- getWith jsonOpts (T.unpack $ queryUrl <> query)
    let texts = r ^.. responseBody . values . key "text" . _String
    let moduleNames = r ^..
                      responseBody .
                      values .
                      key "info" .
                      key "module" .
                      _String
    return $ myZip moduleNames $ rights $ typeParse <$> texts
