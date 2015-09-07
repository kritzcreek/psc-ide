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

queryUrl :: Text
queryUrl = "http://pursuit.purescript.org/search?q="

jsonOpts :: Options
jsonOpts = defaults & header "Accept" .~ ["application/json"]

searchPursuit :: Text -> IO [(Text, Text)]
searchPursuit query = do
    r <- getWith jsonOpts (T.unpack $ queryUrl <> query)
    let texts = r ^.. responseBody . values . key "text" . _String
    let (_,results) = partitionEithers $ typeParse <$> texts
    return results
