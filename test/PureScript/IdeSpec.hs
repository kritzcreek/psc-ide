{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
module PureScript.IdeSpec where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.List
import qualified Data.Map               as Map
import           PureScript.Ide
import           PureScript.Ide.Types
import           Test.Hspec

testState :: PscState
testState = PscState (Map.fromList [("Data.Array", []), ("Control.Monad.Eff", [])])

spec :: SpecWith ()
spec = do
  describe "list" $ do
    describe "loadedModules" $ do
      it "returns an empty list when no modules are loaded" $ do
       st <- newTVarIO emptyPscState
       result <- runReaderT printModules st
       result `shouldBe` ModuleList []
      it "returns the list of loaded modules" $ do
        st <- newTVarIO testState
        ModuleList result <- runReaderT printModules st
        sort result `shouldBe` sort ["Data.Array", "Control.Monad.Eff"]
