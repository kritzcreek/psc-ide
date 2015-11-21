{-# LANGUAGE OverloadedStrings #-}
module PureScript.IdeSpec where

import           Control.Monad.State
import           Data.List
import qualified Data.Map             as Map
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
       result <- evalStateT printModules emptyPscState
       result `shouldBe` ModuleList []
      it "returns the list of loaded modules" $ do
        ModuleList result <- evalStateT printModules testState
        sort result `shouldBe` sort ["Data.Array", "Control.Monad.Eff"]
