{-# LANGUAGE OverloadedStrings #-}
module PureScript.IdeSpec where

import Test.Hspec
import Control.Monad.State
import qualified Data.Map as Map
import PureScript.Ide
import PureScript.Ide.Types

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
        result <- evalStateT printModules testState
        result `shouldBe` ModuleList ["Data.Array", "Control.Monad.Eff"]
