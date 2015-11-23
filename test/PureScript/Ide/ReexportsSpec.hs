{-# LANGUAGE OverloadedStrings #-}
module PureScript.Ide.ReexportsSpec where

import Test.Hspec
import PureScript.Ide.Reexports
import PureScript.Ide.Types
import Data.List (sort)
import qualified Data.Map as Map
import Control.Exception (evaluate)

decl1 = FunctionDecl "filter" "asdasd"
decl2 = DataDecl "Tree" "* -> *"
decl3 = DataDecl "TreeAsd" "* -> *"

circularModule = ("Circular", [Export "Circular"])

module1 :: Module
module1 = ("Module1", [Export "Module2", Export "Module3", decl1])

module2 :: Module
module2 = ("Module2", [decl2])

module3 :: Module
module3 = ("Module3", [decl3])

result :: Module
result = ("Module1", [decl1, decl2, Export "Module3"])

db = Map.fromList [module1, module2, module3]

shouldBeEqualSorted :: Module -> Module -> Expectation
shouldBeEqualSorted (n1, d1) (n2, d2) = (n1, sort d1) `shouldBe` (n2, sort d2)

spec = do
  describe "Reexports" $ do
    it "finds all reexports" $
      getReexports module1 `shouldBe` [Export "Module2", Export "Module3"]

    it "replaces a reexport with another module" $
      replaceReexport (Export "Module2") module1 module2 `shouldBeEqualSorted` result

    it "adds another module even if there is no export statement" $
      replaceReexport (Export "Module2") ("Module1", [decl1, Export "Module3"]) module2
      `shouldBeEqualSorted` result

    it "only adds a declaration once" $
      let replaced = replaceReexport (Export "Module2") module1 module2
      in replaceReexport (Export "Module2") replaced module2  `shouldBeEqualSorted` result

    it "should error when given a non-Export to replace" $
      evaluate (replaceReexport decl1 module1 module2) `shouldThrow` errorCall "Should only get Exports here."
    it "replaces all Exports with their corresponding declarations" $
      replaceReexports module1 db `shouldBe` ("Module1", [decl1, decl2, decl3])

    it "does not list itself as a reexport" $
      getReexports circularModule `shouldBe` []

    it "does not include circular references when replacing reexports" $
      replaceReexports circularModule (uncurry Map.singleton circularModule )
      `shouldBe` ("Circular", [])
