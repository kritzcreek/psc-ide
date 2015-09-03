{-# LANGUAGE OverloadedStrings #-}

module Purescript.IdeSpec where

import Data.Either (isLeft)
import Test.Hspec
import Purescript.Ide

spec :: Spec
spec = do
  describe "Parsing commands" $ do
    it "parses a load command" $
      parseCommand "load Module.Name" `shouldBe` Right (Load "Module.Name")
    it "parses a print command" $
      parseCommand "print" `shouldBe` Right Print
    it "parses a cwd command" $
      parseCommand "cwd" `shouldBe` Right Cwd
    it "parses a quit command" $
      parseCommand "quit" `shouldBe` Right Quit
    it "parses a type lookup command" $
      parseCommand "typeLookup ident" `shouldBe` Right (TypeLookup "ident")
    it "parses a completion command" $
      parseCommand "complete stub" `shouldBe` Right (Completion "stub")
    it "fails to parse a malformed command" $
      parseCommand "compasd asd" `shouldSatisfy` isLeft
