{-# LANGUAGE OverloadedStrings #-}

module PureScript.Ide.CommandSpec where

import           Data.Either    (isLeft)
import           PureScript.Ide.Command
import           Test.Hspec

spec :: Spec
spec = do
  describe "Parsing commands" $ do
    it "parses a load command" $
      parseCommand "load Module.Name" `shouldBe` Right (Load "Module.Name")
    it "parses a dependencies command" $
      parseCommand "dependencies Module.Name" `shouldBe` Right (LoadDependencies "Module.Name")
    it "parses a print command" $
      parseCommand "print" `shouldBe` Right Print
    it "parses a cwd command" $
      parseCommand "cwd" `shouldBe` Right Cwd
    it "parses a quit command" $
      parseCommand "quit" `shouldBe` Right Quit
    it "parses a type lookup command" $
      parseCommand "typeLookup ident" `shouldBe` Right (TypeLookup "ident")
    it "parses a completion command" $
      parseCommand "complete stub Project" `shouldBe` Right (Complete "stub" Project Nothing)
    it "parses a completion command with modules" $
      parseCommand "complete stub Project using Data.Array, Data.List"
        `shouldBe`
          Right (Complete "stub" Project (Just ["Data.Array", "Data.List"]))
    it "fails to parse a malformed command" $
      parseCommand "compasd asd" `shouldSatisfy` isLeft
