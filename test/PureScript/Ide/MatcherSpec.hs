{-# LANGUAGE OverloadedStrings #-}

module PureScript.Ide.MatcherSpec where

import Test.Hspec
import PureScript.Ide.Matcher
import PureScript.Ide.Types

completions :: [Completion]
completions = [
  Completion ("", "firstResult", ""),
  Completion ("", "secondResult", ""),
  Completion ("", "fiult", "")
  ]

mkResult :: [Int] -> [Completion]
mkResult = map (completions !!)

runFlex s = runMatcher (flexMatcher s) completions

spec = do
  describe "Flex Matcher" $ do
    it "doesn't match on an empty string" $
       runFlex "" `shouldBe` []
    it "matches on equality" $
      runFlex "firstResult" `shouldBe` mkResult [0]
    it "scores short matches higher and sorts accordingly" $
      runFlex "filt" `shouldBe` mkResult [2, 0]
