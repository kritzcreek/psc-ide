{-# LANGUAGE OverloadedStrings #-}

module PureScript.Ide.ExternsSpec where

import           PureScript.Ide.Externs
import           Test.Hspec

spec :: Spec
spec = do
    describe "Parsing imports" $ do
        it "parses a simple import statement" $
            parseExtern "import Data.Array" `shouldBe`
              Right (Dependency "Data.Array" [])
        it "parses a simple import statement" $
            parseExtern "import Data.Text (functionName, (++))" `shouldBe`
              Right (Dependency "Data.Text" ["functionName", "++"])
        it "parses a hiding import statement" $
            parseExtern "import Prelude hiding (wasd)" `shouldBe`
              Right (Dependency "Prelude" [])
        it "parses a qualified import statement" $
            parseExtern "import qualified Data.Text as T" `shouldBe`
              Right (Dependency "Data.Text" [])
    describe "Parsing modules" $
        it "parses a module declaration" $
            parseExtern "module My.Module (exportedFunction) where" `shouldBe`
              Right (ModuleDecl "My.Module" ["exportedFunction"])
    describe "Parsing functions" $
        it "parses a function declaration" $
            parseExtern "foreign import text :: forall eff s t. Prim.String -> SYM.Component eff s t"
              `shouldBe` Right (FunctionDecl "text" "forall eff s t. Prim.String -> SYM.Component eff s t")
    describe "Parsing Data Declaration" $ do
        it "parses a data declaration" $
            parseExtern "data Handler (eff :: # !) (s :: *)" `shouldBe`
              Right (DataDecl "Handler" "(eff :: # !) (s :: *)")
        it "parses a foreign data declaration" $
            parseExtern "foreign import data STArray :: * -> * -> *" `shouldBe`
              Right (DataDecl "STArray" "* -> * -> *")
    describe "Parsing type declarations" $ do
        it "parses a newtype declaration" $
            parseExtern "newtype Component (eff :: # !) (s :: *) (t :: *) = Component (s -> SYM.Handler eff t -> Prim.Array React.ReactElement)" `shouldBe`
              Right (DataDecl "Component" "(s -> SYM.Handler eff t -> Prim.Array React.ReactElement)")
        it "parses a type declaration" $
            parseExtern "type Player = { age :: Prim.Number, name :: Prim.String }" `shouldBe`
              Right (DataDecl "Player" "{ age :: Prim.Number, name :: Prim.String }")


