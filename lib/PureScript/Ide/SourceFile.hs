{-# LANGUAGE FlexibleContexts #-}
module PureScript.Ide.SourceFile where

import Data.Maybe (mapMaybe)
import qualified Language.PureScript.Parser as P
import qualified Language.PureScript.AST.Declarations as D

parseModuleFromFile :: FilePath -> IO D.Module
parseModuleFromFile fp = do
  content <- readFile fp
  Right m <- return $ do
    tokens <- P.lex fp content
    P.runTokenParser "" P.parseModule tokens
  return m

-- data Module = Module SourceSpan [Comment] ModuleName [Declaration] (Maybe [DeclarationRef])

getDeclarations :: D.Module -> [D.Declaration]
getDeclarations (D.Module sourceSpan comments moduleName' declarations exports) = declarations

getImports :: D.Module -> [D.Declaration]
getImports (D.Module sourceSpan comments moduleName' declarations exports) =
  mapMaybe isImport declarations
  where
    isImport (D.PositionedDeclaration _ _ (i@D.ImportDeclaration{})) = Just i
    isImport _ = Nothing

getPositionedImports :: D.Module -> [D.Declaration]
getPositionedImports (D.Module sourceSpan comments moduleName' declarations exports) =
  mapMaybe isImport declarations
  where
    isImport i@(D.PositionedDeclaration _ _ (D.ImportDeclaration{})) = Just i
    isImport _ = Nothing

unwrapPositioned :: D.Declaration -> D.Declaration
unwrapPositioned (D.PositionedDeclaration _ _ x) = x
unwrapPositioned x = x
