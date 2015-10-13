{-# LANGUAGE FlexibleContexts #-}
module PureScript.Ide.SourceFile where

import Data.Maybe (mapMaybe, listToMaybe)
import qualified Language.PureScript.Parser as P
import qualified Language.PureScript.AST.Declarations as D
-- import qualified Language.PureScript.AST.Traversals as T
import qualified Language.PureScript.AST.SourcePos as SP
import qualified Language.PureScript.Names as N
-- import qualified Language.PureScript.Types as Ty

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

getDeclPosition :: D.Module -> String -> Maybe SP.SourceSpan
getDeclPosition m ident =
  let decls = getDeclarations m
  in listToMaybe (mapMaybe (match ident) decls)
     where match q (D.PositionedDeclaration ss _ decl) = if go q decl then Just ss else Nothing
           match _ _ = Nothing

           go q (D.DataDeclaration _ name _ constructors)  =
             properEqual name q || any (\(x,_) -> properEqual x q) constructors
           go q (D.DataBindingGroupDeclaration decls)      = any (go q) decls
           go q (D.TypeSynonymDeclaration name _ _)        = properEqual name q
           go q (D.TypeDeclaration ident' _)               = identEqual ident' q
           go q (D.ValueDeclaration ident' _ _ _)          = identEqual ident' q
           go q (D.ExternDeclaration ident' _)             = identEqual ident' q
           go q (D.ExternDataDeclaration name _)           = properEqual name q
           go q (D.TypeClassDeclaration name _ _ members)  =
             properEqual name q || any (go q . unwrapPositioned) members
           go q (D.TypeInstanceDeclaration ident' _ _ _ _) =
             identEqual ident' q
           go _ _ = False

           properEqual x q = N.runProperName x == q
           identEqual x q = N.runIdent x == q

goToDefinition :: String -> FilePath -> IO (Maybe SP.SourceSpan)
goToDefinition q fp = do
  m <- parseModuleFromFile fp
  return $ getDeclPosition m q
