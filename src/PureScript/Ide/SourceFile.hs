{-# LANGUAGE FlexibleContexts #-}
module PureScript.Ide.SourceFile where

import           Data.Maybe                           (mapMaybe)
import           Data.Monoid
import qualified Data.Text                            as T
import qualified Language.PureScript.AST.Declarations as D
import qualified Language.PureScript.Parser           as P
import           PureScript.Ide.Error
import           PureScript.Ide.Types
import qualified Language.PureScript.AST.SourcePos    as SP
import qualified Language.PureScript.Names            as N
import           System.Directory

parseModuleFromFile :: FilePath -> IO (Either Error D.Module)
parseModuleFromFile fp = do
  exists <- doesFileExist fp
  if exists
    then do
      content <- readFile fp
      let m = do tokens <- P.lex fp content
                 P.runTokenParser "" P.parseModule tokens
      return (first (`ParseError` "File could not be parsed.") m)
    else return $ Left (NotFound "File does not exist.")

-- data Module = Module SourceSpan [Comment] ModuleName [Declaration] (Maybe [DeclarationRef])

getDeclarations :: D.Module -> [D.Declaration]
getDeclarations (D.Module _ _ _ declarations _) = declarations

getImports :: D.Module -> [D.Declaration]
getImports (D.Module _ _ _ declarations _) =
  mapMaybe isImport declarations
  where
    isImport (D.PositionedDeclaration _ _ (i@D.ImportDeclaration{})) = Just i
    isImport _ = Nothing

getImportsForFile :: FilePath -> IO (Either Error [ModuleImport])
getImportsForFile fp = do
  module' <- parseModuleFromFile fp
  let imports = getImports <$> module'
  return (fmap (mkModuleImport . unwrapPositionedImport) <$> imports)
  where mkModuleImport (D.ImportDeclaration mn importType' qualifier) =
          ModuleImport
            (T.pack (N.runModuleName mn))
            importType'
            (T.pack . N.runModuleName <$> qualifier)
        mkModuleImport _ = error "Shouldn't have gotten anything but Imports here"
        unwrapPositionedImport (D.ImportDeclaration mn importType' qualifier) =
          D.ImportDeclaration mn (unwrapImportType importType') qualifier
        unwrapPositionedImport x = x
        unwrapImportType (D.Explicit decls) = D.Explicit (map unwrapPositionedRef decls)
        unwrapImportType (D.Hiding decls)   = D.Hiding (map unwrapPositionedRef decls)
        unwrapImportType D.Implicit         = D.Implicit

getPositionedImports :: D.Module -> [D.Declaration]
getPositionedImports (D.Module _ _ _ declarations _) =
  mapMaybe isImport declarations
  where
    isImport i@(D.PositionedDeclaration _ _ (D.ImportDeclaration{})) = Just i
    isImport _ = Nothing

unwrapPositioned :: D.Declaration -> D.Declaration
unwrapPositioned (D.PositionedDeclaration _ _ x) = x
unwrapPositioned x = x

unwrapPositionedRef :: D.DeclarationRef -> D.DeclarationRef
unwrapPositionedRef (D.PositionedDeclarationRef _ _ x) = x
unwrapPositionedRef x = x

getDeclPosition :: D.Module -> String -> Maybe SP.SourceSpan
getDeclPosition m ident =
  let decls = getDeclarations m
  in getFirst (foldMap (match ident) decls)
     where match q (D.PositionedDeclaration ss _ decl) = First (if go q decl
                                                                then Just ss
                                                                else Nothing)
           match _ _ = First Nothing

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
  case m of
    Right module' -> return $ getDeclPosition module' q
    Left _ -> return Nothing
