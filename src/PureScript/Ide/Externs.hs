{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module PureScript.Ide.Externs
  (
    ExternDecl(..),
    ModuleIdent,
    DeclIdent,
    Type,
    Fixity(..),
    readExternFile
  ) where


import           Data.Maybe                  (mapMaybe)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import qualified Language.PureScript.Externs as PE
import qualified Language.PureScript.AST.Declarations as D
import qualified Language.PureScript.Names   as N
import qualified Language.PureScript.Pretty  as PP
import           PureScript.Ide.CodecJSON
import           PureScript.Ide.Error        (Error (..))
import           PureScript.Ide.Types

readExternFile :: FilePath -> IO (Either Error Module)
readExternFile fp = do
   (parseResult :: Maybe PE.ExternsFile) <- decodeT <$> T.readFile fp
   case parseResult of
     Nothing -> return . Left . GeneralError $ "Parsing the extern at: " ++ fp ++ " failed"
     Just externs -> return (Right (convertExterns externs))

moduleNameToText :: N.ModuleName -> T.Text
moduleNameToText = T.pack . N.runModuleName

properNameToText :: N.ProperName a -> T.Text
properNameToText = T.pack . N.runProperName

identToText :: N.Ident -> T.Text
identToText  = T.pack . N.runIdent

convertExterns :: PE.ExternsFile -> Module
convertExterns ef = (moduleName, exportDecls ++ importDecls ++ otherDecls)
  where
    moduleName = moduleNameToText (PE.efModuleName ef)
    importDecls = convertImport <$> PE.efImports ef
    exportDecls = mapMaybe convertExport (PE.efExports ef)
    -- Ignoring operator fixities for now since we're not using them
    -- operatorDecls = convertOperator <$> PE.efFixities ef
    otherDecls = mapMaybe convertDecl (PE.efDeclarations ef)

convertImport :: PE.ExternsImport -> ExternDecl
convertImport ei = Dependency (moduleNameToText (PE.eiModule ei)) []

convertExport :: D.DeclarationRef -> Maybe ExternDecl
convertExport (D.ModuleRef mn) = Just (Export (T.pack $ N.runModuleName mn))
convertExport _ = Nothing

convertDecl :: PE.ExternsDeclaration -> Maybe ExternDecl
convertDecl (PE.EDType{..}) = Just $
  DataDecl
  (properNameToText edTypeName)
  (T.pack (PP.prettyPrintKind edTypeKind))
convertDecl (PE.EDTypeSynonym{..}) = Just $
  DataDecl
  (properNameToText edTypeSynonymName)
  (T.pack (PP.prettyPrintType edTypeSynonymType))
convertDecl (PE.EDDataConstructor{..}) = Just $
  DataDecl
  (properNameToText edDataCtorName)
  (T.pack (PP.prettyPrintType edDataCtorType))
convertDecl (PE.EDValue{..}) = Just $
  FunctionDecl
  (identToText edValueName)
  (T.pack (PP.prettyPrintType edValueType))
convertDecl _ = Nothing
