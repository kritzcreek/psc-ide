{-# LANGUAGE FlexibleContexts #-}
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
    readExternFile,
    convertExterns
  ) where


import           Data.Maybe                  (mapMaybe)
import           Control.Monad.Except
import           Data.Text (Text)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import qualified Language.PureScript.Externs as PE
import qualified Language.PureScript.AST.Declarations as D
import qualified Language.PureScript.Names   as N
import qualified Language.PureScript.Pretty  as PP
import           PureScript.Ide.CodecJSON
import           PureScript.Ide.Error        (PscIdeError (..))
import           PureScript.Ide.Types

readExternFile :: (MonadIO m, MonadError PscIdeError m) =>
                  FilePath -> m PE.ExternsFile
readExternFile fp = do
   parseResult <- liftIO (decodeT <$> T.readFile fp)
   case parseResult of
     Nothing -> throwError . GeneralError $ "Parsing the extern at: " ++ fp ++ " failed"
     Just externs -> pure externs

moduleNameToText :: N.ModuleName -> Text
moduleNameToText = T.pack . N.runModuleName

properNameToText :: N.ProperName a -> Text
properNameToText = T.pack . N.runProperName

identToText :: N.Ident -> Text
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
convertImport ei =
  Dependency
  (moduleNameToText (PE.eiModule ei))
  []
  (T.pack . N.runModuleName <$> PE.eiImportedAs ei)

convertExport :: D.DeclarationRef -> Maybe ExternDecl
convertExport (D.ModuleRef mn) = Just (Export (T.pack $ N.runModuleName mn))
convertExport (D.PositionedDeclarationRef _ _ (D.ModuleRef mn)) = Just (Export (T.pack $ N.runModuleName mn))
convertExport _ = Nothing

convertDecl :: PE.ExternsDeclaration -> Maybe ExternDecl
convertDecl PE.EDType{..} = Just $
  DataDecl
  (properNameToText edTypeName)
  (packAndStrip (PP.prettyPrintKind edTypeKind))
convertDecl PE.EDTypeSynonym{..} = Just $
  DataDecl
  (properNameToText edTypeSynonymName)
  (packAndStrip (PP.prettyPrintType edTypeSynonymType))
convertDecl PE.EDDataConstructor{..} = Just $
  DataDecl
  (properNameToText edDataCtorName)
  (packAndStrip (PP.prettyPrintType edDataCtorType))
convertDecl PE.EDValue{..} = Just $
  FunctionDecl
  (identToText edValueName)
  (packAndStrip (PP.prettyPrintType edValueType))
convertDecl _ = Nothing

packAndStrip :: String -> Text
packAndStrip = T.unwords . fmap T.strip . T.lines . T.pack
