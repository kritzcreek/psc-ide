{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}


module PureScript.Ide.CaseSplit where

import           Control.Concurrent.STM
import           "monad-logger" Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Except
import Data.List (find)
import qualified Data.Map                    as M
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Language.PureScript.Externs
import           Language.PureScript.Names
import           Language.PureScript.Types
import           Language.PureScript.Pretty
import           Language.PureScript.Environment
import           Language.PureScript.Parser.Types
import           Language.PureScript.Parser.Lexer (lex)
import           Language.PureScript.Parser.Common (runTokenParser)
import           Prelude hiding (lex)
import           PureScript.Ide.Error
import           PureScript.Ide.Types hiding (Type)
import           Text.Parsec as P

type Constructor = (ProperName 'ConstructorName, [Type])

getExternFiles :: (PscIde m) =>
                  m (M.Map ModuleName ExternsFile)
getExternFiles = do
  stateVar <- envStateVar <$> ask
  liftIO (externsFiles <$> readTVarIO stateVar)

isEDType :: ExternsDeclaration -> Bool
isEDType (EDType _ _ _) = True
isEDType _ = False

caseSplit :: (PscIde m, MonadLogger m, MonadError PscIdeError m) =>
             Text -> m [Constructor]
caseSplit q = do
  (tc, args) <- splitTypeConstructor (parseType' (T.unpack q))
  (EDType _ _ (DataType typeVars ctors)) <- findTypeDeclaration tc
  let applyTypeVars = everywhereOnTypes (replaceAllTypeVars (zip (map fst typeVars) args))
  let appliedCtors = map (\(n, ts) -> (n, map applyTypeVars ts)) ctors
  pure appliedCtors

{- ["EDType {
     edTypeName = ProperName {runProperName = \"Either\"}
   , edTypeKind = FunKind Star (FunKind Star Star)
   , edTypeDeclarationKind =
       DataType [(\"a\",Just Star),(\"b\",Just Star)]
                [(ProperName {runProperName = \"Left\"},[TypeVar \"a\"])
                ,(ProperName {runProperName = \"Right\"},[TypeVar \"b\"])]}"]
-}

findTypeDeclaration :: (PscIde m, MonadLogger m, MonadError PscIdeError m) =>
                         ProperName 'TypeName -> m ExternsDeclaration
findTypeDeclaration q = do
  efs <- getExternFiles
  let m = getAlt $ foldMap (findTypeDeclaration' q) efs
  case m of
    Just mn -> pure mn
    Nothing -> throwError (GeneralError "Not Found")

findTypeDeclaration' ::
  ProperName 'TypeName
  -> ExternsFile
  -> Alt Maybe ExternsDeclaration
findTypeDeclaration' t ExternsFile{..} =
  Alt $ find (\case
            EDType tn _ _ -> tn == t
            _ -> False) efDeclarations

splitTypeConstructor :: (MonadError PscIdeError m) =>
                        Type -> m (ProperName 'TypeName, [Type])
splitTypeConstructor = go []
  where
    go acc (TypeApp ty arg) = go (arg : acc) ty
    go acc (TypeConstructor tc) = pure (disqualify tc, acc)
    go _ _ = throwError (GeneralError "Failed to read TypeConstructor")

prettyCtor :: Constructor -> Text
prettyCtor (ctorName, []) = T.pack (runProperName ctorName)
prettyCtor (ctorName, ctorArgs) =
  "("<> T.pack (runProperName ctorName) <> " "
  <> T.unwords (map prettyCtorArg ctorArgs) <>")"

prettyCtorArg :: Type -> Text
prettyCtorArg t = "( _ :: " <> T.pack (prettyPrintTypeAtom t) <> ")"

makePattern ::
  Text -> -- ^ current line
  Int -> -- ^ begin of the split
  Int -> -- ^ end of the split
  [Constructor] -> -- ^ constructors to split
  [Text]
makePattern t x y = makePattern' (T.take x t) (T.drop y t)
  where
    makePattern' lhs rhs = map (\ctor -> lhs <> prettyCtor ctor <> rhs)

parseType' :: String -> Type
parseType' s = let (Right t) = do
                     ts <- lex "" s
                     runTokenParser "" (parseType <* P.eof) ts
               in t
