{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module PureScript.Ide
  (
    emptyPscState,
    findTypeForName,
    findCompletions,
    loadExtern,
    getDependenciesForModule,
    printModules,
    unsafeStateFromDecls,
    PscIde,
    PscState(..),
    first,
    maybeToEither,
    textResult,
  ) where

import           Control.Monad.Except
import           Control.Monad.State.Lazy (StateT (..), get, modify)
import qualified Data.Map.Lazy            as M
import           Data.Maybe               (mapMaybe)
import           Data.Monoid
import           Data.Text                (Text ())
import           PureScript.Ide.Completion
import           PureScript.Ide.Externs
import           PureScript.Ide.Pursuit
import           PureScript.Ide.Err
import           PureScript.Ide.Types

type PscIde = StateT PscState IO

getAllDecls :: PscIde [ExternDecl]
getAllDecls = concat . pscStateModules <$> get

getAllModules :: PscIde [Module]
getAllModules = M.toList . pscStateModules <$> get

-- | Given a set of ExternDeclarations finds the type for a given function
--   name and returns Nothing if the functionName can not be matched
findTypeForName :: DeclIdent -> PscIde (Maybe Type)
findTypeForName name =
  getFirst . foldMap (First . nameMatches) <$> getAllDecls
  where
    nameMatches :: ExternDecl -> Maybe Type
    nameMatches decl =
        case decl of
            FunctionDecl n t ->
                if name == n
                    then Just t
                    else Nothing
            _ -> Nothing

findCompletions :: [CompletionFilter] -> Matcher -> PscIde [Completion]
findCompletions filters matcher =
    getCompletions filters matcher <$> getAllModules

findPursuitCompletions :: Text -> PscIde [Completion]
findPursuitCompletions = liftIO . searchPursuit

loadExtern :: FilePath -> PscIde ()
loadExtern fp = do
    parseResult <- liftIO $ readExternFile fp
    case parseResult of
        Right decls ->
            let (name, decls') = unsafeModuleFromDecls decls
            in modify
                   (\x ->
                         x
                         { pscStateModules = M.insert
                               name
                               decls'
                               (pscStateModules x)
                         })
        Left _ -> liftIO $ putStrLn "The module could not be parsed"

getDependenciesForModule :: ModuleIdent -> PscIde (Maybe [ModuleIdent])
getDependenciesForModule m = do
  mDecls <- M.lookup m . pscStateModules <$> get
  return $ mapMaybe getDependencyName <$> mDecls
  where getDependencyName (Dependency dependencyName _) = Just dependencyName
        getDependencyName _ = Nothing

unsafeModuleFromDecls :: [ExternDecl] -> Module
unsafeModuleFromDecls (ModuleDecl name _ : decls) = (name, decls)
unsafeModuleFromDecls _ =
    error "An externs File didn't start with a module declaration"

unsafeStateFromDecls :: [[ExternDecl]] -> PscState
unsafeStateFromDecls = PscState . M.fromList . fmap unsafeModuleFromDecls

printModules :: PscIde [ModuleIdent]
printModules = M.keys . pscStateModules <$> get

-- | Taken from Data.Either.Utils
maybeToEither :: MonadError e m =>
                 e                      -- ^ (Left e) will be returned if the Maybe value is Nothing
              -> Maybe a                -- ^ (Right a) will be returned if this is (Just a)
              -> m a
maybeToEither errorval Nothing = throwError errorval
maybeToEither _ (Just normalval) = return normalval
