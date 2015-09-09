{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module PureScript.Ide
  (
    emptyPscState,
    findTypeForName,
    findCompletionsByPrefix,
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

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Lazy (StateT (..), get, modify)
import qualified Data.Map.Lazy            as M
import           Data.Maybe               (mapMaybe)
import           Data.Monoid
import           Data.Text                (Text ())
import qualified Data.Text                as T
import           PureScript.Ide.Command
import           PureScript.Ide.Completion
import           PureScript.Ide.Externs
import           PureScript.Ide.Pursuit
import           PureScript.Ide.Err
import           PureScript.Ide.Types

type PscIde = StateT PscState IO

getAllDecls :: PscIde [ExternDecl]
getAllDecls = concat . pscStateModules <$> get

getDeclsForModules :: [ModuleIdent] -> PscIde [ExternDecl]
getDeclsForModules moduleIdents = do
    modules <- pscStateModules <$> get
    return . concat $ mapMaybe (`M.lookup` modules) moduleIdents

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

findCompletions :: [CompletionFilter] -> PscIde [Completion]
findCompletions filters =
    applyFilters filters . M.toList . pscStateModules <$> get

findPursuitCompletions :: Text -> PscIde [Completion]
findPursuitCompletions = liftIO . searchPursuit

findCompletionsByPrefix :: Text -> Level -> PscIde [DeclIdent]
findCompletionsByPrefix prefix level =
  case level of
       Pursuit -> liftM2 mappend fileMatches pursuitMatches
       _       -> fileMatches
  where
    fileMatches    = findCompletionsByPrefix' prefix <$> getAllDecls
    pursuitMatches = liftIO $ fmap (\(_, x, _) -> x) <$> searchPursuit prefix

findCompletionsByPrefix' :: DeclIdent -> [ExternDecl] -> [DeclIdent]
findCompletionsByPrefix' prefix =
  mapMaybe matches
  where
    matches :: ExternDecl -> Maybe DeclIdent
    matches (FunctionDecl name _) = maybePrefix name
    matches (DataDecl name _) = maybePrefix name
    matches _ = Nothing
    maybePrefix name =
       if prefix `T.isPrefixOf` name
            then Just name
            else Nothing

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
