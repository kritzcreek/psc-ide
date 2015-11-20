{-# LANGUAGE TupleSections #-}
module PureScript.Ide where

import           Control.Monad.Except
import           Control.Monad.State.Lazy (StateT (..), get, modify)
import           Control.Monad.Trans.Either
import qualified Data.Map.Lazy            as M
import           Data.Maybe               (mapMaybe)
import           Data.Monoid
import qualified Data.Text  as T
import           PureScript.Ide.Completion
import           PureScript.Ide.Externs
import           PureScript.Ide.Pursuit
import           PureScript.Ide.Error
import           PureScript.Ide.Types
import           PureScript.Ide.SourceFile
import           System.FilePath
import           System.Directory

type PscIde = StateT PscState IO

getPscIdeState :: PscIde (M.Map ModuleIdent [ExternDecl])
getPscIdeState = pscStateModules <$> get

getAllDecls :: PscIde [ExternDecl]
getAllDecls = concat <$> getPscIdeState

getAllModules :: PscIde [Module]
getAllModules = M.toList <$> getPscIdeState

getModule :: ModuleIdent -> PscIde (Maybe Module)
getModule m = do
  modules <- getPscIdeState
  return ((m,) <$> M.lookup m modules)

insertModule :: Module -> PscIde ()
insertModule (name, decls) =
  modify (\x -> x { pscStateModules = M.insert name decls (pscStateModules x)})

findCompletions :: [Filter] -> Matcher -> PscIde Success
findCompletions filters matcher =
  CompletionResult <$> getCompletions filters matcher <$> getAllModules

findType :: DeclIdent -> [Filter] -> PscIde Success
findType search filters =
  CompletionResult <$> getExactMatches search filters <$> getAllModules

findPursuitCompletions :: PursuitQuery -> PscIde Success
findPursuitCompletions (PursuitQuery q) =
  PursuitResult <$> liftIO (searchPursuitForDeclarations q)

findPursuitPackages :: PursuitQuery -> PscIde Success
findPursuitPackages (PursuitQuery q) =
  PursuitResult <$> liftIO (findPackagesForModuleIdent q)

loadExtern :: FilePath -> PscIde (Either Error ())
loadExtern fp = runEitherT $ do
  m <- EitherT . liftIO $ readExternFile fp
  lift (insertModule m)

printModules :: PscIde Success
printModules = do
  modules <- M.keys <$> getPscIdeState
  return (ModuleList modules)

listAvailableModules :: PscIde Success
listAvailableModules = liftIO $ do
  cwd <- getCurrentDirectory
  modules <- getDirectoryContents (cwd </> "output")
  let cleanedModules = filter (`notElem` [".", ".."]) modules
  return (ModuleList (map T.pack cleanedModules))

importsForFile :: FilePath -> PscIde (Either Error Success)
importsForFile fp = do
  imports <- liftIO (getImportsForFile fp)
  return (ImportList <$> imports)

-- | The first argument is a set of modules to load. The second argument
--   denotes modules for which to load dependencies
loadModulesAndDeps :: [ModuleIdent] -> [ModuleIdent] -> PscIde (Either Error Success)
loadModulesAndDeps mods deps = do
  r1 <- mapM loadModule (mods ++ deps)
  r2 <- mapM loadModuleDependencies deps
  return $ do
    moduleResults <- fmap T.concat (sequence r1)
    dependencyResults <- fmap T.concat (sequence r2)
    return (TextResult (moduleResults <> ", " <> dependencyResults))

loadModuleDependencies :: ModuleIdent -> PscIde (Either Error T.Text)
loadModuleDependencies moduleName = do
  m <- getModule moduleName
  case getDependenciesForModule <$> m of
    Just deps -> do
      mapM_ loadModule deps
      return (Right ("Dependencies for " <> moduleName <> " loaded."))
    Nothing -> return (Left (ModuleNotFound moduleName))

getDependenciesForModule :: Module -> [ModuleIdent]
getDependenciesForModule (_, decls) = mapMaybe getDependencyName decls
  where getDependencyName (Dependency dependencyName _) = Just dependencyName
        getDependencyName _ = Nothing

loadModule :: ModuleIdent -> PscIde (Either Error T.Text)
loadModule mn = runEitherT $ do
  path <- EitherT . liftIO $ filePathFromModule "json" mn
  EitherT (loadExtern path)
  return ("Loaded extern file at: " <> T.pack path)

filePathFromModule :: String -> ModuleIdent -> IO (Either Error FilePath)
filePathFromModule extension moduleName = do
  cwd <- getCurrentDirectory
  let path = cwd </> "output" </> T.unpack moduleName </> "externs." ++ extension
  ex <- doesFileExist path
  return $
    if ex
    then Right path
    else Left (ModuleFileNotFound moduleName)

-- | Taken from Data.Either.Utils
maybeToEither :: MonadError e m =>
                 e                      -- ^ (Left e) will be returned if the Maybe value is Nothing
              -> Maybe a                -- ^ (Right a) will be returned if this is (Just a)
              -> m a
maybeToEither errorval Nothing = throwError errorval
maybeToEither _ (Just normalval) = return normalval
