{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module PureScript.Ide where

import           Control.Monad.Except
import           Control.Monad.State.Lazy (StateT (..), get, modify)
import qualified Data.Map.Lazy            as M
import           Data.Maybe               (mapMaybe)
import           Data.Monoid
import           Data.Text                (Text ())
import qualified Data.Text  as T
import           PureScript.Ide.Completion
import           PureScript.Ide.Externs
import           PureScript.Ide.Pursuit
import           PureScript.Ide.Err
import           PureScript.Ide.Types
import           System.FilePath
import           System.Directory

type PscIde = StateT PscState IO

getAllDecls :: PscIde [ExternDecl]
getAllDecls = concat . pscStateModules <$> get

getAllModules :: PscIde [Module]
getAllModules = M.toList . pscStateModules <$> get

findCompletions :: [Filter] -> Matcher -> PscIde [Completion]
findCompletions filters matcher =
    getCompletions filters matcher <$> getAllModules

findType :: DeclIdent -> [Filter] -> PscIde [Completion]
findType search filters =
  getExactMatches search filters <$> getAllModules

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
unsafeModuleFromDecls decls@(ModuleDecl name _ : _) = (name, decls)
unsafeModuleFromDecls _ =
    error "An externs File didn't start with a module declaration"

unsafeStateFromDecls :: [[ExternDecl]] -> PscState
unsafeStateFromDecls = PscState . M.fromList . fmap unsafeModuleFromDecls

printModules :: PscIde [ModuleIdent]
printModules = M.keys . pscStateModules <$> get

-- | The first argument is a set of modules to load. The second argument
--   denotes modules for which to load dependencies
loadModulesAndDeps :: [ModuleIdent] -> [ModuleIdent] -> PscIde (Either Err T.Text)
loadModulesAndDeps mods deps = do
    r1 <- mapM loadModule mods
    r2 <- mapM loadModuleDependencies deps
    return $
        liftM2 (<>) (T.concat <$> sequence r1) (T.concat <$> sequence r2)

loadModuleDependencies :: ModuleIdent -> PscIde (Either Err T.Text)
loadModuleDependencies moduleName = do
    _ <- loadModule moduleName
    mDeps <- getDependenciesForModule moduleName
    case mDeps of
        Just deps -> do
            mapM_ loadModule deps
            return (Right ("Dependencies for " <> moduleName <> " loaded."))
        Nothing -> return (Left (ModuleNotFound moduleName))

loadModule :: ModuleIdent -> PscIde (Either Err T.Text)
loadModule mn = do
    path <- liftIO $ filePathFromModule mn
    case path of
        Right p  -> loadExtern p >> return (Right $ "Loaded extern file at: " <> T.pack p)
        Left _ -> return (Left . GeneralErr $ "Could not load module " <> T.unpack mn)

filePathFromModule :: ModuleIdent -> IO (Either T.Text FilePath)
filePathFromModule moduleName = do
    cwd <- getCurrentDirectory
    let path = cwd </> "output" </> T.unpack moduleName </> "externs.purs"
    ex <- doesFileExist path
    return $
        if ex
            then Right path
            else Left ("Extern file for module " <> moduleName <>" could not be found")

-- | Taken from Data.Either.Utils
maybeToEither :: MonadError e m =>
                 e                      -- ^ (Left e) will be returned if the Maybe value is Nothing
              -> Maybe a                -- ^ (Right a) will be returned if this is (Just a)
              -> m a
maybeToEither errorval Nothing = throwError errorval
maybeToEither _ (Just normalval) = return normalval
