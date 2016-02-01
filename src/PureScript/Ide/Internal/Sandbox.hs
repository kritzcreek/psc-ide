{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TemplateHaskell #-}
module PureScript.Ide.Internal.Sandbox where


import           Control.Concurrent.STM
import           "monad-logger" Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Except
import           PureScript.Ide.State
import           PureScript.Ide.Externs
import           PureScript.Ide.Types
import PureScript.Ide.Error
import Control.Lens (over, _1, _2)
import qualified Data.Text as T
import Language.PureScript.Externs
import Language.PureScript.Pretty
import Language.PureScript.Names
import qualified Data.Map as M
import           PureScript.Ide.CaseSplit
---- Testing stuff

env ss = PscEnvironment
        {
          envStateVar = ss
        , envConfiguration = Configuration "" True
        }

type PscM = ReaderT PscEnvironment (ExceptT PscIdeError (LoggingT IO))

runWithExternsFile
  :: FilePath -> PscM b -> IO (Either PscIdeError b)
runWithExternsFile fp = runWithExternsFiles [fp]

runWithExternsFiles
  :: [FilePath] -> PscM b -> IO (Either PscIdeError b)
runWithExternsFiles fps f = do
  serverState <- newTVarIO emptyPscState
  runStdoutLoggingT $ runExceptT $ flip runReaderT (env serverState) $ do
    efs <- liftIO $ runExceptT $ traverse readExternFile fps
    _ <- either
      (const (error "parsing the externs failed"))
      (traverse insertModule)
      efs
    f

runConway = runWithExternsFile   "../conway-purescript/output/MyModule/externs.json"
runDataList = runWithExternsFile "../conway-purescript/output/Data.List/externs.json"
runPrelude = runWithExternsFile  "../conway-purescript/output/Prelude/externs.json"
runEither = runWithExternsFile   "../conway-purescript/output/Data.Either/externs.json"
runHalogen = runWithExternsFile  "../conway-purescript/output/Halogen.Query.StateF/externs.json"

run = runWithExternsFiles [
 "../conway-purescript/output/MyModule/externs.json"
 , "../conway-purescript/output/Data.List/externs.json"
 , "../conway-purescript/output/Prelude/externs.json"
 , "../conway-purescript/output/Data.Either/externs.json"
 , "../conway-purescript/output/Halogen.Query.StateF/externs.json"
 ]

-- testing :: (PscIde m) => m ()
-- testing = do
--   (Just ef) <- M.lookup (moduleNameFromString "Data.Either") <$> getExternFiles
--   liftIO $ print $ getCtorArgs ef (ProperName "Either")
