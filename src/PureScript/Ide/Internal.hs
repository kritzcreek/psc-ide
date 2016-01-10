{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TemplateHaskell #-}
module PureScript.Ide.Internal where


import           Control.Concurrent.STM
import           "monad-logger" Control.Monad.Logger
import           Control.Monad.Reader
import           PureScript.Ide.State
import           PureScript.Ide.Externs
import           PureScript.Ide.Types
import           PureScript.Ide.CaseSplit
import Control.Lens (over, _1, _2)
import qualified Data.Text as T
import Language.PureScript.Externs
import Language.PureScript.Pretty

---- Testing stuff

env ss = PscEnvironment
        {
          envStateVar = ss
        , envConfiguration = Configuration "" True
        }

runWithExternsFile
  :: FilePath -> ReaderT PscEnvironment (LoggingT IO) b -> IO b
runWithExternsFile fp = runWithExternsFiles [fp]

runWithExternsFiles
  :: [FilePath] -> ReaderT PscEnvironment (LoggingT IO) b -> IO b
runWithExternsFiles fps f = do
  serverState <- newTVarIO emptyPscState
  runStdoutLoggingT $ flip runReaderT (env serverState) $ do
    efs <- liftIO $ traverse readExternFile fps
    _ <- either
      (const (error "parsing the externs failed"))
      (traverse insertModule)
      (sequence efs)
    f

runConway = runWithExternsFile   "../conway-purescript/output/MyModule/externs.json"
runDataList = runWithExternsFile "../conway-purescript/output/Data.List/externs.json"
runPrelude = runWithExternsFile  "../conway-purescript/output/Prelude/externs.json"
runEither = runWithExternsFile   "../conway-purescript/output/Either/externs.json"
runHalogen = runWithExternsFile  "../conway-purescript/output/Halogen.Query.StateF/externs.json"

run = runWithExternsFiles [
 "../conway-purescript/output/MyModule/externs.json"
 , "../conway-purescript/output/Data.List/externs.json"
 , "../conway-purescript/output/Prelude/externs.json"
 , "../conway-purescript/output/Data.Either/externs.json"
 , "../conway-purescript/output/Halogen.Query.StateF/externs.json"
 ]

testing = runWithExternsFile "../conway-purescript/output/MyModule/externs.json" $ do
  ctors <- findConstructors "A"
  pure (map (over _1 (fmap ( T.strip . T.pack . prettyPrintType)) . over _2 prettyPrintType . splitType . edDataCtorType) ctors)
