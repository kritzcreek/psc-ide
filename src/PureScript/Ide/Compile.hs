{-# LANGUAGE RecordWildCards #-}
module PureScript.Ide.Compile where

import           Data.Monoid
import           System.Process
import Data.List (intercalate)

data CompileOptions =
  CompileOptions
  { compileSourceFiles :: Maybe [FilePath]
  , compileDependencyFiles :: Maybe [FilePath]
  , compileCwd :: Maybe FilePath
  , compileForce :: Bool
  }

type CompileResult = Bool

conwayCompileOptions :: CompileOptions
conwayCompileOptions =
  CompileOptions Nothing Nothing (Just "/home/creek/Documents/conway-purescript") True

compile :: CompileOptions -> IO CompileResult
compile CompileOptions{..} = do
  let process = shell $ "pulp build" <> srcOption <> depOption <> forceOption <> " --json-errors"
  (exitCode, out, err) <- readCreateProcessWithExitCode (process {cwd = compileCwd}) ""
  print $ "Command:" <> "pulp build" <> srcOption <> depOption <> forceOption <> " --json-errors"
  print $ "ExitCode: " <> show exitCode
  print $ "Stdout: " <> out
  print $ "Stderr: " <> err
  pure True
  where srcOption = case compileSourceFiles of
          Nothing -> ""
          Just xs -> " --src-path=" <> intercalate ", " xs

        depOption = case compileDependencyFiles of
          Nothing -> ""
          Just xs -> " --dependency-path=" <> intercalate ", " xs

        forceOption = if compileForce then " --force" else ""
