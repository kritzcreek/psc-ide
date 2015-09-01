module Main where

import           Control.Monad       (forever)
import           Control.Monad.State
import           Control.Monad.Trans
import qualified Data.Text.IO        as T
import           Options.Applicative
import           Purescript.Ide

data Options = Options
    { externs :: [FilePath]
    }

options :: Parser Options
options =
    Options <$>
    many
        (strArgument
             (metavar "FILE" <> help "Extern File to use for searching"))

main :: IO ()
main = execParser opts >>= pscIde
  where
    opts =
        info
            (helper <*> options)
            (fullDesc <> progDesc "IDE Support for the PureScript language" <>
             header "psc-ide - wat?")

pscIde :: Options -> IO ()
pscIde opts = do
    exts <- sequence <$> mapM readExternFile (externs opts)
    case exts of
        Right exts' -> repl exts'
        Left err ->
            print $
            "There was an error when trying to parse the extern files: " <>
            show err

repl :: [[ExternDecl]] -> IO ()
repl decls =
    evalStateT
        (forever (liftIO getLine >>= interpret))
        (PscState (map unsafeModuleFromDecls decls))
  where
    interpret :: String -> PscIde ()
    interpret "typeLookup" = do
        liftIO $ putStrLn "Insert the function name to look for:"
        fname <- liftIO T.getLine
        ftype <- findTypeForName fname
        liftIO $ putStrLn $ maybe "No function found." show ftype
    interpret "completion" = do
        liftIO $ putStrLn "Insert the stub to lookup completions for:"
        stub <- liftIO T.getLine
        liftIO . print =<< findCompletion stub
    interpret "load" = do
        liftIO $ putStrLn "Insert the filepath to the extern file to import"
        fp <- liftIO getLine
        loadModule fp
    interpret "print" = printModules
    interpret _ = liftIO $ putStrLn "Enter one of: [typeLookup, completion, load, print]"

-- readExts :: [FilePath] -> IO ExternParse
-- readExts fps = do
--     exts <- mapM readExternFile fps
--     return $ fmap concat (sequence exts)
