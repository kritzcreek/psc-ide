module Main where

import qualified Data.Text.IO        as T
import           Options.Applicative
import           Purescript.Ide

data Options = Options
    { externs :: [FilePath]
    }

options :: Parser Options
options =
    Options <$>
    some
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
    exts <- readExts (externs opts)
    case exts of
        Right exts' -> do
            putStrLn "Insert the function name to look for:"
            fname <- T.getLine
            putStrLn $
                maybe "No function found." show (findTypeForName exts' fname)
        Left err ->
            print $
            "There was an error when trying to parse the extern files: " <>
            show err

readExts :: [FilePath] -> IO ExternParse
readExts fps = do
    exts <- mapM readExternFile fps
    return $ fmap concat (sequence exts)
