module Main where

import Purescript.Ide
import qualified Data.Text.IO as T
import Options.Applicative

data Options =
  Options {
    externs :: [FilePath]
    }

options :: Parser Options
options = Options <$>
           some (strArgument ( metavar "FILE" <> help "Extern File to use for searching" ))


main :: IO ()
main = execParser opts >>= pscIde
  where
    opts = info (helper <*> options)
           ( fullDesc
           <> progDesc "IDE Support for the PureScript language"
           <> header "psc-ide - wat?")

pscIde :: Options -> IO ()
pscIde opts = do
  exts <- readExts (externs opts)
  putStrLn "Insert the function name to look for:"
  fname <- T.getLine
  print $ findTypeForName <$> exts <*> pure fname


readExts :: [FilePath] -> IO ExternParse
readExts fps = do
  exts <- mapM readExternFile fps
  return $ fmap concat (sequence exts)
