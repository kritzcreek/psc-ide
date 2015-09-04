module Purescript.Ide.Command (parseCommand, Command (..)) where

import           Data.Text        (Text)
import qualified Data.Text        as T
import           Text.Parsec
import           Text.Parsec.Text

data Command =
  TypeLookup Text
  | Completion Text
  | Load Text
  | Print
  | Cwd
  | Quit
    deriving(Show, Eq)

parseCommand :: Text -> Either ParseError Command
parseCommand = parse parseCommand' ""

parseCommand' :: Parser Command
parseCommand' =
    (string "print" >> return Print) <|>
    try (string "cwd" >> return Cwd) <|>
    (string "quit" >> return Quit) <|>
    parseTypeLookup <|> try parseCompletion <|> parseLoad

parseTypeLookup :: Parser Command
parseTypeLookup = do
    string "typeLookup"
    spaces
    ident <- many1 anyChar
    return (TypeLookup (T.pack ident))

parseCompletion :: Parser Command
parseCompletion = do
    string "complete"
    spaces
    stub <- many1 anyChar
    return (Completion (T.pack stub))

parseLoad :: Parser Command
parseLoad = do
    string "load"
    spaces
    module' <- many1 anyChar
    return (Load (T.pack module'))
