{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Purescript.Ide.Command
       (parseCommand, Command(..), Level(..)) where

import           Data.Text        (Text)
import qualified Data.Text        as T
import           Text.Parsec
import           Text.Parsec.Text

data Level
    = File
    | Project
    | Pursuit
    deriving (Show,Eq)

data Command
    = TypeLookup Text
    | Complete Text Level
    | Load Text
    | Print
    | Cwd
    | Quit
    deriving (Show,Eq)

parseCommand :: Text -> Either ParseError Command
parseCommand = parse parseCommand' ""

parseCommand' :: Parser Command
parseCommand' =
    (string "print" >> return Print) <|>
    try (string "cwd" >> return Cwd) <|>
    (string "quit" >> return Quit) <|>
    parseTypeLookup <|> try parseComplete <|> parseLoad

parseTypeLookup :: Parser Command
parseTypeLookup = do
    string "typeLookup"
    spaces
    ident <- many1 anyChar
    return (TypeLookup (T.pack ident))

parseComplete :: Parser Command
parseComplete = do
    string "complete"
    spaces
    stub <- many1 (noneOf " ")
    spaces
    level <- parseLevel
    return (Complete (T.pack stub) level)

parseLoad :: Parser Command
parseLoad = do
    string "load"
    spaces
    module' <- many1 anyChar
    return (Load (T.pack module'))

parseLevel :: Parser Level
parseLevel =
    (string "File" >> return File) <|>
    (try (string "Project") >> return Project) <|>
    (string "Pursuit" >> return Pursuit)
