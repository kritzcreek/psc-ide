{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module PureScript.Ide.Command
       (parseCommand, Command(..), Level(..)) where

import           Data.Text              (Text)
import qualified Data.Text              as T
import           PureScript.Ide.Err
import           PureScript.Ide.Externs (DeclIdent, ModuleIdent)
import           Text.Parsec
import           Text.Parsec.Text

data Level
    = File
    | Project
    | Pursuit
    deriving (Show,Eq)

data Command
    = TypeLookup DeclIdent
    | Complete Text Level (Maybe [ModuleIdent])
    | Load ModuleIdent
    | LoadDependencies ModuleIdent
    | Print
    | Cwd
    | Quit
    deriving (Show,Eq)

parseCommand :: T.Text -> Either Err Command
parseCommand t = first (`ParseErr` "Parse error") (parse parseCommand' "" t)

parseCommand' :: Parser Command
parseCommand' =
    (string "print" >> return Print) <|> try (string "cwd" >> return Cwd) <|>
    (string "quit" >> return Quit) <|>
    try parseTypeLookup <|>
    try parseComplete <|>
    try parseLoad <|>
    parseLoadDependencies

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
    modules <- parseModuleList <|> return Nothing
    return (Complete (T.pack stub) level modules)

parseModuleList :: Parser (Maybe [Text])
parseModuleList = do
    spaces
    string "using"
    spaces
    idents <- sepBy1 (many (noneOf ", ")) (spaces >> string "," >> spaces)
    spaces
    return $ Just (T.pack <$> idents)


parseLoad :: Parser Command
parseLoad = do
    string "load"
    spaces
    module' <- many1 anyChar
    return (Load (T.pack module'))

parseLoadDependencies :: Parser Command
parseLoadDependencies = do
    string "dependencies"
    spaces
    module' <- many1 anyChar
    return (LoadDependencies (T.pack module'))

parseLevel :: Parser Level
parseLevel =
    (string "File" >> return File) <|>
    (try (string "Project") >> return Project) <|>
    (string "Pursuit" >> return Pursuit)
