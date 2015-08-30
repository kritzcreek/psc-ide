{-# LANGUAGE OverloadedStrings #-}

module Purescript.Ide where

import Data.Text (Text())
import Data.Char (digitToInt)
import Data.Monoid
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Parsec
import Text.Parsec.Text

type ExternParse = Either ParseError [ExternDecl]

data Fixity = Infix | Infixl | Infixr deriving(Show, Eq)

data ExternDecl =
  FunctionDecl {
    functionName :: Text,
    functionType :: Text
    }
  | FixityDeclaration Fixity Int Text
  | Dependency {
    dependencyModule :: Text,
    dependencyNames  :: Text
    }
  | ModuleDecl Text [Text]
  deriving(Show, Eq)

readExtern:: [Text] -> ExternParse
readExtern strs = sequence $ map (parse parseExternDecl "") clean
  where clean = removeComments strs

readExternFile :: FilePath -> IO ExternParse
readExternFile fp = readExtern <$> (T.lines <$> T.readFile fp)

removeComments :: [Text] -> [Text]
removeComments = filter (not . T.isPrefixOf "--")

parseExternDecl :: Parser ExternDecl
parseExternDecl =
  try parseDependency
  <|> try parseFixityDecl
  <|> try parseFunctionDecl
  <|> return (ModuleDecl "" [])

parseDependency :: Parser ExternDecl
parseDependency = do
  string "import "
  module' <- many1 (noneOf " ")
  spaces
  names <- many1 anyChar
  eof
  return $ Dependency (T.pack module') (T.pack names)

parseFixityDecl :: Parser ExternDecl
parseFixityDecl = do
  fixity <- parseFixity
  spaces
  priority <- digitToInt <$> digit
  spaces
  symbol <- many1 anyChar
  eof
  return (FixityDeclaration fixity priority (T.pack symbol))

parseFixity :: Parser Fixity
parseFixity =
  (try (string "infixr") >> return Infixr)
  <|> (try (string "infixl") >> return Infixl)
  <|> (string "infix" >> return Infix)

parseFunctionDecl :: Parser ExternDecl
parseFunctionDecl = do
  string "foreign import"
  spaces
  name <- many1 (noneOf " ")
  spaces
  string "::"
  spaces
  type' <- many1 anyChar
  eof
  return (FunctionDecl (T.pack name) (T.pack type'))

findTypeForName :: [ExternDecl] -> Text -> Maybe Text
findTypeForName decls search = getFirst $ fold (map (First . go) decls)
  where
    go :: ExternDecl -> Maybe Text
    go decl = case decl of
       FunctionDecl n t -> if search == n
                           then Just t
                           else Nothing
       _                -> Nothing

-- Utilities for testing in ghci
findTypeForName' :: Text -> IO (Maybe Text)
findTypeForName' search = do
  exts <- externsFile
  case exts of
    Left x -> print x >> return Nothing
    Right decls -> return $ findTypeForName decls search

externsFile :: IO (Either ParseError [ExternDecl])
externsFile = readExternFile "/home/creek/sandbox/psc-ide/externs.purs"
