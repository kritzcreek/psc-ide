{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module PureScript.Ide.Externs
  (
    ExternParse,
    ExternDecl(..),
    ModuleIdent,
    DeclIdent,
    Type,
    Fixity(..),
    readExternFile,
    parseExtern,
    parseExternDecl,
    typeParse
  ) where

import           Data.Char            (digitToInt)
import           Data.Text            (Text ())
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           PureScript.Ide.Types
import           Text.Parsec
import           Text.Parsec.Text

type ExternParse = Either ParseError [ExternDecl]

-- | Parses an extern file into the ExternDecl format.
readExternFile :: FilePath -> IO ExternParse
readExternFile fp = readExtern . T.lines <$> T.readFile fp

readExtern :: [Text] -> ExternParse
readExtern strs = mapM parseExtern clean
  where
    clean = removeComments strs

removeComments :: [Text] -> [Text]
removeComments = filter (not . T.isPrefixOf "--")

parseExtern :: Text -> Either ParseError ExternDecl
parseExtern = parse parseExternDecl ""

parseExternDecl :: Parser ExternDecl
parseExternDecl =
    try parseDependency <|> try parseFixityDecl <|> try parseFunctionDecl <|>
    try parseDataDecl <|>
    try parseModuleDecl <|>
    return (ModuleDecl "" [])

parseDependency :: Parser ExternDecl
parseDependency =
    try parseQualifiedImport <|> try parseHidingImport <|> parseSimpleImport

parseSimpleImport :: Parser ExternDecl
parseSimpleImport = do
    string "import"
    module' <- identifier
    char '('
    names <- sepBy identifier (char ',')
    char ')'
    eof
    return $ Dependency module' names

parseHidingImport :: Parser ExternDecl
parseHidingImport = do
    string "import"
    module' <- identifier
    string "hiding"
    spaces
    char '('
    hiddenNames <- sepBy identifier (char ',')
    char ')'
    return $ Dependency module' []

parseQualifiedImport :: Parser ExternDecl
parseQualifiedImport = do
    string "import qualified"
    module' <- identifier
    string "as"
    qualifier <- identifier
    return $ Dependency module' []

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
    (try (string "infixr") >> return Infixr) <|>
    (try (string "infixl") >> return Infixl) <|>
    (string "infix" >> return Infix)

parseFunctionDecl :: Parser ExternDecl
parseFunctionDecl = do
    string "foreign import"
    spaces
    (name, type') <- parseType
    eof
    return (FunctionDecl (T.pack name) (T.pack type'))

parseDataDecl :: Parser ExternDecl
parseDataDecl = parseDataDecl' <|> parseForeignDataDecl

parseDataDecl' :: Parser ExternDecl
parseDataDecl' = do
  string "data"
  ident <- identifier
  kind <- many anyChar
  return (DataDecl ident (T.pack kind))

parseForeignDataDecl :: Parser ExternDecl
parseForeignDataDecl = do
  string "foreign import data"
  spaces
  (name, kind) <- parseType
  eof
  return $ DataDecl (T.pack name) (T.pack kind)

parseModuleDecl :: Parser ExternDecl
parseModuleDecl = do
  string "module"
  name <- identifier
  exports <- identifierList
  return (ModuleDecl name exports)

parseType :: Parser (String, String)
parseType = do
  name <- identifier
  string "::"
  spaces
  type' <- many1 anyChar
  return (T.unpack name, type')

typeParse :: Text -> Either Text (Text, Text)
typeParse t = case parse parseType "" t of
  Right (x,y) -> Right (T.pack x, T.pack y)
  Left err -> Left (T.pack (show err))

identifierList :: Parser [Text]
identifierList = between (char '(') (char ')') (sepBy identifier (char ','))

identifier :: Parser Text
identifier = do
    spaces
    ident <-
        -- necessary for being able to parse the following ((++), concat)
        between (char '(') (char ')') (many1 (noneOf ", )")) <|>
        many1 (noneOf ", )")
    spaces
    return (T.pack ident)
