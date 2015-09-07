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
    parseExternDecl,
    typeParse
  ) where

import           Data.Char        (digitToInt)
import           Data.Text        (Text ())
import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import           Text.Parsec
import           Text.Parsec.Text

type ExternParse = Either ParseError [ExternDecl]

data Fixity = Infix | Infixl | Infixr deriving(Show, Eq)

type ModuleIdent = Text
type DeclIdent   = Text
type Type        = Text

data ExternDecl
    = FunctionDecl { functionName :: DeclIdent
                   , functionType :: Type}
    | FixityDeclaration Fixity
                        Int
                        DeclIdent
    | Dependency { dependencyModule :: DeclIdent
                 , dependencyNames  :: Text}
    | ModuleDecl ModuleIdent
                 [DeclIdent]
    | DataDecl DeclIdent
               Text
    deriving (Show,Eq)

-- | Parses an extern file into the ExternDecl format.
readExternFile :: FilePath -> IO ExternParse
readExternFile fp = readExtern <$> (T.lines <$> T.readFile fp)

readExtern :: [Text] -> ExternParse
readExtern strs = mapM (parse parseExternDecl "") clean
  where
    clean = removeComments strs

removeComments :: [Text] -> [Text]
removeComments = filter (not . T.isPrefixOf "--")

parseExternDecl :: Parser ExternDecl
parseExternDecl =
    try parseDependency <|> try parseFixityDecl <|> try parseFunctionDecl <|>
    try parseDataDecl <|> try parseModuleDecl <|>
    return (ModuleDecl "" [])

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
parseDataDecl = do
  string "foreign import data"
  spaces
  (name, kind) <- parseType
  eof
  return $ DataDecl (T.pack name) (T.pack kind)

parseModuleDecl :: Parser ExternDecl
parseModuleDecl = do
  string "module"
  spaces
  name <- many1 (noneOf " ")
  return (ModuleDecl (T.pack name) [])

parseType :: Parser (String, String)
parseType = do
  name <- many1 (noneOf " ")
  spaces
  string "::"
  spaces
  type' <- many1 anyChar
  return (name, type')

typeParse :: Text -> Either Text (Text, Text)
typeParse t = case parse parseType "" t of
  Right (x,y) -> Right (T.pack x, T.pack y)
  Left err -> Left (T.pack (show err))
