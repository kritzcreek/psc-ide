{-# LANGUAGE OverloadedStrings #-}

module Purescript.Ide
  (
    emptyPscState,
    readExternFile,
    findTypeForName,
    findCompletion,
    loadModule,
    printModules,
    parseCommand,
    unsafeStateFromDecls,
    ExternParse,
    ExternDecl,
    PscIde,
    PscState(..),
    Command(..)
  ) where

import           Control.Monad.State.Lazy (StateT (..), get, modify)
import           Control.Monad.Trans
import           Data.Char                (digitToInt)
import           Data.Foldable
import qualified Data.Map.Lazy            as M
import           Data.Maybe               (mapMaybe)
import           Data.Monoid
import           Data.Text                (Text ())
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Text.Parsec
import           Text.Parsec.Text

type ExternParse = Either ParseError [ExternDecl]

type Module = (Text, [ExternDecl])


data PscState = PscState
    { pscStateModules :: M.Map Text [ExternDecl]
    } deriving (Show,Eq)

emptyPscState :: PscState
emptyPscState = PscState M.empty


type PscIde = StateT PscState IO

data Fixity = Infix | Infixl | Infixr deriving(Show, Eq)

data ExternDecl
    = FunctionDecl { functionName :: Text
                   , functionType :: Text}
    | FixityDeclaration Fixity
                        Int
                        Text
    | Dependency { dependencyModule :: Text
                 , dependencyNames  :: Text}
    | ModuleDecl Text
                 [Text]
    | DataDecl Text
               Text
    deriving (Show,Eq)

getAllDecls :: PscIde [ExternDecl]
getAllDecls = return . concat =<< fmap pscStateModules get

-- | Given a set of ExternDeclarations finds the type for a given function
--   name and returns Nothing if the functionName can not be matched
findTypeForName :: Text -> PscIde (Maybe Text)
findTypeForName search = do
  decls <- getAllDecls
  return $ getFirst $ fold (map (First . go) decls)
  where
    go :: ExternDecl -> Maybe Text
    go decl =
        case decl of
            FunctionDecl n t ->
                if search == n
                    then Just t
                    else Nothing
            _ -> Nothing

-- | Given a set of ExternDeclarations finds all the possible completions.
--   Doesn't do any fancy flex matching. Just prefix search
findCompletion :: Text -> PscIde [Text]
findCompletion stub = fmap (mapMaybe go) getAllDecls
  where
    matches name =
        if stub `T.isPrefixOf` name
            then Just name
            else Nothing
    go :: ExternDecl -> Maybe Text
    go (FunctionDecl name _) = matches name
    go (DataDecl name _) = matches name
    go _ = Nothing

loadModule :: FilePath -> PscIde ()
loadModule fp = do
    parseResult <- liftIO $ readExternFile fp
    case parseResult of
        Right decls ->
            let (name, decls') = unsafeModuleFromDecls decls
            in modify
                   (\x ->
                         x
                         { pscStateModules = M.insert
                               name
                               decls'
                               (pscStateModules x)
                         })
        Left _ -> liftIO $ putStrLn "The module could not be parsed"

unsafeModuleFromDecls :: [ExternDecl] -> Module
unsafeModuleFromDecls (ModuleDecl name _ : decls) = (name, decls)

unsafeStateFromDecls :: [[ExternDecl]] -> PscState
unsafeStateFromDecls = PscState . M.fromList . fmap unsafeModuleFromDecls

printModules :: PscIde [Text]
printModules = return . M.keys . pscStateModules =<< get

-- | Parses an extern file into the ExternDecl format.
readExternFile :: FilePath -> IO ExternParse
readExternFile fp = readExtern <$> (T.lines <$> T.readFile fp)

readExtern:: [Text] -> ExternParse
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
    name <- many1 (noneOf " ")
    spaces
    string "::"
    spaces
    type' <- many1 anyChar
    eof
    return (FunctionDecl (T.pack name) (T.pack type'))

parseDataDecl :: Parser ExternDecl
parseDataDecl = do
  string "foreign import data"
  spaces
  name <- many1 (noneOf " ")
  spaces
  string "::"
  spaces
  kind <- many1 anyChar
  eof
  return $ DataDecl (T.pack name) (T.pack kind)

parseModuleDecl :: Parser ExternDecl
parseModuleDecl = do
  string "module"
  spaces
  name <- many1 (noneOf " ")
  return (ModuleDecl (T.pack name) [])

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
    string "completion"
    spaces
    stub <- many1 anyChar
    return (Completion (T.pack stub))

parseLoad :: Parser Command
parseLoad = do
    string "load"
    spaces
    module' <- many1 anyChar
    return (Load (T.pack module'))
