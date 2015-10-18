{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module PureScript.Ide.Types where

import           Control.Monad
import           Data.Aeson
import           Data.Map.Lazy                        as M
import           Data.Maybe                           (maybeToList)
import           Data.Monoid
import           Data.Text                            (Text ())
import qualified Language.PureScript.AST.Declarations as D
import qualified Language.PureScript.Names            as N

type ModuleIdent = Text
type DeclIdent   = Text
type Type        = Text

data Fixity = Infix | Infixl | Infixr deriving(Show, Eq)

data ExternDecl
    = FunctionDecl { functionName :: DeclIdent
                   , functionType :: Type}
    | FixityDeclaration Fixity
                        Int
                        DeclIdent
    | Dependency { dependencyModule :: ModuleIdent
                 , dependencyNames  :: [Text]}
    | ModuleDecl ModuleIdent
                 [DeclIdent]
    | DataDecl DeclIdent
               Text
    deriving (Show,Eq)

type Module = (ModuleIdent, [ExternDecl])

data PscState = PscState
    { pscStateModules :: M.Map Text [ExternDecl]
    } deriving (Show,Eq)

emptyPscState :: PscState
emptyPscState = PscState M.empty

newtype Completion =
    Completion (ModuleIdent, DeclIdent, Type)
    deriving (Show,Eq)

data ModuleImport = ModuleImport {
  importModuleName :: ModuleIdent,
  importType       :: D.ImportDeclarationType,
  importQualifier  :: Maybe Text} deriving(Show)

instance ToJSON ModuleImport where
  toJSON (ModuleImport mn D.Implicit qualifier) =
    object $  ["module" .= mn
              , "importType" .= ("implicit" :: Text)
              ] ++ fmap (\x -> "qualifier" .= x) (maybeToList qualifier)
  toJSON (ModuleImport mn (D.Explicit refs) _) =
    object ["module" .= mn
           , "importType" .= ("explicit" :: Text)
           , "identifiers" .= (identifierFromDeclarationRef <$> refs)]
  toJSON (ModuleImport mn (D.Hiding refs) _) =
    object ["module" .= mn
           , "importType" .= ("hiding" :: Text)
           , "identifiers" .= (identifierFromDeclarationRef <$> refs)]

identifierFromDeclarationRef :: D.DeclarationRef -> String
identifierFromDeclarationRef (D.TypeRef name _) = N.runProperName name
identifierFromDeclarationRef (D.ValueRef ident) = N.runIdent ident
identifierFromDeclarationRef (D.TypeClassRef name) = N.runProperName name
identifierFromDeclarationRef _ = ""

instance FromJSON Completion where
    parseJSON (Object o) = do
        m <- o .: "module"
        d <- o .: "identifier"
        t <- o .: "type"
        return $ Completion (m, d, t)
    parseJSON _ = mzero

instance ToJSON Completion where
    toJSON (Completion (m,d,t)) =
        object ["module" .= m, "identifier" .= d, "type" .= t]

data Success =
  CompletionResult [Completion]
  | TextResult Text
  | PursuitResult [PursuitResponse]
  | ImportList [ModuleImport]
  deriving(Show)

encodeSuccess :: (ToJSON a) => a -> Value
encodeSuccess res =
    object ["resultType" .= ("success" :: Text), "result" .= res]

instance ToJSON Success where
  toJSON (CompletionResult cs) = encodeSuccess cs
  toJSON (TextResult t) = encodeSuccess t
  toJSON (PursuitResult resp) = encodeSuccess resp
  toJSON (ImportList decls) = encodeSuccess decls

newtype Filter = Filter (Endo [Module]) deriving(Monoid)
newtype Matcher = Matcher (Endo [Completion]) deriving(Monoid)

newtype PursuitQuery = PursuitQuery Text
                     deriving (Show, Eq)

data PursuitSearchType = Package | Identifier
                       deriving (Show, Eq)

instance FromJSON PursuitSearchType where
  parseJSON (String t) = case t of
    "package"    -> return Package
    "completion" -> return Identifier
    _            -> mzero
  parseJSON _ = mzero

instance FromJSON PursuitQuery where
  parseJSON o = fmap PursuitQuery (parseJSON o)

data PursuitResponse
    = ModuleResponse { moduleResponseName    :: Text
                     , moduleResponsePackage :: Text}
    | DeclarationResponse { declarationResponseType    :: Text
                          , declarationResponseModule  :: Text
                          , declarationResponseIdent   :: Text
                          , declarationResponsePackage :: Text
                          }
    deriving (Show,Eq)

instance ToJSON PursuitResponse where
    toJSON (ModuleResponse{moduleResponseName = name, moduleResponsePackage = package}) =
        object ["module" .= name, "package" .= package]
    toJSON (DeclarationResponse{..}) =
        object
            [ "module"  .= declarationResponseModule
            , "ident"   .= declarationResponseIdent
            , "type"    .= declarationResponseType
            , "package" .= declarationResponsePackage]
