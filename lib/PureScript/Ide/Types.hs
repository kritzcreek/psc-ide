module PureScript.Ide.Types where

import           Control.Monad
import           Data.Aeson
import           Data.Map.Lazy as M
import           Data.Text     (Text ())

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
    | Dependency { dependencyModule :: DeclIdent
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

instance FromJSON Completion where
  parseJSON (Object o) = do
    m <- o .: "module"
    d <- o .: "identifier"
    t <- o .: "type"
    return $ Completion (m, d, t)
  parseJSON _ = mzero

instance ToJSON Completion where
  toJSON (Completion (m, d, t)) =
    object ["module" .= m, "identifier" .= d, "type" .= t]

newtype Filter  = Filter ([Module] -> [Module])
newtype Matcher = Matcher ([Completion] -> [Completion])
