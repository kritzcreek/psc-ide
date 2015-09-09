module PureScript.Ide.Types where

import Data.Text (Text())
import Data.Map.Lazy as M

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
