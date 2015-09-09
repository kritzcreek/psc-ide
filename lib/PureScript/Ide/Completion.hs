module PureScript.Ide.Completion where

import           Data.Maybe           (mapMaybe)
import           Data.Text            (Text, isPrefixOf)
import           PureScript.Ide.Types

type Completion = (DeclIdent, ModuleIdent, Type)

type CompletionFilter = [Module] -> [Module]

getCompletions :: [CompletionFilter] -> [Module] -> [Completion]
getCompletions filters modules =
    completionsFromModules (applyFilters filters modules)

applyFilters :: [CompletionFilter] -> [Module] -> [Module]
applyFilters = foldl (.) id

completionsFromModules :: [Module] -> [Completion]
completionsFromModules = concat . fmap completionFromModule

completionFromModule :: Module -> [Completion]
completionFromModule (ident, decls) =
  mapMaybe go decls
  where
    go (FunctionDecl name type') = Just (ident, name, type')
    go (DataDecl name kind)      = Just (ident, name, kind)
    go _                         = Nothing

prefixFilter :: Text -> CompletionFilter
prefixFilter prefix =
  filter (not . null . snd) . fmap filterModuleDecls
  where
    filterModuleDecls :: Module -> Module
    filterModuleDecls (moduleIdent, decls) = (moduleIdent, filter matches decls)

    matches :: ExternDecl -> Bool
    matches (FunctionDecl name _) = prefix `isPrefixOf` name
    matches (DataDecl name _) = prefix `isPrefixOf` name
    matches _ = False

moduleFilter :: [ModuleIdent] -> CompletionFilter
moduleFilter moduleIdents =
  filter (flip elem moduleIdents . fst)
