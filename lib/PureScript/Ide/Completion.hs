module PureScript.Ide.Completion
       (getCompletions, moduleFilter, prefixFilter, equalityFilter,
        showCompletion)
       where

import           Data.Maybe           (mapMaybe)
import           Data.Monoid
import           Data.Text            (Text, isPrefixOf)
import           PureScript.Ide.Types


-- | Applies the CompletionFilters and the Matcher to the given Modules
--   and sorts the found Completions according to the Matching Score
getCompletions :: [CompletionFilter] -> Matcher -> [Module] -> [Completion]
getCompletions filters matcher modules =
    matcher $ completionsFromModules (applyFilters filters modules)

applyFilters :: [CompletionFilter] -> [Module] -> [Module]
applyFilters = foldl (.) id

completionsFromModules :: [Module] -> [Completion]
completionsFromModules = concat . fmap completionFromModule

showCompletion :: Completion -> Text
showCompletion (Completion (moduleIdent,ident,type')) =
    "(" <> moduleIdent <> ", " <> ident <> ", " <> type' <> ")"

completionFromModule :: Module -> [Completion]
completionFromModule (moduleIdent, decls) =
    mapMaybe go decls
    where
        go (FunctionDecl name type') = Just (Completion (moduleIdent, name, type'))
        go (DataDecl name kind)      = Just (Completion (moduleIdent, name, kind))
        go _                         = Nothing

-------------------------------- Filters --------------------------------------------

-- | Only keeps the given Modules
moduleFilter :: [ModuleIdent] -> CompletionFilter
moduleFilter moduleIdents =
    filter (flip elem moduleIdents . fst)

-- | Only keeps Identifiers that start with the given prefix
prefixFilter :: Text -> CompletionFilter
prefixFilter = identFilter prefix
  where
    prefix :: ExternDecl -> Text -> Bool
    prefix (FunctionDecl name _) search = search `isPrefixOf` name
    prefix (DataDecl name _) search = search `isPrefixOf` name
    prefix _ _ = False


-- | Only keeps Identifiers that are equal to the search string
equalityFilter :: Text -> CompletionFilter
equalityFilter = identFilter equality
  where
    equality :: ExternDecl -> Text -> Bool
    equality (FunctionDecl name _) prefix = prefix == name
    equality (DataDecl name _) prefix = prefix == name
    equality _ _ = False


identFilter :: (ExternDecl -> Text -> Bool ) -> Text -> CompletionFilter
identFilter predicate search =
    filter (not . null . snd) . fmap filterModuleDecls
  where
    filterModuleDecls :: Module -> Module
    filterModuleDecls (moduleIdent,decls) =
        (moduleIdent, filter (`predicate` search) decls)
