module PureScript.Ide.Completion
       (getCompletions, getExactMatches)
       where

import           Data.Maybe            (mapMaybe)
import           PureScript.Ide.Filter
import           PureScript.Ide.Types

-- | Applies the CompletionFilters and the Matcher to the given Modules
--   and sorts the found Completions according to the Matching Score
getCompletions :: [Filter] -> Matcher -> [Module] -> [Completion]
getCompletions filters (Matcher matcher) modules =
    matcher $ completionsFromModules (applyFilters filters modules)

getExactMatches :: DeclIdent -> [Filter] -> [Module] -> [Completion]
getExactMatches search filters modules =
    completionsFromModules $
    applyFilters (equalityFilter search : filters) modules

applyFilters :: [Filter] -> [Module] -> [Module]
applyFilters = foldl (\f (Filter g) -> f . g) id

completionsFromModules :: [Module] -> [Completion]
completionsFromModules = concat . fmap completionFromModule

completionFromModule :: Module -> [Completion]
completionFromModule (moduleIdent, decls) =
    mapMaybe go decls
    where
        go (FunctionDecl name type') = Just (Completion (moduleIdent, name, type'))
        go (DataDecl name kind)      = Just (Completion (moduleIdent, name, kind))
        go _                         = Nothing

