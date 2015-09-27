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
completionsFromModules = foldMap completionFromModule
    where
        completionFromModule :: Module -> [Completion]
        completionFromModule (moduleIdent, decls) = mapMaybe (completionFromDecl moduleIdent) decls

completionFromDecl :: ModuleIdent -> ExternDecl -> Maybe Completion
completionFromDecl mi (FunctionDecl name type') = Just (Completion (mi, name, type'))
completionFromDecl mi (DataDecl name kind)      = Just (Completion (mi, name, kind))
completionFromDecl mi (ModuleDecl name _)       = Just (Completion ("module", name, "module"))
completionFromDecl mi _                         = Nothing
