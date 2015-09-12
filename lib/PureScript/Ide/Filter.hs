module PureScript.Ide.Filter
       (moduleFilter, prefixFilter, equalityFilter, dependencyFilter) where

import           Data.Maybe           (mapMaybe, listToMaybe)
import           Data.Text            (Text, isPrefixOf)
import           PureScript.Ide.Types


-- | Only keeps the given Modules
moduleFilter :: [ModuleIdent] -> Filter
moduleFilter =
    Filter . moduleFilter'

moduleFilter' :: [ModuleIdent] -> [Module] -> [Module]
moduleFilter' moduleIdents = filter (flip elem moduleIdents . fst)

-- | Only keeps the given Modules and all of their dependencies
dependencyFilter :: [ModuleIdent] -> Filter
dependencyFilter = Filter . dependencyFilter'

dependencyFilter' :: [ModuleIdent] -> [Module] -> [Module]
dependencyFilter' moduleIdents mods =
  moduleFilter' (concatMap (getDepForModule mods) moduleIdents) mods
  where
    getDepForModule :: [Module] -> ModuleIdent -> [ModuleIdent]
    getDepForModule ms moduleIdent =
      moduleIdent : maybe [] extractDeps (findModule moduleIdent ms)

    findModule :: ModuleIdent -> [Module] -> Maybe Module
    findModule i ms = listToMaybe $ filter go ms
      where go (mn, _) = i == mn

    extractDeps :: Module -> [ModuleIdent]
    extractDeps = mapMaybe extractDep . snd
      where extractDep (Dependency n _) = Just n
            extractDep _ = Nothing

-- | Only keeps Identifiers that start with the given prefix
prefixFilter :: Text -> Filter
prefixFilter = Filter . identFilter prefix
  where
    prefix :: ExternDecl -> Text -> Bool
    prefix (FunctionDecl name _) search = search `isPrefixOf` name
    prefix (DataDecl name _) search = search `isPrefixOf` name
    prefix _ _ = False


-- | Only keeps Identifiers that are equal to the search string
equalityFilter :: Text -> Filter
equalityFilter = Filter . identFilter equality
  where
    equality :: ExternDecl -> Text -> Bool
    equality (FunctionDecl name _) prefix = prefix == name
    equality (DataDecl name _) prefix = prefix == name
    equality _ _ = False


identFilter :: (ExternDecl -> Text -> Bool ) -> Text -> [Module] -> [Module]
identFilter predicate search =
    filter (not . null . snd) . fmap filterModuleDecls
  where
    filterModuleDecls :: Module -> Module
    filterModuleDecls (moduleIdent,decls) =
        (moduleIdent, filter (`predicate` search) decls)
