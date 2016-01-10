{-# LANGUAGE TupleSections #-}
module PureScript.Ide.Reexports where

import           Data.List            (union)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe
import           PureScript.Ide.Types

getReexports :: Module -> [ExternDecl]
getReexports (mn, decls)= mapMaybe getExport decls
  where getExport e@(Export mn')
          | mn /= mn' = Just e
          | otherwise = Nothing
        getExport _ = Nothing

replaceReexport :: ExternDecl -> Module -> Module -> Module
replaceReexport e@(Export _) (m, decls) (_, newDecls) =
  (m, filter (/= e) decls `union` newDecls)
replaceReexport _ _ _ = error "Should only get Exports here."

emptyModule :: Module
emptyModule = ("Empty", [])

isExport :: ExternDecl -> Bool
isExport (Export _) = True
isExport _ = False

removeExportDecls :: Module -> Module
removeExportDecls = fmap (filter (not . isExport))

replaceReexports :: Module -> Map ModuleIdent [ExternDecl] -> Module
replaceReexports m db = result
  where reexports = getReexports m
        result = foldl go (removeExportDecls m) reexports

        go :: Module -> ExternDecl -> Module
        go m' re@(Export name) = replaceReexport re m' (getModule name)
        go _ _ = error "partiality! woohoo"

        getModule :: ModuleIdent -> Module
        getModule name = clean res
          where res = fromMaybe emptyModule $ (name , ) <$> Map.lookup name db
                -- we have to do this because keeping self exports in will result in
                -- infinite loops
                clean (mn, decls) = (mn,) (filter (/= Export mn) decls)

resolveReexports :: Map ModuleIdent [ExternDecl] -> Module ->  Module
resolveReexports modules m = do
  let replaced = replaceReexports m modules
  if null . getReexports $ replaced
    then replaced
    else resolveReexports modules replaced
