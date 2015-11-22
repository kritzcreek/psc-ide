{-# LANGUAGE TupleSections #-}
module PureScript.Ide.Reexports where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (union)
import PureScript.Ide.Types

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

        getModule :: ModuleIdent -> Module
        getModule name = fromMaybe emptyModule $ (name , ) <$> Map.lookup name db
