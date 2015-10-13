module Main where

import Control.Monad.Eff

import Prelude
import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject, printJson, jsonParser)
import Data.Argonaut.Core
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)

import Data.Array (tail)
import Data.Either
import Data.Maybe
import Data.Maybe.Unsafe
import Data.String (split)

data PSType = Package | Ident

instance showPSType :: Show PSType where
  show Package = "package"
  show Ident   = "completion"

class Dummy a where
  typeclassFun :: a -> String

instance encodePSType :: EncodeJson PSType where
  encodeJson = encodeJson <<< show

data Matcher = Flex String

instance encodeMatcher :: EncodeJson Matcher where
  encodeJson (Flex q) =
    "matcher" := "flex"
    ~> "params" := (jsonSingletonObject' "search" q)
    ~> jsonEmptyObject

data Filter =
  ExactFilter String
  | PrefixFilter String
  | ModuleFilter (Array String)
  | DependencyFilter (Array String)

filterWrapper :: forall a. (EncodeJson a) => String -> a -> Json
filterWrapper f q =
  "filter" := f
  ~> ("params" := q)
  ~> jsonEmptyObject

jsonSingletonObject' :: forall a. (EncodeJson a) => String -> a -> Json
jsonSingletonObject' s o = jsonSingletonObject s (encodeJson o)

instance encodeFilter :: EncodeJson Filter where
  encodeJson (ExactFilter q) =
    filterWrapper "exact" (jsonSingletonObject' "search" q)
  encodeJson (PrefixFilter q) =
    filterWrapper "prefix" (jsonSingletonObject' "search" q)
  encodeJson (ModuleFilter modules) =
    filterWrapper "modules" (jsonSingletonObject' "modules" modules)
  encodeJson (DependencyFilter deps) =
    filterWrapper "dependencies" (jsonSingletonObject' "modules" deps)

data Command =
  Cwd
  | Ls
  | Quit
  | Load (Array String) (Array String)
  | Complete (Array Filter) (Maybe Matcher)
  | Pursuit PSType String

commandWrapper :: forall a. (EncodeJson a) => String -> a -> Json
commandWrapper s o =
  "command" := s
  ~> ("params" := o)
  ~> jsonEmptyObject

instance encodeCommand :: EncodeJson Command where
  encodeJson Cwd = jsonSingletonObject' "command" "cwd"
  encodeJson Ls = jsonSingletonObject' "command" "list"
  encodeJson Quit = jsonSingletonObject' "command" "quit"
  encodeJson (Load modules dependencies) =
    commandWrapper "load" (
      "modules" := (encodeJson modules)
      ~> "dependencies" := (encodeJson dependencies)
      ~> jsonEmptyObject
      )
  encodeJson (Complete filters matcher) =
    commandWrapper "complete" (
      "filters" := (encodeJson filters)
      ~> "matcher" := (encodeJson matcher)
      ~> jsonEmptyObject
      )
  encodeJson (Pursuit psType q) =
    commandWrapper "pursuit" (
        "type" := (encodeJson psType)
        ~> "query" := q
        ~> jsonEmptyObject
      )

type Result a = Either String a

type GenCompletion a = {
  type' :: String,
  identifier :: String,
  module' :: String
  | a
}

newtype Completion = Completion (GenCompletion ())
newtype PursuitCompletion = PursuitCompletion (GenCompletion (package :: String))
newtype Modules = Modules (Array String)
newtype Message = Message String

unwrapResponse :: forall a. (DecodeJson a) => String -> Either String a
unwrapResponse s = do
  json <- jsonParser s
  o <- decodeJson json
  resultType <- o .? "resultType"
  case resultType of
    "success" -> do
      result <- o .? "result"
      Right result
    _ -> do
      result <- o .? "result"
      Left result

instance decodeMessage :: DecodeJson Message where
  decodeJson json = maybe
    (Left "ParseError")
    (Right <<< Message)
    (toString json)

instance decodeModules :: DecodeJson Modules where
  decodeJson json = do
    ms <- decodeJson json
    return (Modules (fromJust $ tail (split ", " ms)))

instance decodeCompletion :: DecodeJson Completion where
  decodeJson json = do
    o <- decodeJson json
    identifier <- o .? "identifier"
    type' <- o .? "type"
    module' <- o .? "module"
    return (Completion {identifier: identifier, type': type', module': module'})

instance decodePursuitCompletion :: DecodeJson PursuitCompletion where
  decodeJson json = do
    o <- decodeJson json
    identifier <- o .? "ident"
    type' <- o .? "type"
    module' <- o .? "module"
    package <- o .? "package"
    return (PursuitCompletion {
      identifier: identifier,
      type': type',
      module': module',
      package: package
      })
