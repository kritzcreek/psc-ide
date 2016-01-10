{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module PureScript.Ide.Matcher (Matcher, flexMatcher, runMatcher) where

import           Control.Monad
import           Data.Aeson
import           Data.Function        (on)
import           Data.List            (sortBy)
import           Data.Maybe           (mapMaybe)
import           Data.Monoid
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           PureScript.Ide.Types
import           Text.Regex.TDFA      ((=~))


type ScoredCompletion = (Completion, Double)

newtype Matcher = Matcher (Endo [Completion]) deriving(Monoid)

instance FromJSON Matcher where
  parseJSON = withObject "matcher" $ \o -> do
    (matcher :: Maybe String) <- o .:? "matcher"
    case matcher of
      Just "flex" -> do
        params <- o .: "params"
        search <- params .: "search"
        pure $ flexMatcher search
      Just "helm" -> error "Helm matcher not implemented yet."
      Just "distance" -> error "Distance matcher not implemented yet."
      Just _ -> mzero
      Nothing -> return mempty

-- | Matches any occurence of the search string with intersections
-- |
-- | The scoring measures how far the matches span the string where
-- | closer is better.
-- | Examples:
-- |   flMa matches flexMatcher. Score: 14.28
-- |   sons matches sortCompletions. Score: 6.25
flexMatcher :: T.Text -> Matcher
flexMatcher pattern = mkMatcher (flexMatch pattern)

mkMatcher :: ([Completion] -> [ScoredCompletion]) -> Matcher
mkMatcher matcher = Matcher . Endo  $ fmap fst . sortCompletions . matcher

runMatcher :: Matcher -> [Completion] -> [Completion]
runMatcher (Matcher m)= appEndo m

sortCompletions :: [ScoredCompletion] -> [ScoredCompletion]
sortCompletions = sortBy (flip compare `on` snd)

flexMatch :: T.Text -> [Completion] -> [ScoredCompletion]
flexMatch pattern = mapMaybe (flexRate pattern)

flexRate :: T.Text -> Completion -> Maybe ScoredCompletion
flexRate pattern c@(Completion (_,ident,_)) = do
    score <- flexScore pattern ident
    return (c, score)

-- FlexMatching ala Sublime.
-- Borrowed from: http://cdewaka.com/2013/06/fuzzy-pattern-matching-in-haskell/
--
-- By string =~ pattern we'll get the start of the match and the length of
-- the matchas a (start, length) tuple if there's a match.
-- If match fails then it would be (-1,0)
flexScore :: T.Text -> DeclIdent -> Maybe Double
flexScore "" _ = Nothing
flexScore pat str =
    case TE.encodeUtf8 str =~ TE.encodeUtf8 pat' :: (Int, Int) of
        (-1,0) -> Nothing
        (start,len) -> Just $ calcScore start (start + len)
  where
    Just (first,pattern) = T.uncons pat
    -- This just interleaves the search string with .*
    -- abcd -> a.*b.*c.*d
    pat' = first `T.cons` T.concatMap (T.snoc ".*") pattern
    calcScore start end =
        100.0 / fromIntegral ((1 + start) * (end - start + 1))
