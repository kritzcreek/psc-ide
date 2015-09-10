module PureScript.Ide.Matcher (flexMatcher) where

import           Data.Function        (on)
import           Data.List            (sortBy)
import           Data.Maybe           (mapMaybe)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           PureScript.Ide.Types
import           Text.Regex.TDFA      ((=~))

type ScoredCompletion = (Double, Completion)

flexMatcher :: T.Text -> Matcher
flexMatcher pattern = mkMatcher (flexMatch pattern)

mkMatcher :: ([Completion] -> [ScoredCompletion]) -> Matcher
mkMatcher matcher = fmap snd . sortCompletions . matcher

sortCompletions :: [ScoredCompletion] -> [ScoredCompletion]
sortCompletions = sortBy (compare `on` fst)

flexMatch :: T.Text -> [Completion] -> [ScoredCompletion]
flexMatch pattern = mapMaybe (flexRate pattern)

flexRate :: T.Text -> Completion -> Maybe ScoredCompletion
flexRate pattern c@(Completion (_,ident,_)) =
    (,) <$> score pattern ident <*> pure c

-- FlexMatching ala Sublime.
-- Borrowed from: http://cdewaka.com/2013/06/fuzzy-pattern-matching-in-haskell/
--
-- By string =~ pattern we'll get the start of the match and the length of
-- the matchas a (start, length) tuple if there's a match.
-- If match fails then it would be (-1,0)
score :: T.Text -> DeclIdent -> Maybe Double
score "" _ = Nothing
score pat str =
    case TE.encodeUtf8 str =~ TE.encodeUtf8 pat' :: (Int, Int) of
        (-1,0) -> Nothing
        (start,len) -> Just $ calcScore start (start + len)
  where
    Just (first, pattern) = T.uncons pat
    pat' = first `T.cons` T.concatMap (T.snoc ".*") pattern
    calcScore start end =
        100.0 / fromIntegral ((1 + start) * (end - start + 1))
