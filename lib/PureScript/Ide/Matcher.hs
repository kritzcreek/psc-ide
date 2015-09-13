module PureScript.Ide.Matcher (flexMatcher, runMatcher) where

import           Data.Function        (on)
import           Data.List            (sortBy)
import           Data.Maybe           (mapMaybe)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           PureScript.Ide.Types
import           Text.Regex.TDFA      ((=~))


type ScoredCompletion = (Double, Completion)

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
mkMatcher matcher = Matcher $ fmap snd . sortCompletions . matcher


runMatcher :: Matcher -> [Completion] -> [Completion]
runMatcher (Matcher m) = m

sortCompletions :: [ScoredCompletion] -> [ScoredCompletion]
sortCompletions = sortBy (flip compare `on` fst)

flexMatch :: T.Text -> [Completion] -> [ScoredCompletion]
flexMatch pattern = mapMaybe (flexRate pattern)

flexRate :: T.Text -> Completion -> Maybe ScoredCompletion
flexRate pattern c@(Completion (_,ident,_)) =
    (,) <$> flexScore pattern ident <*> pure c

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
    Just (first, pattern) = T.uncons pat
    pat' = first `T.cons` T.concatMap (T.snoc ".*") pattern
    calcScore start end =
        100.0 / fromIntegral ((1 + start) * (end - start + 1))
