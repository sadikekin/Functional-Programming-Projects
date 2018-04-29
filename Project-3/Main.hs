import Data.Map (Map, (!))
import qualified Data.Map as M
import qualified Data.List as L
import Data.Char
import Control.Monad.Fix

data WordInput  = WordInput {contentWord :: [Char]}
          deriving (Eq, Show, Ord)
data Sentence  = Sentence {contentSentence :: [WordInput]}
          deriving (Eq, Show, Ord)
data CharacterCount  = CharacterCount {contentCC :: M.Map Char Int}
          deriving (Eq, Show, Ord)


wordCharCounts :: WordInput ->  CharacterCount
wordCharCounts x = CharacterCount{ contentCC = wordCharCountsHelper (lowerChanger $ contentWord x) (L.nub $ lowerChanger $ contentWord x) }
  where
    lowerChanger (h:h')                             = if h' == [] then [h] else (toLower h) : (lowerChanger h')
    --------------------------------------------
    wordCharCountsHelper _ []                       = M.empty
    wordCharCountsHelper originalList (elmnt:nList) =  M.insert elmnt (charNum originalList elmnt) $ wordCharCountsHelper originalList nList
    --------------------------------------------
    charNum s k                                     = length $ filter (== k) s

sentenceCharCounts :: Sentence -> CharacterCount
sentenceCharCounts sentence                     = CharacterCount { contentCC =  sentenceCharCountsHelper $ contentSentence sentence }
  where
    sentenceCharCountsHelper []                 = M.empty
    sentenceCharCountsHelper (wrdList:wrdList') = M.unionWith (+) (contentCC $ wordCharCounts wrdList) (sentenceCharCountsHelper wrdList')


dictCharCounts :: Sentence -> M.Map WordInput CharacterCount
dictCharCounts wordList = dictCharCountsHelper $ contentSentence wordList
  where
    dictCharCountsHelper [] = M.empty
    dictCharCountsHelper (wrdList:wrdList') = M.insert (wrdList) (wordCharCounts wrdList) $ dictCharCountsHelper wrdList'

dictWordsByCharCounts :: M.Map WordInput CharacterCount -> M.Map CharacterCount [WordInput]
dictWordsByCharCounts fListMap = dictWordsByCharCountsSwapKeysWithValues (M.keys fListMap) (M.elems fListMap)
  where
    dictWordsByCharCountsSwapKeysWithValues [] []         = M.empty
    dictWordsByCharCountsSwapKeysWithValues (k:k') (e:e') = M.insertWith (++) (e) ([k]) $ dictWordsByCharCountsSwapKeysWithValues k' e'


wordAnagrams :: M.Map CharacterCount [WordInput] -> WordInput -> [WordInput]
wordAnagrams fListMap w = fListMap ! (wordCharCounts w)


subtractCounts :: CharacterCount -> CharacterCount -> CharacterCount
subtractCounts ccOne ccTwo = CharacterCount { contentCC = subtractCountsHelper (contentCC ccOne) (flatter $ M.toList $ contentCC ccTwo) }
  where
    -- Flatter function evolves [('a',2)] to [('a',1),('a',1)]
    flatter (x:xs) = if xs /= [] then flatterRecursive (fst x) (snd x) ++ flatter xs else flatterRecursive (fst x) (snd x)
      where
        flatterRecursive _  0 = []
        flatterRecursive f s = (f, 1) : flatterRecursive f (s-1)
    --------------------------------------------
    removerFunc k e = if e > 1 then Just (e-1) else Nothing
    --------------------------------------------
    subtractCountsHelper ccOne'  (ccTwo':[])        = M.updateWithKey removerFunc (fst ccTwo') (ccOne')
    subtractCountsHelper ccOne'  (ccTwo':ccTwos')   = subtractCountsHelper currentCCOne ccTwos'
      where
        currentCCOne = M.updateWithKey removerFunc (fst ccTwo') (ccOne')
