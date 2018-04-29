import Data.Map (Map, (!))
import qualified Data.Map as M
import qualified Data.List as L
import Data.Char

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

dictWordsByCharCounts :: M.Map WordInput CharacterCount -> M.Map (CharacterCount) [WordInput]
dictWordsByCharCounts fListMap = dictWordsByCharCountsSwapKeysWithValues (M.keys fListMap) (M.elems fListMap)
  where
    dictWordsByCharCountsSwapKeysWithValues [] []         = M.empty
    dictWordsByCharCountsSwapKeysWithValues (k:k') (e:e') = M.insertWith (++) (e) ([k]) $ dictWordsByCharCountsSwapKeysWithValues k' e'


wordAnagrams :: M.Map CharacterCount [WordInput] -> WordInput -> [WordInput]
wordAnagrams fListMap w = fListMap ! (wordCharCounts w)
