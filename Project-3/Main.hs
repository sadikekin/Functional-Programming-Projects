import Data.Map (Map, (!))
import qualified Data.Map as M
import qualified Data.List as L
import Data.Char

data WordInput  = WordInput {contentWord :: [Char]}
          deriving (Eq,Show)
data Sentence  = Sentence {contentSentence :: [Word]}
          deriving (Eq,Show)
data CharacterCount  = CharacterCount {contentCC :: M.Map Char Int}
          deriving (Eq,Show)







wordCharCounts :: WordInput ->  CharacterCount
wordCharCounts x = CharacterCount{ contentCC = wordCharCountsHelper (lowerChanger $ contentWord x) (L.nub $ lowerChanger $ contentWord x) }
  where
    lowerChanger (h:h') = if h' == [] then [] else (toLower h) : (lowerChanger h')
    --------------------------------------------
    wordCharCountsHelper _ [] = M.empty
    wordCharCountsHelper originalList (elmnt:nList) =  M.insert elmnt (charNum originalList elmnt) $ wordCharCountsHelper originalList nList
    --------------------------------------------
    charNum s k = length $ filter (== k) s
