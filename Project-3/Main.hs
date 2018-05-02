import Data.Map (Map, (!))
import System.Environment
import Data.List.Split
import qualified Data.Map as M
import qualified Data.List as L
import Data.Char
import Control.Monad.Fix

type WordInput        = [Char]
type Sentence         = [WordInput]
type CharacterCount   = M.Map Char Int


wordCharCounts :: WordInput ->  CharacterCount
wordCharCounts x = wordCharCountsHelper (lowerChanger x) ((L.nub . lowerChanger) x)
  where
    lowerChanger []                                 = []
    lowerChanger (h:h')                             = if h' == [] then [h] else (toLower h) : (lowerChanger h')
    --------------------------------------------
    wordCharCountsHelper _ []                       = M.empty
    wordCharCountsHelper originalList (elmnt:nList) = M.insert elmnt (charNum originalList elmnt) $ wordCharCountsHelper originalList nList
    --------------------------------------------
    charNum s k                                     = length $ filter (\z -> z == k) s


sentenceCharCounts :: Sentence -> CharacterCount
sentenceCharCounts sentence                     = sentenceCharCountsHelper sentence
  where
    sentenceCharCountsHelper []                 = M.empty
    sentenceCharCountsHelper (wrdList:wrdList') = M.unionWith (+) (wordCharCounts wrdList) (sentenceCharCountsHelper wrdList')


dictCharCounts :: Sentence -> M.Map WordInput CharacterCount
dictCharCounts wordList                     = dictCharCountsHelper wordList
  where
    dictCharCountsHelper []                 = M.empty
    dictCharCountsHelper (wrdList:wrdList') = M.insert (wrdList) (wordCharCounts wrdList) $ dictCharCountsHelper wrdList'


dictWordsByCharCounts :: M.Map WordInput CharacterCount -> M.Map CharacterCount Sentence
dictWordsByCharCounts fListMap                            = dictWordsByCharCountsSwapKeysWithValues (M.keys fListMap) (M.elems fListMap)
  where
    dictWordsByCharCountsSwapKeysWithValues [] []         = M.empty
    dictWordsByCharCountsSwapKeysWithValues (k:k') (e:e') = M.insertWith (++) (e) ([k]) $ dictWordsByCharCountsSwapKeysWithValues k' e'


wordAnagrams :: M.Map CharacterCount Sentence -> WordInput -> Sentence
wordAnagrams fListMap w                   = fListMap ! (wordCharCounts w)


charCountsSubsets :: CharacterCount -> [CharacterCount]
charCountsSubsets cCount                  = (L.nub . subSetArrayToMap) $ charCountsSubsetsHelper (length flatterVersion) (flatterVersion)
  where
    flatterVersion                        = flatter ( M.toList cCount )
    --------------------------------------------
    -- Flatter function evolves [('a',2)] to [('a',1),('a',1)]
    flatter (x:xs)                        = if xs /= [] then flatterRecursive (fst x) (snd x) ++ flatter xs else flatterRecursive (fst x) (snd x)
      where
        flatterRecursive _  0             = []
        flatterRecursive f s              = (f, 1) : flatterRecursive f (s-1)
    --------------------------------------------
    charCountsSubsetsHelper 0 _           = [[]]
    charCountsSubsetsHelper number cc = subSet number cc ++ charCountsSubsetsHelper (number-1) cc
    --------------------------------------------
    -- Created all posible subsets for given array of tuples.
    subSet number set                     = take (lenSet-number+1) (subSetHelper number set)
      where
        lenSet = length set
        ----------------------------------------
        subSetHelper _ []                 = [[]]
        subSetHelper num' (b:bs)          = take num' (b:bs) : subSetHelper num' bs
    --------------------------------------------
    subSetArrayToMap []                   = [M.empty]
    subSetArrayToMap (a:as)               = subSetArrayToMapHelper a : subSetArrayToMap as
      where
        subSetArrayToMapHelper []         = M.empty
        subSetArrayToMapHelper (l:ls)     = M.insertWith (+) (fst l) (snd l) (subSetArrayToMapHelper ls)


subtractCounts :: CharacterCount -> CharacterCount -> CharacterCount
subtractCounts ccOne ccTwo                          = subtractCountsHelper ccOne $ (flatter . M.toList) ccTwo
  where
    -- Flatter function evolves [('a',2)] to [('a',1),('a',1)]
    flatter (x:xs)                                  = if xs /= [] then flatterRecursive (fst x) (snd x) ++ flatter xs else flatterRecursive (fst x) (snd x)
      where
        flatterRecursive _  0                       = []
        flatterRecursive f s                        = (f, 1) : flatterRecursive f (s-1)
    --------------------------------------------
    removerFunc k e                                 = if e > 1 then Just (e-1) else Nothing
    --------------------------------------------
    subtractCountsHelper ccOne'  (ccTwo':[])        = M.updateWithKey removerFunc (fst ccTwo') (ccOne')
    subtractCountsHelper ccOne'  (ccTwo':ccTwos')   = subtractCountsHelper currentCCOne ccTwos'
      where
        currentCCOne                                = M.updateWithKey removerFunc (fst ccTwo') (ccOne')

-- This finds every single possible sentence that we can create from given sentence
sentenceAnagrams :: Sentence -> [Sentence]
sentenceAnagrams s
  | length s == 1 && length (s!!0)  == 1    = [s]
  | otherwise                               = (L.nub . spaceAdder) $ stringCreator [toLower x | x <- concat s] 0 ""
  where
    stringCreatorHelper x currentString     = stringCreator x 0 currentString
    --------------------------------------------
    stringCreator [] _ currentString        = [currentString]
    stringCreator xt@(x:xs) currentIndex currentString
      | currentIndex == length xt           = []
      | otherwise                           = stringCreatorHelper xs ([x]++currentString) ++ stringCreator (xs ++ [x]) (currentIndex+1) currentString
    --------------------------------------------
    spaceAdder []                           = []
    spaceAdder (x:xs)                       = spaceAdderHelper (length x - 1) ++ spaceAdder xs
      where
        spaceAdderHelper 0                  = []
        spaceAdderHelper delimeterNum       = ( (sentenceChanger . L.permutations) $ x ++ replicate delimeterNum ' ' )  ++ spaceAdderHelper (delimeterNum-1)
        ----------------------------------------
        sentenceChanger []                  = []
        sentenceChanger (kl:kl')            = [x | x <- splitOn " " kl, x /= ""] : sentenceChanger kl'

anagramFinder :: [Sentence] -> [String] -> [Sentence]
anagramFinder [] _                                   = []
anagramFinder (anagramSentence:allAnagrams) wordList = anagramFinderHelper anagramSentence wordList : anagramFinder allAnagrams wordList
  where
    anagramFinderHelper :: Sentence -> [String]-> Sentence
    anagramFinderHelper [] _                         = anagramSentence
    anagramFinderHelper _ []                         = [""]
    anagramFinderHelper st@(s:s') (w:w')
      | s == w                                       = anagramFinderHelper s' wordList
      | otherwise                                    = anagramFinderHelper st w'

main = do
  arg <- getArgs
  file <- readFile "words.txt"
  let wordsArray =  [ [toLower y | y <- x] | x <- (splitOn "\n" file), x /= "" ]

  let allPossibleString = sentenceAnagrams $ splitOn " " (arg !! 0)

  let resultArray = [ x | x <- (anagramFinder allPossibleString wordsArray), x /= [""] ]
  
  putStrLn (show resultArray)
