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

-- This function is responsible for changing the word to CharacterCount
wordCharCounts :: WordInput ->  CharacterCount
wordCharCounts x                                    = wordCharCountsHelper (lowerChanger x) ((L.nub . lowerChanger) x)
  where
    -- lowerChanger function changes the input word to lowercase recursively
    lowerChanger []                                 = []
    lowerChanger (h:h')                             = if h' == [] then [h] else (toLower h) : (lowerChanger h')
    --------------------------------------------
    -- This is the helper function that creates map recursively
    wordCharCountsHelper _ []                       = M.empty
    wordCharCountsHelper originalList (elmnt:nList) = M.insert elmnt (charNum originalList elmnt) $ wordCharCountsHelper originalList nList
    --------------------------------------------
    -- charNum function finds the number of the char in the char list
    charNum s k                                     = length $ filter (\z -> z == k) s


-- This function changes the all words from the sentence to characters
sentenceCharCounts :: Sentence -> CharacterCount
sentenceCharCounts []                     = M.empty
sentenceCharCounts (wrdList:wrdList')     =  M.unionWith (+) (wordCharCounts wrdList) (sentenceCharCounts wrdList')

-- This function maps the all words from the sentence to their CharacterCount
dictCharCounts :: Sentence -> M.Map WordInput CharacterCount
dictCharCounts  []                        = M.empty
dictCharCounts (wordList:wordList')       = M.insert (wordList) (wordCharCounts wordList) $ dictCharCounts wordList'

-- This function finds the all same values over a list. The main feature in here is splitting the keys with their elements.
dictWordsByCharCounts :: M.Map WordInput CharacterCount -> M.Map CharacterCount Sentence
dictWordsByCharCounts fListMap                            = dictWordsByCharCountsSwapKeysWithValues (M.keys fListMap) (M.elems fListMap)
  where
    dictWordsByCharCountsSwapKeysWithValues [] []         = M.empty
    dictWordsByCharCountsSwapKeysWithValues (k:k') (e:e') = M.insertWith (++) (e) ([k]) $ dictWordsByCharCountsSwapKeysWithValues k' e'

-- This function returns anagrams of the given word.
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
    charCountsSubsetsHelper number cc     = subSet number cc ++ charCountsSubsetsHelper (number-1) cc
    --------------------------------------------
    -- Created all posible subsets for given array of tuples.
    subSet number set                     = take (lenSet-number+1) (subSetHelper number set)
      where
        lenSet                            = length set
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

-- This function finds every single possible sentence that we can create from given sentence
sentenceAnagrams :: Sentence -> [Sentence]
sentenceAnagrams s
  | length s == 0                           = []
  | length s == 1 && length (s!!0)  == 1    = [s]
  | otherwise                               = (L.nub . spaceAdder) $ stringCreator [toLower x | x <- concat s] 0 ""
  where
    stringCreatorHelper x currentString     = stringCreator x 0 currentString
    -- This function creates the all possible without spaces.
    --------------------------------------------
    stringCreator [] _ currentString        = [currentString]
    stringCreator xt@(x:xs) currentIndex currentString
      | currentIndex == length xt           = []
      | otherwise                           = stringCreatorHelper xs ([x]++currentString) ++ stringCreator (xs ++ [x]) (currentIndex+1) currentString
    --------------------------------------------
    -- This function finds the all possible strings with spaces. For example [["ab","c"],["ba", "c"],["b","a","c"], ["a", "b", "c"] ... ].
    -- "a b c" and "b a c" are different sentences.
    spaceAdder []                           = []
    spaceAdder (x:xs)                       = sentenceChanger ( L.permutations (x ++  replicate (length x - 1) ' ') )
      where
        sentenceChanger []                  = []
        sentenceChanger (kl:kl')            = [x | x <- splitOn " " kl, x /= ""] : sentenceChanger kl'

-- Since we have all possible string that we can create from given sentence.
-- We go tough every single element in the array and check if that sentence valid according to given words.txt
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

printValues :: [[String]] -> IO ()
printValues [] = return ()
printValues (x:xs) = do
  putStrLn $ L.intercalate " " x
  printValues (xs)


main = do
  arg <- getArgs
  file <- readFile "words.txt"
  let wordsArray =  [ [toLower y | y <- x] | x <- (splitOn "\n" file), x /= "" ]

  let allPossibleString = sentenceAnagrams $ splitOn " " (arg !! 0)

  let result = (anagramFinder allPossibleString wordsArray)

  let resultWithOutEmptyValues = filter (\x -> x /=  [""]) result

  printValues resultWithOutEmptyValues


-- In my implementation,
-- 1- I have found all possible anagram of given sentence
-- 2- Check them one by one the words in sentece are meaningful or not.
-- 3- If the sentence is meaningful it stays, else we change it with [""]
-- 4- Finally, filter the result array for removing [""]

-- Note: I have not used every function in the pdf according to my implementation. However, I have implemented it since the instructions in pdf wanted us to implement it.
