import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe
import System.Environment
import System.IO
import Data.Char
import Data.List.Split
import Prelude hiding (Word)


data Trie = Trie {end :: Bool, children :: M.Map Char Trie}
  deriving (Show, Eq)
type Word = String

-- Creates empty Trie
empty ::  Trie
empty = Trie {end = False, children = M.empty}

-- Inserts the element into given trie
insert :: Word -> Trie -> Trie
insert [] t                               = Trie {end = True, children = children t}
insert (w:ws) t
  | M.lookup w (children t) /= Nothing    = Trie { end = end t, children =  M.insert w ( insert ws (((M.!) . children) t  w) ) (children t) }
  | otherwise                             = Trie { end = end t, children =  M.insert w ( insert ws (( M.insert w empty (children t) ) M.! w) ) (children t) }

-- Inserts all words one by one into given trie
insertList :: [Word] -> Trie
insertList w = foldr insert empty w

-- Searches the given word from the dictionary
search :: Word -> Trie -> Bool
search (w:[]) t = if M.lookup w (children t) == Nothing then False else  end $ (children t) M.! w
search (w:ws) t = if M.lookup w (children t) == Nothing then False else search ws ((children t) M.! w)

-- Get all words from the trie
getWords :: Trie -> [Word]
getWords t = getWordsHelper t  "" []
  where

    getWordsHelper t' currentWord wList
      | M.null $ children t'    = wList
      | otherwise               = childrenIterator (M.toList $ children t') currentWord wList

    childrenIterator [] _ wList = wList
    childrenIterator (x:xs) currentWord wList
      | end $ snd x             = childrenIterator xs currentWord (nodeWord : wList) ++ getWordsHelper (snd x) nodeWord (nodeWord : wList)
      | otherwise               = childrenIterator xs currentWord wList ++ getWordsHelper (snd x) nodeWord wList
        where
          nodeWord              = currentWord ++ [(fst x)]

-- This function finds all words in the dictionary and checks if the words has prefix of given word
prefix :: Word -> Trie -> Maybe [Word]
prefix w t =  if output == [] then Nothing else Just output
   where
    output = [ x | x <- (reverse . L.nub) $ getWords t, (take . length) w x == w]


takeInputsFromUser :: Trie -> IO()
takeInputsFromUser dictTrie = do
  -- Reading the users input and acting according to it
  action <- getLine

  if action == "a" then do
    -- If action is "add", insert the element, which user was typed, to the dictionary
    putStrLn "Please enter the word: "
    wordUser <- getLine
    let newTrie = insert wordUser dictTrie
    putStrLn "Got it. I am ready for your next instruction!"
    takeInputsFromUser newTrie

  else if action == "s" then do
    -- If action is "search", according to function result print outcome.
    putStrLn "Please enter a word: "
    wordUser <- getLine
    if search wordUser dictTrie then do putStrLn "Exists in dictionary!" else do putStrLn "Does not exist in dictionary!"
    putStrLn "I am ready for your next instruction!"
    takeInputsFromUser dictTrie

  else if action == "f" then do
    -- If action is "find", according to function result print outcome.
    putStrLn "Please enter a word/prefix: "
    pre <- getLine
    let foundPrefixes = fromMaybe [] (prefix pre dictTrie)
    if foundPrefixes == [] then do
      putStrLn "No words found with that prefix!"
    else do
      putStrLn "Found words"
      (putStrLn . concat) $ L.intersperse "\n" (foundPrefixes)
      putStrLn "I am ready for your next instruction!"
    takeInputsFromUser dictTrie

  else if action == "p" then do
    -- If action is "print", print all dictionary values
    putStrLn "Words are "
    let allWords = concat $ L.intersperse "\n" (reverse $ L.nub $ getWords dictTrie)
    putStrLn allWords
    putStrLn "I am ready for your next instruction!"
    takeInputsFromUser dictTrie

  else if action == "e" then
    return ()
  else
    error "Wrong Input."


main = do
  -- Getting the file name
  arg <- getArgs

  -- Reading words from file
  file <- readFile $ (!!) arg 0
  let wordsArray = [ [toLower z | z <- x]  | x <- (splitOn "\n" file), x /= ""]

  -- Printing the user interface
  putStrLn "a) Add word"
  putStrLn "s) Search word"
  putStrLn "f) Find words with prefix"
  putStrLn "p) Print all words"
  putStrLn "e) Exit"
  putStrLn "Enter the action: "

  -- Changing words array to Trie
  let dictTrie = insertList wordsArray
  takeInputsFromUser dictTrie
