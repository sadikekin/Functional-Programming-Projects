import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import Data.Char
import Data.List.Split
import Prelude hiding (Word)


data Trie = Trie {end :: Bool, children :: M.Map Char Trie}
  deriving (Show, Eq)
type Word = String


empty :: Trie
empty = Trie {end = False, children = M.empty}

insert :: Word -> Trie -> Trie
insert (w:[]) t
  | M.lookup w (children t) /= Nothing    = Trie {end = True, children = children t}
  | otherwise                             = Trie {end = True, children = M.insert w empty (children t)}
insert (w:ws) t
  | M.lookup w (children t) /= Nothing    = Trie { end = end t, children =  M.insert w (insert ws  ( ( (M.!) . children )  t  w ) )  (children t)  }
  | otherwise                             = Trie { end = end t, children =  M.insert w (insert ws t) (children t) }
--
-- insertList :: [Word] -> Trie
-- insertList = undefined
--
-- search :: Word -> Trie -> Bool
-- search = undefined
--
-- getWords :: Trie -> [Word]
-- getWords = undefined
--
-- prefix :: Word -> Trie -> Maybe [Word]
-- prefix = undefined


takeInputs = do
  -- Reading the users input and acting according to it
  action <- getLine

  if action == "a" then
    takeInputs
  else if action == "s" then
    takeInputs
  else if action == "f" then
    takeInputs
  else if action == "p" then
    takeInputs
  else if action == "e" then
    return ()
  else
    error "Wrong Input"


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

  takeInputs
