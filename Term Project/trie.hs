import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import Data.Char
import Data.List.Split
import Prelude hiding (Word)


data Trie = Trie {end :: Bool, children :: M.Map Char Trie}
type Word = String


empty :: Trie
empty = undefined

insert :: Word -> Trie -> Trie
insert = undefined

insertList :: [Word] -> Trie
insertList = undefined

search :: Word -> Trie -> Bool
search = undefined

getWords :: Trie -> [Word]
getWords = undefined

prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined


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

  -- Reading the users input and acting according to it
  action <- getLine

  if action == "a" then
    putStrLn "Add"
  else if action == "s" then
    putStrLn "Search"
  else if action == "f" then
    putStrLn "Find"
  else if action == "p" then
    putStrLn "Print"
  else if action == "e" then
    putStrLn "Exit"
  else
    error "Wrong Input"
