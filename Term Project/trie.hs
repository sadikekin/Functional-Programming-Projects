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


empty :: Bool -> Trie
empty withEndValue = Trie {end = withEndValue, children = M.empty}

insert :: Word -> Trie -> Trie
insert [] t                               = Trie {end = True, children = children t}
insert (w:ws) t
  | M.lookup w (children t) /= Nothing    = Trie { end = end t, children =  M.insert w (insert ws ( (children t) M.! w )) (children t) }
  | otherwise                             = Trie { end = end t, children =  M.insert w (insert ws (( M.insert w (empty False) (children t) ) M.! w)) (children t) }

insertList :: [Word] -> Trie
insertList w = foldr insert (empty False) w

search :: Word -> Trie -> Bool
search (w:[]) t = if M.lookup w (children t) == Nothing then False else  end $ (children t) M.! w
search (w:ws) t = if M.lookup w (children t) == Nothing then False else search ws ((children t) M.! w)


--
-- getWords :: Trie -> [Word]
-- getWords = undefined
--
-- prefix :: Word -> Trie -> Maybe [Word]
-- prefix = undefined

takeInputsFromUser :: Trie -> IO()
takeInputsFromUser dictTrie = do
  -- Reading the users input and acting according to it
  action <- getLine

  if action == "a" then
    takeInputsFromUser dictTrie
  else if action == "s" then
    takeInputsFromUser dictTrie
  else if action == "f" then
    takeInputsFromUser dictTrie
  else if action == "p" then
    takeInputsFromUser dictTrie
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

  -- Changing words array to Trie
  let dictTrie = insertList wordsArray
  takeInputsFromUser dictTrie

  putStrLn "Final"
