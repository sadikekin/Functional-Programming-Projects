import qualified Data.Map -- as M (can be shortened) import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)


data Trie = Trie {end :: Bool, children :: Map Char Trie}
type Word = String empty :: Trie

empty = undefined

insert :: Word -> Trie -> Trie insert = undefined

insertList :: [Word] -> Trie insertList = undefined

search :: Word -> Trie -> Bool search = undefined

getWords :: Trie -> [Word] getWords = undefined

prefix :: Word -> Trie -> Maybe [Word] prefix = undefined


main = do 
