import Data.Char

data Color = Red | Black
          deriving (Eq,Show)
data Suit  = Clubs | Diamonds | Hearts | Spades
          deriving (Eq,Show)
data Rank  = Num Int | Jack | Queen | King | Ace
          deriving (Eq,Show)
data Card  = Card { suit :: Suit, rank :: Rank }
          deriving (Eq,Show)
data Move  = Draw | Discard Card
          deriving (Eq,Show)
data State = End | Ongoing
          deriving (Eq,Show)


cardColor :: Card -> Color
cardColor c = case suit c of
   Clubs      -> Black
   Spades     -> Black
   Diamonds   -> Red
   Hearts     -> Red

cardValue :: Card -> Int
cardValue c = case rank c of
  Num y       -> y
  Jack        -> 10
  Queen       -> 10
  King        -> 10
  Ace         -> 11

removeCard :: [Card] -> Card -> [Card]
removeCard [] card = error "Card not in list"
removeCard (c:cs) card
  | c == card = cs
  | otherwise = c : removeCard cs card


allSameColors :: [Card] -> Bool
allSameColors (x:[]) = True
allSameColors (x:xs:xss) = case (cardColor x) == (cardColor xs) of
  False           -> False
  True            -> allSameColors (xs:xss)

sumCards :: [Card] -> Int
sumCards cs = sumCardsHelper cs 0
    where
      sumCardsHelper :: [Card] -> Int -> Int
      sumCardsHelper [] n = n
      sumCardsHelper (x:xs) n = sumCardsHelper xs (n + cardValue x)

score :: [Card] -> Int -> Int
score c g
  | c' > g      = preliminaryScoreGreater
  | otherwise   = preliminaryScoreSmaller
    where
      c' = sumCards c
      preliminaryScoreGreater = if allSameColors c then (3 * (c' - g)) `div` 2 else (3 * (c' - g))
      preliminaryScoreSmaller = if allSameColors c then (g - c') `div` 2 else (g - c')


runGame :: [Card] -> [Move] -> Int -> Int
runGame cs ms g = runGameHelper cs [] ms Ongoing (maxBound :: Int)
  where
    runGameHelper :: [Card] -> [Card] -> [Move] -> State -> Int ->Int
    runGameHelper _ _ _ End minScore                = minScore
    runGameHelper _ hcs [] Ongoing minScore         = runGameHelper [] hcs [] End minScore
    runGameHelper cs' hcs (m':ms') Ongoing minScore = case m' of
      (Discard c)     -> runGameHelperDiscardHelper cs' (removeCard hcs c) hcs ms' minScore
      (Draw)          -> runGameHelperDrawHelper cs' hcs ms' minScore


    runGameHelperDiscardHelper :: [Card] -> [Card] -> [Card] -> [Move] -> Int -> Int
    runGameHelperDiscardHelper cs' removedCards originalCards ms' minScore  = if removedCards == originalCards then error "No cards" else runGameHelper cs' removedCards  ms' Ongoing (min (score removedCards g) minScore)

    runGameHelperDrawHelper :: [Card] -> [Card] -> [Move] -> Int -> Int
    runGameHelperDrawHelper [] hcs ms'  minScore          = runGameHelper [] [] [] End minScore
    runGameHelperDrawHelper (c':cs') hcs ms'  minScore    = if sumCards (c':hcs) > g then runGameHelper [] [] [] End (min (score (c':hcs) g) minScore) else runGameHelper cs' (c':hcs) ms' Ongoing (min (score (c':hcs) g) minScore)

convertSuit :: Char -> Suit
convertSuit 'd' = Diamonds
convertSuit 'D' = Diamonds
convertSuit 'c' = Clubs
convertSuit 'C' = Clubs
convertSuit 's' = Spades
convertSuit 'S' = Spades
convertSuit 'h' = Hearts
convertSuit 'H' = Hearts
convertSuit  x  = error "Wrong input"


convertRank :: Char -> Rank
convertRank x
  | x == 'J' || x == 'j'  = Jack
  | x == 'K' || x == 'k'  = King
  | x == 'Q' || x == 'q'  = Queen
  | x == '1'              = Ace
  | x == 'T' || x == 't'  = Num 10
  | otherwise             = Num (digitToInt x)


convertCard :: Char -> Char -> Card
convertCard s r =  Card (convertSuit s) (convertRank r)



readCards :: IO([Card])
readCards = do
  cardList <- readCards' ([]::[Card])
  return (reverse cardList)
    where
      readCards' :: [Card] -> IO([Card])
      readCards' cs = do
        line <- getLine
        if line == "." then return cs else do
            let suit = head line
            let rank = head (tail line)
            readCards' ((convertCard suit rank) : cs)



convertMove :: Char -> Char -> Char -> Move
convertMove 'd' _ _ = Draw
convertMove 'D' _ _ = Draw
convertMove 'r' s c = Discard (convertCard s c)
convertMove 'R' s c = Discard (convertCard s c)




readMoves :: IO([Move])
readMoves = do
  moveList <- readMoves' ([]::[Move])
  return (reverse moveList)
    where
      readMoves' :: [Move] -> IO([Move])
      readMoves' ms = do
        line <- getLine
        if line == "." then return ms else do
            let moveName = head line
            let suitName = head (tail line)
            let rankName = head (tail (tail line))
            readMoves' ((convertMove moveName suitName rankName) : ms)



main :: IO()
main = do
  putStrLn "Enter cards:"
  cards <- readCards
  putStrLn (show cards)
  putStrLn "Enter moves:"
  moves <- readMoves
  putStrLn (show moves)
  putStrLn "Enter goal:"
  line <- getLine
  let goal = read line :: Int
  let score = runGame cards moves goal
  putStrLn ("Score: " ++ show score)
