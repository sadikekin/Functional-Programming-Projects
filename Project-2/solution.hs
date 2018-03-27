data Color = Red | Black
          deriving Eq
data Suit = Clubs | Diamonds | Hearts | Spades
          deriving Eq
data Rank = Num Int | Jack | Queen | King | Ace
          deriving Eq
data Card = Card { suit :: Suit, rank :: Rank }
          deriving Eq
data Move = Draw | Discard Card
          deriving Eq

cardColor :: Card -> Color
cardColor c = case suit c of
   Clubs      -> Red
   Spades     -> Red
   Diamonds   -> Black
   Hearts     -> Black

cardValue :: Card -> Int
cardValue c = case rank c of
  Num y       -> y
  Jack        -> 10
  Queen       -> 10
  King        -> 10
  Ace         -> 11

removeCard :: [Card] -> Card -> [Card]
removeCard cs c = removeCardHelper cs c 0
  where
    -- The integer is for checking if we find element or not
    removeCardHelper :: [Card] -> Card -> Int -> [Card]
    removeCardHelper [] x 1 = []
    removeCardHelper [] x 0 = error "Not found"
    removeCardHelper (c':cs') x y
      | y == 1    = c' : removeCardHelper cs' x y
      | c' == x   = removeCardHelper cs' x 1
      | otherwise = c' : removeCardHelper cs' x y


allSameColors :: [Card] -> Bool
allSameColors [] = True
allSameColors (x:[]) = True
allSameColors (x:xs:xss) = case (cardColor x) /= (cardColor xs) of
  False           -> False
  True            -> allSameColors (xs:xss)

sumCards :: [Card] -> Int
sumCards cs = sumCardsHelper cs 0
    where
      sumCardsHelper :: [Card] -> Int -> Int
      sumCardsHelper [] n = n
      sumCardsHelper (x:xs) n = sumCardsHelper xs (n + cardValue x)

Score :: [Card] -> Int -> Int
