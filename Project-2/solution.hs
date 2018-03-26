data Color = Red | Black
data Suit = Clubs | Diamonds | Hearts | Spades
data Rank = Num Int | Jack | Queen | King | Ace
data Card = Card { suit :: Suit, rank :: Rank }
data Move = Draw | Discard Card

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
    removeCard :: [Card] -> Card -> Int -> [Card]
    removeCard [] x 1 = []
    removeCard [] x 0 = error "Not found"
    removeCard (c':cs') x y
      | y == 1    = c' : removeCard cs' x y
      | c' == x   = removeCard cs' x 1
      | otherwise = c' : removeCard cs' x y
