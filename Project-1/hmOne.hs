
dayOfWeek :: Integer -> Integer -> Integer -> Integer

dayOfWeek y m d = returnValue
  where
    j :: Double
    k :: Integer
    l :: Integer
    tOne :: Integer
    tTwo :: Integer
    tThree :: Integer
    returnValue :: Integer
    j = fromIntegral y / fromIntegral 100
    k = y `mod` 100
    l = m' m
      where
        m' :: Integer -> Integer
        m' c
          | c <= 2 = c + 12
          | otherwise = c

    tOne = floor (fromIntegral 13 * fromIntegral (m + 1) / fromIntegral 5)
    tTwo = floor  (fromIntegral k / fromIntegral 4)
    tThree = floor (j / fromIntegral 4)
    returnValue = (d + tOne + k + tTwo + tThree + 5 * floor (j)) `mod` 7
