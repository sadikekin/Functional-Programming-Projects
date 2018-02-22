
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


sundaysOne :: Integer -> Integer -> Integer
sundaysOne start end = sundays' y m
  where
    sundays' :: Integer -> Integer -> Integer
    sundays' y m
      | y > end = y
      | otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest where
        nextY = y
        nextM = m
        rest = sundays' y m  

leap :: Integer -> Bool
leap y = (y `mod` 4 == 0) && (y `mod` 100 /= 0) || (y `mod` 400 == 0)

daysInMonth :: Integer -> Integer -> Integer
daysInMonth  m y
  | m == 2 = if leap(y) then 29 else 28
  | m == 4 || m == 6 || m == 9 || m == 11 = 30
  | otherwise = 31
