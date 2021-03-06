
dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d = returnValue
  where
    m' = if m <= 2 then m + 12 else m
    y' = if m <= 2 then y - 1 else y
    j = floor (fromIntegral y' / fromIntegral 100)
    k = y' `mod` 100

    tOne = floor (13.0 * fromIntegral (m' + 1) /  5.0)
    tTwo = floor  (fromIntegral k /  4.0)
    tThree  = floor (fromIntegral j /  4.0)
    returnValue = ((d + tOne + k + tTwo + tThree + (5 * j)) `mod` 7) - 1


sundaysOne :: Integer -> Integer -> Integer
sundaysOne start end = sundays' start 1
  where
    sundays' :: Integer -> Integer -> Integer
    sundays' y m
      | y > end   = 0
      | otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest where
        nextY = if m == 12 then y + 1 else y
        nextM = if m == 12 then 1 else m + 1
        rest = sundays' nextY nextM

sundaysOneTail :: Integer -> Integer -> Integer
sundaysOneTail start end = sundays' start 1 0
  where
    sundays' :: Integer -> Integer -> Integer -> Integer
    sundays' y m n
      | y > end   = n
      | otherwise = rest
        where
        n'    = if dayOfWeek y m 1 == 1 then n + 1 else n
        nextY = if m == 12 then y + 1 else y
        nextM = if m == 12 then 1 else m + 1
        rest = sundays' nextY nextM n'



leap :: Integer -> Bool
leap y = (y `mod` 4 == 0) && (y `mod` 100 /= 0) || (y `mod` 400 == 0)

daysInMonth :: Integer -> Integer -> Integer
daysInMonth  m y
  | m == 2 = if leap(y) then 29 else 28
  | m == 4 || m == 6 || m == 9 || m == 11 = 30
  | otherwise = 31

sundaysTwo :: Integer -> Integer -> Integer
sundaysTwo start end = sundays' start 1 0 2
  where
    sundays' :: Integer -> Integer -> Integer -> Integer -> Integer
    sundays' y m n weekday
      | y > end   = n
      | otherwise = rest
        where
        days = daysInMonth m  y
        weekday' = weekday +  days `mod` 7
        n'    = if weekday `mod` 7 == 0  then n + 1 else n
        nextY = if m == 12 then y + 1 else y
        nextM = if m == 12 then 1 else m + 1
        rest = sundays' nextY nextM n' weekday'
