-- Problem 1.
toDigits :: Integer -> [Integer]
toDigits x = if x <= 0 then [] else toDigits (div x 10) ++ [mod x 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = if x <= 0 then [] else mod x 10 : toDigitsRev (div x 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = if length xs < 2 then xs
                      else (doubleEveryOther (init(init xs))) ++ [last (init xs) * 2] ++ [last xs]

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits xs = if head xs > 9 then sumDigits (toDigits (head xs)) + sumDigits (tail xs)
               else head xs + sumDigits (tail xs)

validate :: Integer -> Bool
validate x = mod (sumDigits (doubleEveryOther (toDigits x))) 10 == 0

-- Problem 2.
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi x p1 p2 p3 = (hanoi (x-1) p1 p3 p2) ++ (p1, p2) : (hanoi (x-1) p3 p2 p1)