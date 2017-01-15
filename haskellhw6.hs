-- Exercise 1
import Data.List

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

nextFib :: [Integer] -> Integer
nextFib xs = (xs !! (length xs - 1)) + (xs !! (length xs - 2))

getFib xs = xs ++ [nextFib xs]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = [0, 1] ++ [fibs2 !! (n-1) + fibs2 !! (n-2) | n <- [2..]]
	--Q21`2111111``````````````````````````````````````````````````````````

-- Exercise 3
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream s ss) =  s:(streamToList ss)

instance Show a => Show (Stream a) where
    show a = show (take 20 (streamToList a))

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream s ss) = Stream (f s) $ streamMap f ss

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x $ streamFromSeed f (f x)

nats :: Stream Integer
nats = streamFromSeed (\x -> x + 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) sy = Stream x $ interleaveStreams sy xs

ruler :: Stream Integer
ruler = interleaveStreams zeros onesTwosfromThree
              where zeros = streamRepeat 0
                    onesTwosfromThree = interleaveStreams ones twosfromThree
                     where   ones = streamRepeat 1
                             twosfromThree = interleaveStreams twos fromThree
                               where twos = streamRepeat 2
                                     fromThree = streamFromSeed (\x -> x +1) 3