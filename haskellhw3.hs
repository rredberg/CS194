module Golf where

import Data.Char
import Data.List

-- Exercise 1 Hopscotch
-- Approach: Create a function makeIndList that keeps the first element of every nth
-- tuple in a list of tuples, then map this function over the characters in the input word.
skips :: [a] -> [[a]]
skips word = let ind = [1..length word]
               in map (makeIndList (zip word ind)) ind

makeIndList :: (Integral b) => [(a, b)] -> b -> [a]
makeIndList xs x = [ a | (a, b) <- xs, mod b x == 0]

-- Exercise 2 Local Maxima
-- Approach: Take 3-elem slices of the list and filter based on middle element.
localMaxima :: [Integer] -> [Integer]
localMaxima xs = [ b | (a:b:c:[]) <- (threeSlices xs), a < b && c < b]

dr :: [a] -> Int -> [a]
dr xs x = drop x xs

threeSlices :: [a] -> [[a]]
threeSlices xs = map (take 3) (map (dr xs) [0..length xs-3])

-- Exercise 3 Histogram
-- start with curr_Freq = max frequency and filter based on if freq_x >= curr_freq?
-- zip [0..9] with freq
--histogram :: [Integer] -> String
histogram xs = let fList = map (getFreq xs) [0..9]
                   maxFreq = maximum fList
                  in intercalate "" (map (generateRow fList) [maxFreq, maxFreq - 1..1])
                   ++ "==========\n0123456789\n"

getFreq :: (Eq a) => [a] -> a -> Int
getFreq xs x = length (filter (==x) xs)

generateRow :: (Integral a) => [a] -> a -> [Char]
generateRow freqList currFreq = (map (\x -> if x >= currFreq then '*' else ' ') freqList)  ++ "\n"