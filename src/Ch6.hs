module Ch6 where

import Data.Char
import Data.List
import qualified Data.Map as M

wordsNums = map (\ws -> (head ws, length ws)) . group . sort . words

needle `isIn` haystack = any (needle `isPrefixOf`) $ tails haystack

encode offset = map $ chr . (+ offset) . ord

decode offset = map $ chr . subtract offset . ord

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 = find (\x -> digitSum x == 40) [1 ..]

findKey key [] = Nothing
findKey key ((k, v) : xs)
  | key == k = v
  | otherwise = findKey key xs

phoneBookToMap :: (Ord k) => [(k, a)] -> M.Map k [a]
phoneBookToMap xs = M.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs