dividedByTen = (/ 10)

-- applyTwice :: (t -> t) -> t -> t
applyTwice f x = f (f x)

zipWith' :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

-- flip' f = g
--   where
--     g x y = f y x

flip' f x y = f y x

map' _ [] = []
map' f (x : xs) = f x : map' f xs

filter' _ [] = []
filter' p (x : xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

largestDivisible = head $ filter p [100000, 99999 ..]
  where
    p x = x `mod` 3829 == 0

chain :: Integral a => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (div n 2)
  | odd n = n : chain (n * 3 + 1)

numLongChains :: Int
-- numLongChains = length $ filter isLong $ map chain [1 .. 100]
--   where
--     isLong xs = length xs > 15
numLongChains = length $ filter (\xs -> length xs > 15) $map chain [1 .. 100]

map'' f = foldr (\x acc -> f x : acc) []

map''' f = foldl (\acc x -> acc ++ [f x]) []

product' :: Num a => [a] -> a
product' = foldl (*) 1

sqrtSums = length $takeWhile (< 1000) $scanl1 (+) $ map sqrt [1 ..]
