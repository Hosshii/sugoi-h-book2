module Ch3 (lucky, sayMe, factorial) where

lucky :: Int -> String
lucky 7 = "Lucky num"
lucky x = show x ++ "not lucky num"

sayMe :: Int -> [Char]
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe x = "Not between 1 and 2"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n -1)

addVectors :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [p] -> p
head' [] = error "Cannot call head on an empty list"
head' (x : _) = x

firstLetter :: [Char] -> [Char]
firstLetter "" = "empty"
firstLetter all@(x : _) = "The first letter of " ++ all ++ " is " ++ [x]

-- bmiTell :: (Fractional a, Ord a) => a -> a -> [Char]
-- bmiTell weight height
--   | weight / height ^ 2 <= 18.5 = "You are underweight."
--   | weight / height ^ 2 <= 25.0 = "You are normal."
--   | weight / height ^ 2 <= 30.0 = "You are fat"
--   | otherwise = "You are whale"
bmiTell :: Double -> Double -> String
bmiTell weight height
  | bmi <= skinny = "You are underweight."
  | bmi <= normal = "You are normal."
  | bmi <= fat = "You are fat"
  | otherwise = "You are whale"
  where
    bmi = weight / height ^ 2
    skinny = 18.5
    normal = 25.0
    fat = 30.0

myCompare :: Ord a => a -> a -> Ordering
a `myCompare` b
  | a == b = EQ
  | a < b = LT
  | otherwise = GT

initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f : _) = firstname
    (l : _) = lastname

calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ^ 2

-- calcBmis' xs = [let bmi w h = w / h ^ 2 in bmi w h | (w, h) <- xs]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

describeList ls =
  "the list is "
    ++ case ls of
      [] -> "empty"
      [x] -> "a singleton list"
      xs -> "a longer list"
