module Ch14 where

import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Monoid
import Data.Ratio
import Data.Semigroup
import System.Random

isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

type Food = String

type Price = Sum Int

addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 50)
addDrink _ = ("beer", Sum 30)

test = runWriter (return 3 :: Writer String Int)

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multiWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Gonna multiply these two"]
  return (a * b)

-- gcd' a b
--   | b == 0 = a
--   | otherwise = gcd' b (a `mod` b)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    gcd' b (a `mod` b)

-- gcdReverse :: Int -> Int -> Writer [String] Int
-- gcdReverse a b
--   | b == 0 = do
--     tell ["Finished with " ++ show a]
--     return a
--   | otherwise = do
--     result <- gcdReverse b (a `mod` b)
--     tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
--     return result

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

toDiffList xs = DiffList (xs ++)

fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)

--   (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

instance Semigroup (DiffList a) where
  (DiffList f) <> (DiffList g) = DiffList $ f . g

gcdReverse :: Int -> Int -> Writer (DiffList String) Int
gcdReverse a b
  | b == 0 = do
    tell $ toDiffList ["Finished with " ++ show a]
    return a
  | otherwise = do
    result <- gcdReverse b (a `mod` b)
    tell $ toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    return result

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
  tell (toDiffList ["0"])
finalCountDown x = do
  finalCountDown (x -1)
  tell (toDiffList [show x])

finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = do
  tell ["0"]
finalCountDown' x = do
  finalCountDown' (x -1)
  tell [show x]

addStuff = do
  a <- (* 2)
  b <- (+ 10)
  return (a + b)

-- addStuff x =
--   let a = (* 2) x
--       b = (+ 10) x
--    in a + b

type Stack = [Int]

-- pop :: Stack -> (Int, Stack)
-- pop (x : xs) = (x, xs)
pop :: State Stack Int
pop = state $ \(x : xs) -> (x, xs)

-- push :: Int -> Stack -> ((), Stack)
-- push a xs = ((), a : xs)
push :: Int -> State Stack ()
push a = state $ \xs -> ((), a : xs)

-- stackManip :: Stack -> (Int, Stack)
-- stackManip stack =
--   let ((), newStack1) = push 3 stack
--       (a, newStack2) = pop newStack1
--    in pop newStack2

stackManip = do
  push 3
  a <- pop
  pop

stackyStack :: State Stack ()
stackyStack = do
  stackNow <- get
  if stackNow == [1, 2, 8]
    then put [8, 3, 1]
    else put [9, 2, 1]

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)

keepSmall :: Int -> Writer [String] Bool
keepSmall x
  | x < 4 = do
    tell ["Keeping" ++ show x]
    return True
  | otherwise = do
    tell [show x ++ " is too large, throwing it away"]
    return False

powerset = filterM (const [True, False])

binSmalls acc x
  | x > 9 = Nothing
  | otherwise = Just (acc + x)

solveRPN2 = calculate . parse

parse = words

calculate st = do
  [result] <- foldM foldingFunction [] st
  return result
  where
    foldingFunction (x : y : ys) "*" = return ((x * y) : ys)
    foldingFunction (x : y : ys) "+" = return ((x + y) : ys)
    foldingFunction (x : y : ys) "-" = return ((x - y) : ys)
    foldingFunction (x : y : ys) "/" = return ((x / y) : ys)
    foldingFunction (x : y : ys) "^" = return ((x ** y) : ys)
    foldingFunction (x : xs) "ln" = return (log x : xs)
    foldingFunction xs "sum" = return [sum xs]
    foldingFunction xs numberString = liftM (: xs) (readMaybe numberString)

readMaybe st = case reads st of
  [(x, "")] -> Just x
  _ -> Nothing

inMany x =
  foldr
    (<=<)
    return
    (replicate x moveKnight)

moveKnight (c, r) = do
  (c', r') <- [(c + 2, r -1), (c + 2, r + 1), (c -2, r -1), (c -2, r + 1), (c + 1, r -2), (c + 1, r + 2), (c -1, r -2), (c -1, r + 2)]
  guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])
  return (c', r')

canReachIn x start end = end `elem` inMany x start

newtype Prob a = Prob {getProb :: [(a, Rational)]} deriving (Show)

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

instance Applicative Prob where
  pure x = Prob [(x, 1 % 1)]
  (<*>) = ap

-- (Prob (f, _)) <*> g = fmap f g

instance Monad Prob where
  m >>= f = flatten (fmap f m)
    where
      mulAll (Prob innerxs, p) = map (\(x, r) -> (x, p * r)) innerxs
      flatten (Prob xs) = Prob $ concat $ map mulAll xs

data Coin = Heads | Tails deriving (Show, Eq)

coin = Prob [(Heads, 1 % 2), (Tails, 1 % 2)]

loadedCoin = Prob [(Heads, 1 % 10), (Tails, 9 % 10)]

flipThree = do
  a <- coin
  b <- coin
  c <- loadedCoin
  return (all (== Tails) [a, b, c])