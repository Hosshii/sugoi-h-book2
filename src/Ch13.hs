module Ch13 where

import Control.Monad

applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

type Birds = Int

type Pole = (Birds, Birds)

landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight n (left, right)
  | abs ((right + n) - left) < 4 = Just (left, right + n)
  | otherwise = Nothing

x -: f = f x

banana _ = Nothing

routine = do
  start <- return (0, 0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 1 second

wopwop = do
  (x : xs) <- Just ""
  return x

type KnightPos = (Int, Int)

moveKnight (c, r) = do
  (c', r') <- [(c + 2, r -1), (c + 2, r + 1), (c -2, r -1), (c -2, r + 1), (c + 1, r -2), (c + 1, r + 2), (c -1, r -2), (c -1, r + 2)]
  guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])
  return (c', r')

in3 start = moveKnight start >>= moveKnight >>= moveKnight

canReachIn3 start end = end `elem` in3 start

type KnightRoute = [KnightPos]

routeKnight rt = (moveKnight . head) rt >>= \next -> [next : rt]

routeIn3 pos = routeKnight [pos] >>= routeKnight >>= routeKnight

reachRouteIn3 start end = reverse $ filter (\x -> end == head x) $ routeIn3 start