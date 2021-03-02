module Ch12 where

import qualified Data.Foldable as F
import Data.Monoid

newtype ChaList = CharList {getCharList :: [Char]} deriving (Eq, Show)

newtype Pair b a = Pair {getPair :: (a, b)}

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

-- data CoolBool = CoolBool {getCoolBool :: Bool}
newtype CoolBool = CoolBool {getCoolBool :: Bool}

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

lengthCompare x y =
  (length x `compare` length y)
    `mappend` (vowels x `compare` vowels y)
    `mappend` (x `compare` y)
  where
    vowels = length . filter (`elem` "aiueo")

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

instance F.Foldable Tree where
  foldMap f EmptyTree = mempty
  foldMap f (Node x l r) =
    F.foldMap f l
      `mappend` f x
      `mappend` F.foldMap f r

testTree =
  Node
    5
    ( Node
        3
        (Node 1 EmptyTree EmptyTree)
        (Node 6 EmptyTree EmptyTree)
    )
    ( Node
        9
        (Node 8 EmptyTree EmptyTree)
        (Node 10 EmptyTree EmptyTree)
    )