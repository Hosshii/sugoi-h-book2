module Ch11 where

import Control.Applicative
import Data.Char
import Data.List

spaceToUpper = do
  line <- fmap (intersperse '-' . reverse . map toUpper) getLine
  putStrLn line

data CMayBe a = CNothing | CJust Int a deriving (Show)

instance Functor CMayBe where
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter + 1) (f x)

-- sequenceA' [] = pure []
-- sequenceA' (x : xs) = (:) <$> x <*> sequenceA' xs
sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])