module Main where

import Ch1 (doubleMe)
import Ch3
import Geometry.Sphere as Sphere
import Lib

main :: IO ()
main = do
  someFunc
  print (doubleMe 1)
  print (lucky 1)
  print (sayMe 2)
  print (sayMe 2)
  print (factorial 2)
  print $ Sphere.area 2
