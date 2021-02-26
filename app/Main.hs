module Main where

import Ch1 (doubleMe)
import Ch3
import Ch7
import Ch8
import Data.Char
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
  print $ Ch7.area $ Rectangle (Point 0 0) (Point 1 2)

  putStrLn "Hello World! What's your name?"
  name <- getLine
  putStrLn $ "Hey " ++ name ++ " you lock!"
  lastName <- getLine
  let uFirstName = map toUpper name
      uLastName = map toUpper lastName

  putStrLn $ "hello " ++ uFirstName ++ " " ++ uLastName ++ "."
  reverseLine
  sequence $ map print [1, 2, 3, 4, 5]
  mapM_ print [1, 2, 3, 4]
  getColors
