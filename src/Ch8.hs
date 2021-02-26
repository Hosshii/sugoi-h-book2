module Ch8 (reverseLine, getColors) where

import Control.Monad

reverseLine = do
  line <- getLine
  when (line == "exit") $ do
    putStrLn "exit inputted, so exit"

  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      reverseLine

reverseWords = unwords . map reverse . words

getColors = do
  colors <- forM [1, 2, 3, 4] $ \a -> do
    putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
    getLine
  putStrLn "The color that you associate with 1, 2, 3 and 4 are: "
  mapM_ putStrLn colors