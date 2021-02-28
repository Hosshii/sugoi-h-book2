module Ch9 (readArgAndName, capslocker, todoList, capslocker2, deleteTasks, respondPalindromes, readFile', todoList2, threeCoins, askForNumber, copy) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.List
import System.Directory
import System.Environment
import System.IO
import System.Random

capslocker = forever $ do
  l <- getLine
  putStrLn $ map toUpper l

capslocker2 = do
  l <- getContents
  putStrLn $ map toUpper l

shortLine = do
  contents <- getLine
  putStr $ shortLinesOnly contents

shortLinesOnly = unlines . filter (\line -> length line < 10) . lines

shortLines2 = interact shortLinesOnly

respondPalindromes = interact $ unlines . map (\xs -> if isPal xs then "palindrome" else "not palindrome") . lines

isPal xs = xs == reverse xs

readFile' = do
  handle <- openFile "hoge.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

withFile' :: FilePath -> IOMode -> (Handle -> IO c) -> IO c
withFile' name mode =
  bracket
    (openFile name mode)
    hClose

readFile'' = do
  contents <- readFile "hoge.txt"
  putStrLn contents

todoList = do
  putStrLn "put todo item"
  todoItem <- getLine
  appendFile "todo.txt" (todoItem ++ "\n")

deleteTasks = do
  contents <- readFile "todo.txt"
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStrLn "These are your Todo items:"
  mapM_ putStrLn numberedTasks
  numberString <- getLine
  let number = read numberString
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks

  bracketOnError
    (openTempFile "." "temp")
    ( \(tmpName, tmpHandle) -> do
        hClose tmpHandle
        removeFile tmpName
    )
    ( \(tmpName, tmpHandle) -> do
        hPutStr tmpHandle newTodoItems
        hClose tmpHandle
        removeFile "todo.txt"
        renameFile tmpName "todo.txt"
    )

readArgAndName = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The args are:"
  mapM_ putStrLn args

  putStrLn "The program name is:"
  putStrLn progName

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch command = doesNotExist command

add [fileName, todoItem] = appendFile fileName $ todoItem ++ "\n"
add _ = putStrLn "add command takes exactly two arguments."

view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks

  putStrLn $ unlines numberedTasks
view _ = putStrLn "view command takes exactly one arguments."

remove [fileName, numberString] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  let number = read numberString
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks

  bracketOnError
    (openTempFile "." "temp")
    ( \(tmpName, tmpHandle) -> do
        hClose tmpHandle
        removeFile tmpName
    )
    ( \(tmpName, tmpHandle) -> do
        hPutStr tmpHandle newTodoItems
        hClose tmpHandle
        removeFile "todo.txt"
        renameFile tmpName "todo.txt"
    )
remove _ = putStrLn "view command takes exactly one arguments."

doesNotExist command _ =
  putStrLn $ "The " ++ command ++ " command doesn't exist."

todoList2 = do
  ipt <- getArgs
  let (command, argList) = case ipt of
        (x : xs) -> (x, xs)
        _ -> error "invalid arguments"

  dispatch command argList

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, newGen'') = random newGen'
   in (firstCoin, secondCoin, thirdCoin)

askForNumber gen = do
  let (randomNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
  putStrLn "Which number in the range from 1 to 10 am I thinking of ? "
  numberString <- getLine
  unless (null numberString) $ do
    let number = read numberString
    if randomNumber == number
      then putStrLn "You are correct!"
      else putStrLn $ "Sorry, it was " ++ show randomNumber
    askForNumber newGen

copy = do
  (src : dest : _) <- getArgs
  copy' src dest

copy' source dest = do
  contents <- B.readFile source
  bracketOnError
    (openTempFile "." "tmp")
    ( \(tmpName, tmpHandle) -> do
        hClose tmpHandle
        removeFile tmpName
    )
    ( \(tmpName, tmpHandle) -> do
        B.hPutStr tmpHandle contents
        hClose tmpHandle
        renameFile tmpName dest
    )