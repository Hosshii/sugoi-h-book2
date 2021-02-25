module Ch3 (lucky, sayMe) where

lucky :: Int -> String
lucky 7 = "Lucky num"
lucky x = show x ++ "not lucky num"

sayMe :: Int -> [Char]
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe x = "Not between 1 and 2"