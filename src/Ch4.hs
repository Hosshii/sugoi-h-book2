maximum' [] = error "cannot use empty list."
maximum' [x] = x
maximum' (x : xs) = max x $ maximum' xs

replicate' a b
  | a <= 0 = []
  | otherwise = b : replicate' (a -1) b

take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n -1) xs

reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

repeat' x = x : repeat' x

zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' _ [] = False
elem' a (x : xs)
  | a == x = True
  | otherwise = elem' a xs

quicksort [] = []
quicksort (x:xs) = 
  let smallOrEqual = [a|a<-xs,a<=x ]
      larger = [a|a<-xs,a>x]
  in quicksort smallOrEqual ++ [x] ++ quicksort larger 