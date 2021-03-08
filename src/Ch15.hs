module Ch15 where

import Data.List (break)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree =
  Node
    'P'
    ( Node
        'O'
        ( Node
            'L'
            (Node 'N' Empty Empty)
            (Node 'T' Empty Empty)
        )
        ( Node
            'Y'
            (Node 'S' Empty Empty)
            (Node 'A' Empty Empty)
        )
    )
    ( Node
        'L'
        ( Node
            'W'
            (Node 'C' Empty Empty)
            (Node 'R' Empty Empty)
        )
        ( Node
            'A'
            (Node 'A' Empty Empty)
            (Node 'C' Empty Empty)
        )
    )

-- changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)

data Direction = L | R deriving (Show)

type Directions = [Direction]

-- type Breadcrumbs = [Direction]
type Breadcrumbs a = [Crumb a]

type Zipper a = (Tree a, Breadcrumbs a)

changeToP (L : ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R : ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r

elemAt (L : ds) (Node _ l _) = elemAt ds l
elemAt (R : ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

goLeft (Node x l r, bs) = Just (l, LeftCrumb x r : bs)
goLeft (Empty, _) = Nothing

goRight (Node x l r, bs) = Just (r, RightCrumb x l : bs)
goRight (Empty, _) = Nothing

goUp (t, LeftCrumb x r : bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l : bs) = Just (Node x l t, bs)
goUp (_, []) = Nothing

modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Maybe (Zipper a)
topMost (t, []) = Just (t, [])
topMost z = goUp z >>= topMost

data Crumb a
  = LeftCrumb a (Tree a)
  | RightCrumb a (Tree a)
  deriving (Show)

type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x : xs, bs) = (xs, x : bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b : bs) = (b : xs, bs)

-- file system

type Name = String

type Data = String

data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

fsUp (item, FSCrumb name ls rs : bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo name (Folder folderName items, bs) =
  let (ls, item : rs) = break (nameIs name) items
   in (item, FSCrumb folderName ls rs : bs)

nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

myFile = Folder "root" [File "hosshii" "test", Folder "home" [File ".bashrc" "export PATH=\"\""]]

fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

fsNewFile item (Folder folderName items, bs) = (Folder folderName (item : items), bs)