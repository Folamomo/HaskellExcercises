module Tree where

data Tree a = Tree {left :: Tree a, right :: Tree a, value::a}
            | Leaf deriving Show


insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf a = Tree Leaf Leaf a
insert (Tree l r b) a
            | a < b = Tree (insert l a) r b
            | otherwise = Tree l (insert r a) b

insertAll :: (Ord a) => Tree a -> [a] -> Tree a
insertAll = foldl insert

testTree = insertAll Leaf [1, 2, 5, 7, 3, 12, 13, 4, 5, 6, 4, 6]

empty :: Tree a -> Bool
empty Leaf = True
empty Tree {} = False

isBinary :: (Ord a) => Tree a -> Bool
isBinary Leaf = True
isBinary (Tree Leaf Leaf a) = True
isBinary (Tree l@(Tree _ _ b) Leaf a) = b <= a && isBinary l
isBinary  (Tree Leaf r@(Tree _ _ b) a) = a <= b && isBinary r
isBinary  (Tree l@(Tree _ _ b) r@(Tree _ _ c) a) = a <= b && b <= c && isBinary r && isBinary l


search :: (Ord a) => Tree a -> a -> Bool
search Leaf a = False
search (Tree l r b) a
            | a == b = True
            | a < b = search l a
            | otherwise = search r a

traverseLVR :: Tree a -> [a]
traverseLVR Leaf = []
traverseLVR (Tree l r a) = traverseLVR l ++ a : traverseLVR r

merge :: (Ord a) => Tree a -> Tree a -> Tree a
merge l r = foldl insert r $ traverseLVR l

remove :: (Ord a) => Tree a -> a -> Tree a
remove Leaf a = Leaf
remove t@(Tree left right b) a
            | a < b = Tree (remove left a) right b
            | a > b = Tree left (remove right a) b
            | a == b = pop t

pop :: Tree a -> Tree a
pop Leaf = Leaf
pop (Tree left Leaf _) = left
pop (Tree Leaf right _) = right
pop (Tree left right _) = Tree (pop left) right (value left)

min' :: Tree a -> a
min' (Tree Leaf _ a) = a
min' (Tree left _ _) = min' left

