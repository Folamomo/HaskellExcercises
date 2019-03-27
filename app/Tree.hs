module Tree where

data Tree a = Tree {left :: Tree a, right :: Tree a, value::a}
            | Leaf deriving Show

insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf a = Tree Leaf Leaf a
insert (Tree l r b) a
            | a < b = Tree (insert l a) r b
            | otherwise = Tree l (insert r a) b

empty :: Tree a -> Bool
empty Leaf = True
empty (Tree _ _ _) = False

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