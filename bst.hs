module BST where

data BST a = EmptyLeaf | Node (BST a) a (BST a) deriving Show

-- insert (create, update)
bstInsert :: (Ord a) => BST a -> a -> BST a
bstInsert EmptyLeaf a = Node EmptyLeaf a EmptyLeaf
bstInsert (Node l a r) b 
    | b == a = Node l a r
    | b > a  = Node l a (bstInsert r b)
    | b < a  = Node (bstInsert l b) a r

-- search (read)
bstSearch :: (Ord a) => BST a -> a -> Bool
bstSearch EmptyLeaf a = False
bstSearch (Node l a r) b
    | b == a = True
    | b /= a = (bstSearch l b) || (bstSearch r b)

-- check if a binary tree is a BST
largestChild :: (Ord a) => BST a -> a
largestChild (Node EmptyLeaf a EmptyLeaf) = a
largestChild (Node l a EmptyLeaf) = a
largestChild (Node EmptyLeaf a r) = largestChild r
largestChild (Node l a r) = largestChild r


-- remove (delete)

-- convert BST to list

-- convert list to BST
