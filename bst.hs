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

-- remove (delete)

-- convert BST to list

-- convert list to BST
