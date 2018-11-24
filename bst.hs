module BST where

data BST a = EmptyLeaf 
           | Node (BST a) a (BST a) deriving Show

-- insert value (create, update)
bstInsert :: (Ord a) => BST a -> a -> BST a
bstInsert EmptyLeaf a = Node EmptyLeaf a EmptyLeaf
bstInsert (Node l a r) b 
    | b == a = Node l a r
    | b > a  = Node l a (bstInsert r b)
    | b < a  = Node (bstInsert l b) a r

-- search for value (read)
bstSearch :: (Ord a) => BST a -> a -> Bool
bstSearch EmptyLeaf a = False
bstSearch (Node l a r) b
    | b == a = True
    | b /= a = (bstSearch l b) || (bstSearch r b)

-- remove value (delete)
bstPopLargest :: (Ord a) => BST a -> (BST a, a)
bstPopLargest b =
    case b of 
        EmptyLeaf          -> error "Can't pop from empty tree"
        Node EmptyLeaf a EmptyLeaf -> (EmptyLeaf, a)
        Node l a EmptyLeaf -> (l, a)
        Node l b r         -> (Node l b r', x) where 
            (r', x) = bstPopLargest(r)

-- convert in-order to list

-- convert pre-order to list

-- convert post-order to list

-- convert list to BST
