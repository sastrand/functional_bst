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
        Node EmptyLeaf a EmptyLeaf -> (EmptyLeaf, a)
        Node l a EmptyLeaf -> (l, a)
        Node l b r         -> (Node l b r', x) where 
            (r', x) = bstPopLargest(r)

bstRemove :: (Ord a) => BST a -> a -> BST a
bstRemove EmptyLeaf a = EmptyLeaf
bstRemove (Node l a r) b
    | b > a  = Node l a (bstRemove r b)
    | b < a  = Node (bstRemove l b) a r
    | b == a = case (l, r) of
        (EmptyLeaf, _) -> r 
        (_, EmptyLeaf) -> Node l' x EmptyLeaf
        (_, _)   -> Node l' x r
  where 
    (l', x) = bstPopLargest(l)

-- convert in-order to list
bstInOrder :: (Ord a) => BST a -> [a]
bstInOrder t =
    case t of 
        EmptyLeaf -> []
        Node EmptyLeaf a EmptyLeaf -> [a]
        Node l a EmptyLeaf -> bstInOrder(l) ++ [a]
        Node EmptyLeaf a r -> bstInOrder(r) ++ [a]
        Node l a r -> bstInOrder(l) ++ [a] ++ bstInOrder(r)

-- convert pre-order to list
bstPreOrder :: (Ord a) => BST a -> [a]
bstPreOrder t =
    case t of 
        EmptyLeaf -> []
        Node EmptyLeaf a EmptyLeaf -> [a]
        Node l a EmptyLeaf -> [a] ++ bstPreOrder(l)
        Node EmptyLeaf a r -> [a] ++ bstPreOrder(r)
        Node l a r -> [a] ++ bstPreOrder(l) ++ bstPreOrder(r)

-- convert post-order to list
bstPostOrder :: (Ord a) => BST a -> [a]
bstPostOrder t =
    case t of 
        EmptyLeaf -> []
        Node EmptyLeaf a EmptyLeaf -> [a]
        Node l a EmptyLeaf -> [a] ++ bstPostOrder(l)
        Node EmptyLeaf a r -> [a] ++ bstPostOrder(r)
        Node l a r -> bstPostOrder(l) ++ bstPostOrder(r) ++ [a]

-- convert list to BST
bstCreateFromList :: (Ord a) => [a] -> BST a
bstCreateFromList l = 
    case l of
        [] -> EmptyLeaf
        x:[] -> Node EmptyLeaf x EmptyLeaf
        x:xs -> foldl bstInsert (Node EmptyLeaf x EmptyLeaf) xs

