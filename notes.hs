{-
    `bstVerify` checks that a BST is a binary search tree
    In O(n) time and O(1) space
    The algorithm outlined in this coude would be useful if 
    we had a binary tree type that was not already a BST. 

    This code was developed with reference to a Stack Overflow discussion
    "Determine if binary tree is BST haskell"
    Answer by Willem Van Onsem
    Available at <https://stackoverflow.com/a/48365455>
-}

bstVerify :: (Ord a) => BST a -> Bool
bstVerify b = bstVerify' b Nothing Nothing

bstVerify' :: (Ord a) => BST a -> Maybe a -> Maybe a -> Bool
bstVerify' EmptyLeaf low upp = True
bstVerify' (Node l a r) low upp = 
    checkBound (<) low a &&
    checkBound (>) upp a &&
    bstVerify' l low (Just a) &&
    bstVerify' r (Just a) upp 

checkBound :: (a -> a -> Bool) -> Maybe a -> a -> Bool
checkBound _ Nothing _ = True
checkBound f (Just b) x = f b x
