
--  * Huayue Sun 
--    2021.1.19
-- Grading note: 10pts total
--  * 2pts each for encodeList and mapTree
--  * 3pts each for valueAt and pathTo
module HW2 where

-- | Binary trees with nodes labeled by values of an arbitrary type.
data Tree a
   = Node a (Tree a) (Tree a)
   | End
  deriving (Eq,Show)

-- | One step in a path, indicating whether to follow the left subtree (L)
--   or the right subtree (R).
data Step = L | R
  deriving (Eq,Show)

-- | A path is a sequence of steps. Each node in a binary tree can be
--   identified by a path, indicating how to move down the tree starting
--   from the root.
type Path = [Step]

-- | Create a leaf node.
leaf :: a -> Tree a
leaf x = Node x End End

-- | An example tree.
ex :: Tree Int
ex = Node 4 (Node 3 (leaf 2) End)
            (Node 7 (Node 5 End (leaf 6))
                    (leaf 8))


-- | Encode a list as a tree with only right branches.
--
--   >>> encodeList []
--   End
--
--   >>> encodeList [1,2,3,4]
--   Node 1 End (Node 2 End (Node 3 End (Node 4 End End)))
--
--   >>> encodeList ":-D"
--   Node ':' End (Node '-' End (Node 'D' End End))
--
--encodeList = undefined
encodeList ::[a] -> Tree a
encodeList [] = End
encodeList (h:t) = Node h End (encodeList t)

-- | Map a function over a tree. Applies the given function to every label
--   in the tree, preserving the tree's structure.
--   
--   >>> mapTree odd End
--   End
--
--   >>> mapTree even (Node 5 (leaf 2) End)
--   Node False (Node True End End) End
--
--   >>> (mapTree not . mapTree even) (Node 5 End (leaf 2))
--   Node True End (Node False End End)
--
--   >>> mapTree (+10) ex
--   Node 14 (Node 13 (Node 12 End End) End) (Node 17 (Node 15 End (Node 16 End End)) (Node 18 End End))
--
--   >>> ex == (mapTree (subtract 27) . mapTree (+27)) ex
--   True
--
--mapTree = undefined
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree s End = End
mapTree s (Node x l r) = Node (s x) (mapTree s l) (mapTree s r)


-- | Get the value at the node specified by a path. Returns 'Nothing' if
--   the given path is invalid.
--
--   >>> valueAt [] ex
--   Just 4
--
--   >>> valueAt [L,L] ex
--   Just 2
--
--   >>> valueAt [L,R] ex
--   Nothing
--
--   >>> valueAt [R,L,R] ex
--   Just 6
--
--   >>> valueAt [L,L,L] ex
--   Nothing
--   
--   >>> valueAt [R,L,L] ex
--   Nothing
--
--   >>> valueAt [R,R] ex
--   Just 8
--
--   >>> valueAt [R,L] ex
--   Just 5


--valueAt = undefined
valueAt :: Path -> Tree a -> Maybe a
valueAt _ End = Nothing
valueAt []    (Node x _ _) = Just x
valueAt (L:u) (Node _ l _) = valueAt u l
valueAt (R:u) (Node _ _ r) = valueAt u r


-- | Find a path to a node that contains the given value.
--
--   >>> pathTo 3 (leaf 5)
--   Nothing
--
--   >>> pathTo 5 ex
--   Just [R,L]
--
--   >>> pathTo 6 ex
--   Just [R,L,R]
--
--   >>> pathTo 4 ex
--   Just []
--
--   >>> pathTo 10 ex
--   Nothing
--
--   >>> pathTo 2 ex
--   Just [L,L]
--
--   >>> pathTo 8 ex
--   Just [R,R]
--
--pathTo = undefined
pathTo :: Eq a => a -> Tree a -> Maybe Path
pathTo x End = Nothing
pathTo x (Node y l r)
  | x == y = Just []
  | otherwise = case pathTo x l of
      Just u  -> Just (L:u)
      Nothing -> case pathTo x r of
        Just u  -> Just (R:u)
        Nothing -> Nothing