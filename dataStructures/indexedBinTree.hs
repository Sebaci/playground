module IndexedBinTree (BinTree, getElem, getElemSafe) where

{-
Examples of getElem and getElemSafe functions returning k-th (counting from 0) element in foldables and traversables via state monad, respectively
Relevant instances for binary tree are defined below.
For binary tree, the operation returns k-th element as visualized left to right
In case of BST, it means we get k-th element in order
-}

import Control.Monad.State (MonadState (get, put), State, execState, modify)

getElem :: Foldable t => Int -> t a -> a
getElem index tree = foldMap (: []) tree !! index

getElemSafe :: Traversable t => Int -> t Int -> Maybe Int
getElemSafe k t = snd $ execState (traverse go t) ((0, k), Nothing)
  where
    go :: Int -> State ((Int, Int), Maybe Int) ()
    go cur = modify (\((i, k), v) -> ((i + 1, k), if i /= k then v else Just cur))

-- Binary tree

data BinTree a = Node (BinTree a) a (BinTree a) | Leaf deriving (Show)

instance Foldable BinTree where
  foldMap _ Leaf = mempty
  foldMap f (Node left x right) = foldMap f left <> f x <> foldMap f right

instance Functor BinTree where
  fmap _ Leaf = Leaf
  fmap f (Node left x right) = Node (fmap f left) (f x) (fmap f right)

instance Traversable BinTree where
  traverse _ Leaf = pure Leaf
  traverse f (Node left x right) = Node <$> traverse f left <*> f x <*> traverse f right

tree :: BinTree Int
tree = Node (Node Leaf 3 Leaf) 5 (Node Leaf 7 Leaf)

{-
>>> getElem 1 tree
5

>>> getElemSafe 1 tree
Just 5
-}
