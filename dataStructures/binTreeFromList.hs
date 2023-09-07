module BinTree where

{-
  here we introduce fromList function, which converts list representation of nodes to binary tree structure
  the nodes is filled level by level with consecutive elements of the list, where Nothing represents a leaf (empty node)

  e.g. [1, 2, 3, Nothing, 4, 5, 6, 7] ~ [[1], [2,3], [empty, 4, 5, 6], [7, empty, empty, empty, empty, empty], [empty, empty]]
                                    |
                                    |
                                   \|/

                                    1
                                   / \
                                  /   \
                                 2     3
                                /\    / \
                               .  4  5   6
                                 /\  /\  /\
                                7 . . . . .
                               /\
                              . .
-}

import Data.Sequence (Seq (Empty, (:<|)), singleton, (<|), (|>))

data Tree a = Node a (Tree a) (Tree a) | Leaf deriving (Show)

data Dir = L | R

type Path = Seq Dir

insert :: a -> Path -> Tree a -> Tree a
insert x Empty _ = Node x Leaf Leaf
insert _ (d :<| ds) Leaf = error "Invalid path"
insert x (L :<| ds) (Node y l r) = Node y (insert x ds l) r
insert x (R :<| ds) (Node y l r) = Node y l (insert x ds r)

fromList :: [Maybe a] -> Tree a
fromList [] = Leaf
fromList (Nothing : xs) = error "Root node cannot be nothing"
fromList ((Just x) : xs) =
  let tree = Node x Leaf Leaf
      dirs = singleton L <| singleton R <| Empty
   in snd $ foldl aux (dirs, tree) xs
  where
    aux (d :<| ds, tree) Nothing = (ds, tree)
    aux (d :<| ds, tree) (Just x) = (ds |> (d |> L) |> (d |> R), insert x d tree)
    aux _ _ = error "missing path for element insertion"

example1 :: [Maybe Char]
example1 = [Just 'a', Just 'b', Just 'c', Nothing, Just 'e', Just 'f', Nothing]

example1T :: Tree Char -- fromList example1
example1T =
  Node
    'a'
    ( Node
        'b'
        Leaf
        (Node 'e' Leaf Leaf)
    )
    ( Node
        'c'
        (Node 'f' Leaf Leaf)
        Leaf
    )

example2 :: [Maybe Int]
example2 = [Just 10, Just 5, Just 15, Just 2, Just 6, Just 12, Nothing, Nothing, Just 3, Nothing, Just 8, Just 11]

example2T :: Tree Int -- fromList example2
example2T =
  Node
    10
    ( Node
        5
        ( Node
            2
            Leaf
            (Node 3 Leaf Leaf)
        )
        ( Node
            6
            Leaf
            (Node 8 Leaf Leaf)
        )
    )
    ( Node
        15
        ( Node
            12
            (Node 11 Leaf Leaf)
            Leaf
        )
        Leaf
    )