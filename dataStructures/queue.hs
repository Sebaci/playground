{-# LANGUAGE NoImplicitPrelude #-}

{-
FIFO queue based on two lists with O(1) head, snoc, tail operations (amortized)
invariant: front part is not longer than the back part
note that reversal of the back part is not forced unless the front is empty
see BankersQueue in Purely Functional Data Structures by Chris Okasaki
-}

module Queue (Queue, empty, isEmpty, snoc, head, tail) where

import Prelude (Bool (False, True), Foldable (foldl), Integer, Num ((+), (-)), String, error, otherwise, reverse, ($), (++), (<=))

data Queue a = Queue {lenf :: Integer, front :: [a], lenb :: Integer, back :: [a]}

empty :: Queue a
empty = Queue 0 [] 0 []

isEmpty :: Queue a -> Bool
isEmpty (Queue 0 [] 0 []) = True
isEmpty _ = False

check :: Queue a -> Queue a
check q@(Queue lenf front lenb back)
  | lenb <= lenf = q
  | otherwise = Queue (lenf + lenb) (front ++ reverse back) 0 []

snoc :: Queue a -> a -> Queue a
snoc (Queue lenf front lenb back) x = check $ Queue lenf front (lenb + 1) (x : back)

head :: Queue a -> a
head (Queue _ [] _ _) = error "head on empty queue"
head (Queue _ (f : front) _ _) = f

tail :: Queue a -> Queue a
tail (Queue _ [] _ _) = error "tail on empty queue"
tail (Queue lenf (f : front) lenb back) = check $ Queue (lenf - 1) front lenb back
