module KSublist where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (filterM, foldM)
import Data.List (mapAccumR, tails)

{-
reflections on computing sublists (subsets) of given size
e.g. sublists(2, [1, 2, 3]) = [[1, 2], [1, 3], [2, 3]]
order is irrelevant
-}

{-
compute powerset using filterM: for each element, we take it or not - use list monad for this purpose
we obtain 2^n results and need only a portion of that - the elements of size n - not very efficient :/
-}
sub :: Int -> [a] -> [[a]]
sub n = filter ((== n) . length) . filterM (const [True, False])

-- >>> sub 3 [1,2,3,4]
-- [[1,2,3],[1,2,4],[1,3,4],[2,3,4]]

{-
similarly, we can use monadic fold to generate both possibilities at every step
-}
sub' :: Int -> [a] -> [[a]]
sub' n = filter ((== n) . length) . foldM (\xs x -> [x : xs, xs]) []

-- >>> sub' 3 [1,2,3,4]
-- [[3,2,1],[4,2,1],[4,3,1],[4,3,2]]

{-
the benefit of this approach is that we can modify it to avoid to avoid useless computations,
add counter to start element, indicating how many elements are left to be generated for a sublist at given step
as the counter reaches 0, there is only one choice - the list generated so far itself
still, we need to filter out sublists of the demanded size (not shorter)
-}
sub'' :: Int -> [a] -> [[a]]
sub'' n = map snd . filter ((== 0) . fst) . foldM go (n, [])
  where
    go (0, xs) x = [(0, xs)]
    go (k, xs) x = [(k -1, x : xs), (k, xs)]

-- >>> sub'' 3 [1,2,3,4]
-- [[3,2,1],[4,2,1],[4,3,1],[4,3,2]]

{-
monadic approach with tails function
nondeterministically choose current element and repeat the procedure for the remaining elements
note that we don't have to worry about running out of elements
-}
sub''' :: Int -> [a] -> [[a]]
sub''' 0 _ = [[]]
sub''' n ls = do
  x : xs <- tails ls
  ts <- sub''' (n -1) xs
  return (x : ts)

-- >>> sub''' 3 [1,2,3,4]
-- [[1,2,3],[1,2,4],[1,3,4],[2,3,4]]

{-
simple approach
added case when we run out of elements - empty list represents failure (no results)
each sublist either starts with the head and has tail generated from the rest or (alternative op.) is entirely generated from the tail
<|> is same as (++) here, but has better precedence
-}
sub'''' :: Int -> [a] -> [[a]]
sub'''' 0 _ = [[]]
sub'''' n [] = []
sub'''' n (x : xs) = (x :) <$> sub'''' (n - 1) xs <|> sub'''' n xs

-- >>> sub'''' 3 [1,2,3,4]
-- [[1,2,3],[1,2,4],[1,3,4],[2,3,4]]
