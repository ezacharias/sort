-- A tail-call optimized sort.
-- This appears to be roughly 30% faster than Data.List.sortBy.

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp xs = merge xs []
  where
    -- merge :: [a] -> [a] -> [a]
    merge [] ts = ts
    merge (x:xs) ts = split x xs [] [] ts
    -- split :: a -> [a] -> [a] -> [a] -> [a] -> [a]
    split a [] ys zs ts = merge ys $ a : merge zs ts
    split a (x:xs) ys zs ts
      | cmp x a == LT = split a xs (x:ys) zs ts
      | otherwise = split a xs ys (x:zs) ts
