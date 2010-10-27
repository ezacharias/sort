-- This is an attempt at a fully tail-calling version of sortBy.  I still get a
-- stack space overflow on large lists (> 1200000 elements) and I don't know
-- how to get rid of it.

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp xs = merge [] xs []
  where
    -- merge :: [(a, [a])] -> [a] -> [a] -> [a]
    merge [] [] ts = ts
    merge ((a,s):ss) [] ts = merge ss s $! (a:ts)
    merge ss (x:xs) ts = split ss x xs [] [] ts
    -- split :: [(a, [a])] -> a -> [a] -> [a] -> [a] -> [a] -> [a]
    split ss a [] ys zs ts = (merge $! (((:) $! (a,ys)) ss)) zs ts
    split ss a (x:xs) ys zs ts
      | cmp x a == LT = (split ss a xs $! (x:ys)) zs ts
      | otherwise = (split ss a xs ys $! (x:zs)) ts
