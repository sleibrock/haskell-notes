-- Check if an element exists in a list or not

> module Need where

This one is simple list recursion/pattern matching until
the list is exhausted. If the needle ever matches the head,
then terminate with True, and return with False when the list is
fully empty.

> check :: Eq a => [a] -> a -> Bool
> check (x:[]) y = if x == y then True else False
> check (x:xs) y = if x == y then True else check xs y

-- end
