-- Pack consecutive duplicates into sublists

(WIP)


Similar to duplicate consecutives, except this time
we compress all duplicates into sub-lists.

> pack :: (Eq a) => [a] -> [[a]]

With some more pattern matching we can accomplish this

pack []       -- nothing to pack
pack (x:[])   -- returns a single [x]
pack (x:y:[]) -- compares and either groups or ends grouping
pack (x:y:zs) -- compares, groups, and continues


The idea will be to hold onto a single element and create
a grouping list until we discover a new element.


> pack [] = []
> pack (x:y:zs) = if x == y then [[x] ++ compress (y:zs)] else [[x]] ++ compress (y:zs)



> main :: IO ()
> main = do
>   putStrLn $ show $ pack [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]
>   putStrLn $ show $ pack ['a','a','a','a','b','c','c','a','a','d','e','e','e','e']

-- end
