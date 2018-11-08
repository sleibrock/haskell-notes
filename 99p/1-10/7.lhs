-- Flatten a nested list into one singular list

We need a special data type for this because lists in
Haskell are homogenous (one-type only).

> data NestedList a = Elem a | List [NestedList a]

Since we have two different types of NestedList, one with a special
list variant, we can developt a pattern match for this to flatten it
into a singular list of [a].

flatten (Elem x) => [x]      -- a singular element
flatten (List []) => []      -- a list with nothing
flatten (List (Elem x))      -- a list with a single element
flatten (List (Elem x, ...)) -- a list with more than one elements

We can match these patterns to compress a list into a single list

> flatten :: NestedList a -> [a]
> flatten (Elem x) = [x]
> flatten (List []) = []
> flatten (List (x:[])) = flatten x 
> flatten (List (x:xs)) = flatten x ++ flatten (List xs)


> main :: IO ()
> main = do
>   putStrLn $ show $ flatten (Elem 5)
>   putStrLn $ show $ flatten (List [Elem 1, Elem 2])
>   putStrLn $ show $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])


-- end
