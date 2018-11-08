-- find the second to last element in a list

Same as problem 1, we find the second-to-last element.
We can use more pattern matching for this by adding
another matching variable


> myButLast :: [a] -> a
> myButLast (x:y:[]) = x   -- found the second-to-last
> myButLast (x:y:zs) = myButLast zs   -- recurse
> myButLast []       = error "Woops!"


> main :: IO ()
> main = do
>   putStrLn $ show $ myButLast [1, 2, 3, 4]
>   putStrLn $ show $ myButLast ['a' .. 'z']


-- end
