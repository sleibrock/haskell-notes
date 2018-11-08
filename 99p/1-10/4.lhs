-- Find the length of a list

Finding the length of the list involves counting,
but using pattern matching we can recurse and find
that value easily without ever having to store an
accumulating variable.


> myLength :: [a] -> Int
> myLength (_:[]) = 1                -- base case
> myLength (_:xs) = 1 + myLength xs  -- recurse case


> main :: IO ()
> main = do
>   putStrLn $ show $ myLength [1, 2, 3, 4]
>   putStrLn $ show $ myLength "Hello, World!"


-- end
