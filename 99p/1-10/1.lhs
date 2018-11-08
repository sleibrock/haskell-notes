-- Find the last element of a list

Using Haskell pattern matching, we can define a
recursive function that fits all possible cases for
finding the last element in a list

myLast (x:[]) = x -- list is at it's last element because of the
                     trailing list at the tail
myLast (x:xs) = myLast xs -- recurse by applying myLast to the tail

> myLast :: [a] -> a
> myLast (x:[]) = x
> myLast (x:xs) = myLast xs


Now we can test it on some lists

> main :: IO ()
> main = do
>   putStrLn $ show $ myLast [1, 2, 3]
>   putStrLn $ show $ myLast [1 .. 10]

-- end

