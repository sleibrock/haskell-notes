-- Reverse a list

Using more pattern matching we can accomplish this.
If our list is 0 elements, nothing
If our list is 1 elements, return that list
If our list is 2+ elements, take the second element and reverse
it's position.

(This is not an efficient way of reversing, but it's
very simple with pattern matching alone)


> myReverse :: [a] -> [a]
> myReverse [] = []
> myReverse (x:[]) = [x]
> myReverse (x:xs) = myReverse xs ++ [x]


> main :: IO ()
> main = do
>   putStrLn $ show $ myReverse "Hello, World!"
>   putStrLn $ show $ myReverse [1..10]
