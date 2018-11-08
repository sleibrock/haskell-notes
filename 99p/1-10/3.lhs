-- find the element at the K-th index

Our function accepts now a list and an integer
indicating the position of the element we want
to retrieve.


> elementAt :: [a] -> Int -> a

So long as the integer N is above one, we keep recursing
on the tail end of the list. When it reaches zero, we've found
our element. But if we reach the end of the list before it hits
zero, then we need to error because we're out of bounds.

> elementAt (x:_) 1 = x
> elementAt (_:xs) n = elementAt xs $ pred n 
> elementAt [] n = error "Empty list!"


> main :: IO ()
> main = do
>   putStrLn $ show $ elementAt [1,2,3] 2
>   putStrLn $ show $ elementAt "Haskell" 5


-- end
