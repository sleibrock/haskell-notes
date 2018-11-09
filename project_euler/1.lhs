-- Find sum of multiples of 3 or 5 under 1000

Sort of like a FizzBuzz but more Project Euler inspired.

Ideally we want to create a simple line of code that executes
a "sum" of all of the multiples for us. 

First let's define a "sum" function which is just a fold-left
function using an addition operator with a zero supplied.

> numsum :: (Foldable t, Integral b) => t b -> b
> numsum = foldl (+) 0

Then let's define a function that tells us whether a number is
a multiple of either 3 or 5

> threeOrFive :: (Integral a) => a -> Bool
> threeOrFive n
>   | mod n 3 == 0 = True
>   | mod n 5 == 0 = True
>   | otherwise    = False

Then we can define a function to sum up all numbers up to N
and apply the threeOrFive predicate.

> sumTo :: (Num a, Integral a) => a -> a
> sumTo n
>   | n <= 1    = 0
>   | otherwise = numsum $ filter threeOrFive [1 .. n]

Putting it all together:

> main :: IO ()
> main = do
>   putStr "Sum of multiples of 3 or 5 under 1000 = "
>   putStrLn $ show $ sumTo 1000 


-- end
