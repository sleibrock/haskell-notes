-- Determine if a given integer is a Prime number

A number is prime if the only numbers that divide it evenly
are the number itself and the number 1. Zero and One are not prime.
In order to check if a number is prime, you must check all numbers
less than that and see if any numbers divide it evenly. Here we use
`mod` but you can also subsitute `rem` as well.


> isPrime :: (Integral a) => a -> Bool

This function will determine if the list of all modulos of numbers
up to N is equal to exactly 0 elements (no even divisors between 2..n-1)

> isPrime n
>   | n <= 1    = False
>   | otherwise = [] == filter (==0) [mod n x | x <- [2..(n-1)]]

> main :: IO ()
> main = do
>   putStrLn "Is 7 Prime?"
>   putStrLn $ show $ isPrime 7
>   putStrLn "Is 13 Prime?"
>   putStrLn $ show $ isPrime 13
>   putStrLn "Is 15 prime?"
>   putStrLn $ show $ isPrime 15
>   putStrLn "Is 4057 prime?"
>   putStrLn $ show $ isPrime 4057

Further possible optimizations: Sieve of Eratosthenes, checking numbers
up to sqrt(n) instead, primality/probabilistic tests, etc

-- end
