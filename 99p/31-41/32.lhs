-- Find the greatest common divisor between two numbers

The greatest common divisor is the largest number that
is considered the largest divisor between two numbers A and B.

> myGcd :: (Integral a) => a -> a -> a


Using Euclid's algorithm for finding the GCD, what it boils down
to is applying repeated modulo math and swapping the arguments
until either A or B is zero. The basic 3 rule pattern below
will execute Euclid's algorithm recursively.

> myGcd a 0 = a
> myGcd 0 b = b
> myGcd a b = gcd b (mod a b)


> main :: IO ()
> main = do
>   putStr "GCD(36, 63) = "
>   putStrLn $ show $ myGcd 36 63
>   putStr "GCD(-3, -6) = "
>   putStrLn $ show $ myGcd (-3) (-6)
>   putStr "GCD(-3, 6) = "
>   putStrLn $ show $ myGcd (-3) 6

-- end
