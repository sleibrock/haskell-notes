-- Check if a list is a palindrome

Using our solution from problem 5, we check if a given
list is a palindrome or not. We need to implement a typeclass 'Eq'
here to make sure we accept a list of values implementing the Equals
typeclass.

> isPalindrome :: (Eq a) => [a] -> Bool

Doing this involves checking the equality of a list
with itself when reversed. For example:

isPalindrome 121   => true
isPalindrome 12321 => true
isPalindrome "abc" => false

Let's redefine our reverse function

> myReverse :: [a] -> [a]
> myReverse [] = []
> myReverse (x:[]) = [x]
> myReverse (x:xs) = myReverse xs ++ [x]

Then define our palindrome function

> isPalindrome [] = True   -- an empty list is a palindrome?
> isPalindrome (x:[]) = True -- one element lists are palindromes
> isPalindrome xs = xs == myReverse xs -- check if it equals the reverse


> main :: IO ()
> main = do
>   putStrLn $ show $ isPalindrome [1,2,3]
>   putStrLn $ show $ isPalindrome "racecar"
>   putStrLn $ show $ isPalindrome [1,2,4,8,16,8,4,2,1]


-- end



