-- Rotate String
https://www.hackerrank.com/challenges/rotate-string/problem

The idea is that a string can be rotated in N number ways where
rotating is appending a character from the front to the back, until
you get the original string. The number of rotations is the number
of characters in a string.

Program: design a function that will print out all rotations of a given
string.

First let's start with a definition of a rotation folding algorithm

> rofold :: Int -> [String] -> [String]

This function will accept an initial integer N and a list of Strings
Initially, the Int will be however long the given string is ("abc" is 3),
and the [String] will be a list of all variations accumulated over
the course of a full rotation.

Each time we recurse, we append a new rotated string to the [String]
list. When N reaches zero, that means we can stop rotating and
return our final list of rotated strings.

> rofold 0 acc = acc
> rofold n (x:xs) = rofold (pred n) ([[(last x)] ++ (init x)] ++ (x:xs))

This part looks a little awful, but the important part is the
use of last/init to build new rotated strings.

[(last x)] - the last character the string
(init x)   - every character but the last one as a list

(last x) returns a type Char but has to be wrapped in brackets
to convert it to a list. Therefore the string rotation looks like

[(last x)] ++ (init x)

Afterwords, it's appended to the accumulative list of rotated
strings until we finally reach zero (using (pred n) to decrement)

> main :: IO ()
> main = do
>   n <- getLine
>   lins <- getContents
>   mapM_ (putStrLn . unwords) $ map (\x -> rofold (pred (length x)) [x]) (lines lins)
    

-- end
