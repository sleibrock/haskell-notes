-- Eliminate consecutive duplicates from a list

We need to remove duplicates from a list without breaking the
ordering of the original list. Such that

[a, b, b, c, d, d, a] => [a, b, c, d, a]


With some pattern matching we can accomplish this

compress (x:[])   -- nothing to compare/swap
compress (x:y:[]) -- two elements to check for duplication
compress (x:y:zs) -- two elements and a tail list to check for

When comparing two elements, just check if they are equal and swap
In the last case, if x and y match, remove either x or y from the
list, and re-apply the function to the remainder (compress (x:zs)).
If they are not equal, push x to the front and apply the compress
function to the rest of the list ([x] ++ compress (y:zs))

> compress :: (Eq a) => [a] -> [a]
> compress (x:[])   = [x]
> compress (x:y:[]) = if x == y then [x] else [x, y]
> compress (x:y:zs) = if x == y then compress (x:zs) else [x] ++ compress (y:zs)


> main :: IO ()
> main = do
>   putStrLn $ show $ compress "abc"
>   putStrLn $ show $ compress "aabbcc"
>   putStrLn $ show $ compress "aaaabccaadeeee"


-- end
