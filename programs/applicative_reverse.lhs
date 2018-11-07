-- Reverse a line of text from stdio in one line of code

For this program we will be using two functions from the Monad collection
forever :: Applicative f => f a -> f b
liftM :: Monad m => (a1 -> r) -> m a1 -> m r 

> import Control.Monad       (forever, liftM)

Let's pretend we have a program that takes lines of input from STDIN,
reverses each line, then sends it back to STDOUT. In order to do this,
we need getLine, putStrLn, and reverse.

getLine :: IO String will get a line of text from STDIN
reverse :: [a] -> [a] will reverse a given list
putStrLn :: String -> IO () will print a string to STDOUT

Using getLine and putStrLn is easy, the problem is reversing this line
of text before printing it. It's because of the monadic type IO that
restricts normal pure operation on the given String.

> -- example getLine >>= putStrLn program 
> notMain :: IO ()
> notMain = forever $ do
>   getLine >>= putStrLn


Something like the following works because of how >>= will apply
the function to the left-hand side.

(>>=) :: Monad m => m a -> (a -> m b) -> m b

In the case of getLine/putStrLn, doing getLine >>= putStrLn looks like

(>>=) :: IO String -> (String -> IO ()) -> IO ()

but because reverse is pure, it makes it hard to wedge it in there somewhere.
Because getLine returns an (IO String) and not just a normal String it makes it hard.
Doing a reverse in a non-oneliner fashion is trivial however using do-> notation

> --do-> notation to reverse lines of text
> notMain2 :: IO ()
> notMain2 = forever $ do
>   line <- getLine;
>   putStrLn $ reverse line

Using do-> arrows allows you to extract the type from a monad, so it becomes
a normal string that we can pass to putStrLn very easily. However, there's a
way we can do this without using a do-> arrow.

An interesting function we can use is called liftM, which allows us to
"lift" data out of a given monad and apply a function to the data.

liftM :: Monad m => (a1 -> r) -> m a1 -> m r

It takes a function that applies a very basic computation, a monad with a value,
and outputs the same monad with a final result.

If we try to apply a reverse to getLine, it looks like

reverse :: [a] -> [a]
liftM reverse getLine :: ([a] -> [a]) -> IO String -> IO String

We get back an IO monad with a String that should be reversed, so the final
line of code will look like the following (parentheses added to show breakdown)

(liftM reverse getLine) >>= (putStrLn)
  
(liftM reverse getLine)                       (putStrLn)        (final)
(([a] -> [a]) -> IO String -> IO String) -> (String -> IO ()) -> IO ()


> -- our liftM-based reverse text one-liner
> main :: IO ()
> main = forever $ do
>   liftM reverse getLine >>= putStrLn


Take-away notes:
- anything that takes user input or handles output is bound to an IO monad
- in order to extract data from monads, you "lift" the data from it
- pure functionality should be separate from impure functionality for better
  program design

-- end








