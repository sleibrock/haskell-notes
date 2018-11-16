-- A Linked list impl

A linked list consists of a Cell
that holds a value and a reference to another cell
or it can be Nil, meaning end-of-list
example:
    let myList = Cons 5 (Cons 7 (Cons 9 Nil))


> data List a = Nil
>               | Cons a (List a)
>                deriving (Show, Eq)

myHead is the simplest, it pops the first value off the list
and returns it. It does not work for Nil lists because
it would cause a different type signature
(List a -> List a instead of List a -> a)

> myHead :: List a -> a
> myHead (Cons a _) = a

myTail returns the "cdr" of a given list. This will work for
Nil lists because "Nil" itself is a tail end of a list.

> myTail :: List a -> List a
> myTail Nil       = Nil
> myTail (Cons _ b) = b

myLast returns the last element of a list. Does not
work for Nil lists, requires at least one Cons element

> myLast :: List a -> a
> myLast (Cons a Nil) = a
> myLast (Cons _ b)    = myLast b

myLength counts the length of a list by recursively
adding 1 + (length (tail list))

> myLength :: List a -> Int
> myLength Nil       = 0
> myLength (Cons a b) = 1 + (myLength b)

myIndex will fetch the nth element of a list
(if it can even find it)

> myIndex :: Int -> List a -> a
> myIndex 0 (Cons a _) = a
> myIndex n Nil        = Nil
> myIndex n b          = myIndex (pred n) b

Generates a list of nums from [N..0]
It's reversed because of the recursive nature

> myRange :: Int -> List Int
> myRange 0 = Nil
> myRange n = Cons n (myRange (pred n))

myTake will take the first N elements of a list and return
it, while dropping the remaining elements after it

> myTake :: Int -> List a -> List a
> myTake 0 _             = Nil
> myTake n (Cons a Nil)  = Cons a Nil
> myTake n (Cons a b)    = Cons a (myTake (pred n) b)

myDrop will perform the inverse of myTake, dropping the
first N elements of a list and returning the remainder

> myDrop :: Int -> List a -> List a
> myDrop 0 b          = b
> myDrop n (Cons _ b) = myDrop (pred n) b

myAppend will join two lists together by cons'ing the
second list at the end of the first list

> myAppend :: List a -> List a -> List a
> myAppend Nil c       = c
> myAppend (Cons a b) c = Cons a (myAppend b c)

myReverse will recursively reverse a list using an inner function.
It uses an accumulator value to build the intermediate reversed list

> myReverse :: List a -> List a
> myReverse lst = rev lst Nil
>   where
>     rev Nil c       = c
>     rev (Cons a b) c = rev b (Cons a c) 

myFold will apply a binary operation over a list of elements by
pairing them up with an accumulator value. The first pair is
(op (head lst) acc) and keeps applying the operation with
acc as the right-hand operand.

> myFold :: (Num a) => (a -> a-> a) -> a -> List a -> a
> myFold fn acc Nil       = acc
> myFold fn acc (Cons a b) = myFold fn (fn acc a) b

myMap applies a one-arg function to a list of elements
and returns the new list with each mapped value.

> myMap :: (Num a) => (a -> a) -> List a -> List a
> myMap fn Nil       = Nil
> myMap fn (Cons a b) = Cons (fn a) (myMap fn b)

> main :: IO ()
> main = do
>   putStr "Head of '(1 2 3) is: "
>   putStrLn $ show $ myHead (Cons 1 (Cons 2 (Cons 3 Nil)))
>   putStr "Sum of nums from 1 to 10 is "
>   putStrLn $ show $ myFold (+) 0 $ myRange 10
>   putStrLn ""
>   putStr "Map of (x->x+1) from 1 to 10 is "
>   putStrLn $ show $ myMap succ $ myReverse $ myRange 10
  
-- end
