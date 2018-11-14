-- Arithmetic Tree in Haskell

An Arithmetic Tree is a kind of binary tree where each node
stores a pointer to two other nodes and an operation. The
operation can be any of the core arithmetic operations
(Addition, Subtraction, Division, Multiplication, Exponentiation)

However, a node can also have no children and only contain a
value. When traversing this tree, if you reach a leaf node,
it means it's time to stop recursing and start calculating the
operation node using those values.

In the case of Addition(1, 2) => 3

        (Add)
        /   \
       (1)  (2)

When you traverse this structure, you look to "solve"
each sub-tree before solving the node you are inspecting via
recursion. Such that

Solve(Add(Left, Right)) => Solve(Left) + Solve(Right)

So far we've only talked about Binary operations, but
there are also Unary operations like the Negate(x) operation
which negates whatever argument it is supplied.

Negate(5) => -5

This node does not have two children and only has one,
but is still solved in the same way

Solve(Negate(Child)) = (-1) * Solve(Child)

If for any reason a NaN value is produced, the equation must fail
because there is no way to complete an operation using a
NaN value. Division is the only operation that can produce a NaN
from dividing by zero, but NaN handling must be in place properly.

Solve(Add(5, NaN)) = NaN


First we start with an algebraic data type that can store all
possible operations and the "Value" type which holds a number,
as well as the "NaN" value which will cause other NaNs when
interacted with

> data Arith a = NaN
>              | Value a
>              | Neg (Arith a)
>              | Add (Arith a) (Arith a)
>              | Sub (Arith a) (Arith a)
>              | Mul (Arith a) (Arith a)
>              | Div (Arith a) (Arith a)
>              | Pow (Arith a) (Arith a)
>                deriving (Show, Eq)


Next we need a way to extract values and apply operations to two
different Arith types. If we did this with pattern matching, we would
have to add 3 rules for every operation ((X, Y), (X, NaN), (Y, NaN))

Instead we will write an Applicative rule that will "lift" the values
from the Arith type and apply a 2-arg function for us. If any of the
values are in fact a NaN, then the function will return a NaN for us,
telling us the equation failed.

> lift :: (a -> a -> a) -> Arith a -> Arith a -> Arith a
> lift f (Value a) (Value b) = Value (f a b)
> lift _ _ _                 = NaN


We need a method to determine if an Arith type is a (Value 0),
which will be used when dividing to check for a zero-division,
which in turn generates a NaN. Instead of having this functionality
inside our "lift" function, we'll just check it before lifting values
  
> isZero :: (Eq a, Num a) => Arith a -> Bool
> isZero (Value a) = a == 0


Now we can write our equation solver which will recurse through the
arithmetic tree and solve nodes for us. Each node will "lift" two values
and apply it's specific operation to the values, and will recursively
solve sub-trees until final values are reached.

> solve :: (Eq a, Num a, Fractional a, Floating a) => Arith a -> Arith a
> solve (Value a)  = Value a
> solve (Neg a)    = lift (*)  (solve a) (Value (-1.0))
> solve (Add a b)  = lift (+)  (solve a) (solve b)
> solve (Sub a b)  = lift (-)  (solve a) (solve b)
> solve (Mul a b)  = lift (*)  (solve a) (solve b)
> solve (Pow a b)  = lift (**) (solve a) (solve b)
> solve (Div a b)  = let d = (solve b) in
>                      if (isZero d) then NaN else lift (/) (solve a) d


Now to test it out

> main :: IO ()
> main = do
>   putStr "(/ 3 0) => "
>   putStrLn $ show $ solve (Div (Value 3) (Value 0))
>   putStr "(^ 3 5) => "
>   putStrLn $ show $ solve (Pow (Value 3) (Value 5))
>   putStr "(* 7 (+ (- 10 8) (/ 25 5))) => "
>   putStrLn $ show $ solve (Mul (Value 7) (Add (Sub (Value 10) (Value 8)) (Div (Value 25) (Value 5))))
>   putStr "(Negate 3) => "
>   putStrLn $ show $ solve (Neg (Value 3))
  
-- end
