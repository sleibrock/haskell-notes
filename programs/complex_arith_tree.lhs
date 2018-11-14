-- Complex Arithmetic Tree in Haskell

This will be more or less the same concept as arith_tree.lhs,
except with added support for Complex Numbers.

A Complex Number is a number that satisfies the equation:

x^2 = -1

When you square any number, it will always be positive, meaning
this equation is technically impossible. However, a number known as
an Imaginary number can satisfy this rule with the following definition:

i^1 =  i
i^2 = -1
i^3 = -i
i^4 =  1

A Complex Number can be thought of as two numbers combined:
a Real component and an Imaginary component. Complex numbers are
often represented in the format of (Real, Imag) tuples. The above
can be re-written as Complex numbers instead:

(0+1j)^1 =  (0+1j)
(0+1j)^2 = (-1+0j)
(0+1j)^3 =  (0-1j)
(0+1j)^4 =  (1+0j)

Where "j" represents the Imaginary component. The + or - symbol
between tells us whether the Imag component is positive or negative.

When you add or subtract Complex Numbers, they apply the arithmetic
on both fields. When you add/subtract/multiply/divide Complex Numbers
with Real numbers, the Real numbers will (mostly) only affect the
Real component of a Complex Number.

If we want to think about our different interactions between Real and
Complex numbers, here's a simple table:

Real + Real       => Real
Real + Complex    => Complex
Complex + Real    => Complex
Complex + Complex => Complex

The + can be any common binary arithmetic operation (+-*/^) but
the interactions stay the same.


Extending the Arithmetic tree to support Complex numerals is
going to be a lot trickier. Much like how we had all the operations
and core 'Value' types, we're going to extend it to both Real and Complex
values.

> data Arith a = NaN
>              | Real a
>              | Complex a a
>              | Neg  (Arith a)
>              | Conj (Arith a)  -- specific to complex nums
>              | Add  (Arith a) (Arith a)
>              | Sub  (Arith a) (Arith a)
>              | Mul  (Arith a) (Arith a)
>              | Div  (Arith a) (Arith a)
>                deriving (Show, Eq)

-->               | Pow (Arith a) (Arith a) -- exponentiation is hard


Because the arithmetic operations we previously used in arith_tree.lhs
don't support our new Complex type, we will have to define some new
operations ourselves! All newly defined arithmetic functions
will have a pattern to deal with when NaN was found by returning NaN

(Note: there will be a lot of Complex math here, so feel free
to skip down quite a bit to avoid reading lots of this)

Arithmetic is easy because it is simply adding both
components of numbers. Such that (a+bj)+(c+dj) = (a+c)+(b+d)j

> add :: (Num a) => Arith a -> Arith a -> Arith a
> add (Real x) (Real y)               = Real (x + y)
> add (Real x) (Complex r i)          = Complex (r + x) i
> add (Complex r i) (Real y)          = Complex (r + y) i
> add (Complex r0 i0) (Complex r1 i1) = Complex (r0 + r1) (i0 + i1)
> add _ _                             = NaN


Subtraction is more or less simliar to addition. It's tricky with
left-hand Reals because you have to *subtract* the imag component from
nothing, meaning we just negate the given Imag component.
The subtraction of two complex nums is (a+bj)-(c+dj) = (a-c)+(b-d)j

> sub :: (Num a, Fractional a) => Arith a -> Arith a -> Arith a
> sub (Real x) (Real y) = Real (x - y)
> sub (Real x) (Complex r i) = Complex (x - r) (i * (-1.0))
> sub (Complex r i) (Real y) = Complex (r - y) i
> sub (Complex r0 i0) (Complex r1 i1) = Complex (r0 - r1) (r1 - i1)
> sub _ _ = NaN



> mul :: (Num a) => Arith a -> Arith a -> Arith a
> mul (Real x) (Real y) = Real (x * y)
> mul (Real x) (Complex r i) = NaN
> mul (Complex r i ) (Real y) = NaN
> mul (Complex r0 i0) (Complex r1 i1) = NaN
> mul _ _ = NaN


The next two functions are devoted purely for division. Since division
has to check for whether the denominator is a Zero value (in both
Complex and Real terms), we need a function that can handle both cases
and assert a Boolean.

> isZero :: (Eq a, Num a) => Arith a -> Bool
> isZero (Real a)      = a == 0
> isZero (Complex r i) = and [r==0, i==0]
> isZero _             = False


Division is the most complicated Complex arithmetic case of all.
When doing Real/Complex division it is fairly simple, but Complex/Complex
division is very tricky with lots of multiplication and division.

> mdiv :: (Eq a, Num a, Fractional a) => Arith a -> Arith a -> Arith a
> mdiv (Real x) (Real y) = if (isZero (Real y)) then NaN else Real (x / y) 
> mdiv (Real x) (Complex r i) = if (isZero (Complex r i))
>                                then NaN
>                                else (Complex r i)
> mdiv (Complex r i) (Real y) = if (isZero (Real y)) then NaN
>                                else Complex (r / y) (i / y)
> mdiv (Complex r0 i0) (Complex r1 i1) = if (isZero (Complex r1 i1))
>                                         then NaN
>                                         else NaN 
> 
> mdiv _ _ = NaN


Negation is simply inverting a number over it's X (or real/imag) axis.
Negate(5) => -5
Negate(5+2j) => -5-2j

> neg :: (Num a, Fractional a) => Arith a -> Arith a
> neg (Real x)      = Real (x * (-1.0))
> neg (Complex r i) = Complex (r * (-1.0)) (i * (-1.0))
> neg _             = NaN


A Conjugate negates the Imag component of a Complex number
If used on anything else, should produce a NaN (can't conjugate a Real)

> conj :: (Num a, Fractional a) => Arith a -> Arith a
> conj (Complex r i) = Complex r (i * (-1.0))
> conj _ = NaN


Lifting is now a lot easier because we don't have to write
specific patterns dealing with NaN. Because we wrote all the rules
in our arithmetic functions, we simply just delegate a function to
both values

Now for the final part where we solve our arithmetic tree, matching
across all patterns and solving for the final value

> solve :: (Eq a, Num a, Fractional a, Floating a) => Arith a -> Arith a
> solve (Real x)      = Real x
> solve (Complex r i) = Complex r i
> solve (Neg a)       = neg  (solve a)
> solve (Conj a)      = conj (solve a)
> solve (Add a b)     = add  (solve a) (solve b)
> solve (Sub a b)     = sub  (solve a) (solve b)
> solve (Mul a b)     = mul  (solve a) (solve b)
> solve (Div a b)     = mdiv (solve a) (solve b)


> main :: IO ()
> main = do
>   putStr "(+ 5 7) => "
>   putStrLn $ show $ solve (Add (Real 5) (Real 7))
>   putStr "(+ (3+4j) (2+3j)) => "
>   putStrLn $ show $ solve (Add (Complex 3 4) (Complex 2 3))
>   putStr "(+ 5 (3+5j)) => "
>   putStrLn $ show $ solve (Add (Real 5) (Complex 3 5))
>   putStr "(/ 5 (0+0j)) => "
>   putStrLn $ show $ solve (Div (Real 5) (Complex 0 0))

-- end
