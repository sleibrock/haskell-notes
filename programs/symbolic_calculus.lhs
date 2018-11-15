-- Symbolic Calculus in Haskell


Calculus is the study of continuous functions that change.

In order to calculate a derivative of a certain function, one must
normally calculate the difference of two points on the graph of the
function, and take a limit as the difference approaches zero to calculate
what is known as the "derivative", or how rapid a function is changing over
the a certain frame of it's domain.

Normally the method for obtaining a derivative is:

lim h->0 = f(x+h) - f(x)  / h  

However, there are a number of recognizable patterns that allow
us to quickly "derive" functions, such as when given a function
being raised to a certain power, such as:

f(x) = 7x^3

What you can instead do is the following:

dy/dx f(x) = 3*7x^(3-1) = 21x^2

There are a large number of rules for quickly deriving functions,
and calculating many of these at once proves to be quite tricky.


Taking inspiration from my previous programs like complex_arith_tree.lhs,
let's define a data structure that can represent our functions, and
pattern match against it to see if we can make a program that can derive
any given function.


This only works for functions of single variables currently

(All variables are assumed to be of the letter "x")

Rules to implement checklist:

* Distributive rule   dy/dx f(x) + g(x) = f'(x) + g'(x)
* Power rule (ax^n = nax^n-1)
* Sine / Cosine rules (cos = sin = -cos = -sin)
* Product Rule     (f(x) * g(x) = f'(x)g(x) + f(x)g'(x))
* Quotient Rule    (f(x) / g(x) = (f'(x)g(x) + f(x)g'(x)) / g(x)^2 )
* Chain rule       (f(g(x)) = (f'(g(x))g'(x)
* Logarithm Rules  (Ln(x) = 1/x)


Our "Calc" data type will store (almost) all possible functions
that we can use to create functions. Anything that accepts arbitrary
arguments like addition or subtraction are placed into binary arguments
instead to make it easier. We can match against each possible type
to create derivative formulas. Instead of traversing down the entire
expression tree like we would with the arith_tree programs, we only
need to recurse when we need to derive inner expressions (like when
functions are composed of one another, or of multiplication/division rules).



> data Calc a = NaN
>             | Const a         -- a constant value ie 5
>             | Var a           -- a variable with a 
>             | Add (Calc a) (Calc a)  -- distributive laws
>             | Sub (Calc a) (Calc a)
>             | Mul (Calc a) (Calc a)  -- product rule
>             | Div (Calc a) (Calc a)  -- quotient rule
>             | Pow (Calc a) (Calc a)  -- ax^(f(x))
>             | Exp (Calc a) (Calc a)  -- ne^f(x)
>             | Ln  (Calc a)           -- logarithm rule #1
>             | Log (Calc a)           -- logarithm rule #2
>             | Sin (Calc a)           -- trig laws, sin(x) = -cos(x)
>             | Cos (Calc a)           -- cos(x) => sin(x)
>             | Tan (Calc a)           -- tan(x) = sin(x)/cos(x)
>               deriving (Show, Eq)


Still incomplete, but a work in progress.
TODO:
- powers of a^x
- e^x
- trig identities / reciprocals?

> derive :: (Num a, Eq a) => Calc a -> Calc a
> derive (Const a)               = Const 0
> derive (Var a)                 = Const a
> derive (Add f g)               = Add (derive f) (derive g)
> derive (Pow (Var a) (Const 1)) = Const a
> derive (Pow (Var a) (Const 2)) = Var (a*2)
> derive (Pow (Var a) (Const n)) = Pow (Var (a*n)) (Const (n-1))
> derive (Pow f (Const n))       = Mul (Pow (Mul (Const n) f) (Const (n-1))) (derive f)
> derive (Sin (Var a))           = Cos (Var a)
> derive (Cos (Var a))           = Mul (Const (-1)) (Sin (Var a))
> derive (Tan f)                 = derive (Div (Sin f) (Cos f))
> derive (Ln  (Var a))           = Div (Const 1) (Var a)
> derive (Ln f)                  = Mul (derive f) (derive f) 
> derive (Mul f g)               = Add (Mul f (derive g)) (Mul (derive f) g)
> derive (Div f g)               = Div (Sub (Mul f (derive g)) (Mul (derive f) g)) (Pow g (Const 2))
> derive _ = NaN


The pretty printing section where we map all possible calculation
types into pretty strings for printing.
By extension, this could also be extended to LaTex later down the line.

> join :: (Foldable t) => t String -> String
> join = foldl (++) ""

> pretty :: (Eq a, Num a, Show a) => Calc a -> String
> pretty (Const a)             = show a
> pretty (Var 1)               = "x"
> pretty (Var a)               = join [(show a), "x"]
> pretty (Add f g)             = join ["(", (pretty f), "+", (pretty g), ")"]
> pretty (Sub f g)             = join ["(", (pretty f), "-", (pretty g), ")"]
> pretty (Mul f (Const 1))     = pretty f
> pretty (Mul (Const 1) f)     = pretty f
> pretty (Mul f (Const (-1)))  = join ["-", (pretty f)]
> pretty (Mul (Const (-1)) f)  = join ["-", (pretty f)] 
> pretty (Mul f (Const x))     = join [(show x), (pretty f)]
> pretty (Mul (Const x) f)     = join [(show x), (pretty f)]
> pretty (Mul f g)             = join ["(", (pretty f), "*", (pretty g), ")"]
> pretty (Mul (Sin f) (Sin f)) = join []
> pretty (Div f (Const 1))     = pretty f
> pretty (Div f g)             = join ["(", (pretty f), "/", (pretty g), ")"]
> pretty (Pow a n)             = join ["[", (pretty a), "^(", (pretty n), ")]"]
> pretty (Sin f)               = join ["sin(", (pretty f), ")"]
> pretty (Cos f)               = join ["cos(", (pretty f), ")"]
> pretty (Tan f)               = join ["tan(", (pretty f), ")"]
> pretty (Ln f)                = join ["log|", (pretty f), "|"]
> pretty f                     = show f  -- <- last resort formatter


Now to test as many functions out as we can.

> solve :: (Num a, Eq a, Show a) => String -> Calc a -> IO ()
> solve s f = putStrLn $ (++) s $ pretty $ derive f

> main :: IO ()
> main = do
>   solve "dy/dx 5 => " (Const 5)
>   solve "dy/dx 2x => " (Var 2)
>   solve "dy/dx x^2 => " (Pow (Var 1) (Const 2))
>   solve "dy/dx 7x^5 => " (Pow (Var 7) (Const 5))
>   solve "dy/dx 7x^5 + 8x^3 => " (Add (Pow (Var 7) (Const 5)) (Pow (Var 8) (Const 3)))
>   solve "dy/dx ln|x| => " (Ln (Var 1))
>   solve "dy/dx sin(x)^7 => " (Pow (Sin (Var 1)) (Const 7))
>   solve "dy/dx sin(x)*cos(2x) => " (Mul (Sin (Var 1)) (Cos (Var 2)))
>   solve "dy/dx tan(x) => " (Tan (Var 1))



Sample output for readers:

dy/dx 5 => 0
dy/dx 2x => 2
dy/dx x^2 => 2x
dy/dx 7x^5 => 35x^(4)
dy/dx 7x^5 + 8x^3 => (35x^(4)+24x^(2))
dy/dx ln|x| => (1/1x)
dy/dx sin(x)^7 => ([(7*sin(x))^(6)]*cos(x))

-- end
              
