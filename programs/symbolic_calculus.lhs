-- Symbolic Calculus in Haskell

(Inspiration and reference from: http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html)


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



> data Expr a = NaN                    -- when something is div by zero
>             | Var Char               -- a variable with a Char
>             | Const a                -- a constant value ie 5
>             | Add (Expr a) (Expr a)  -- distributive laws
>             | Sub (Expr a) (Expr a)
>             | Mul (Expr a) (Expr a)  -- product rule
>             | Div (Expr a) (Expr a)  -- quotient rule
>             | Pow (Expr a) (Expr a)  -- ax^(f(x))
>             | Exp (Expr a) (Expr a)  -- ne^f(x)
>             | Ln  (Expr a)           -- logarithm rule #1
>             | Log (Expr a)           -- logarithm rule #2
>             | Sin (Expr a)           -- trig laws, sin(x) = -cos(x)
>             | Cos (Expr a)           -- cos(x) => sin(x)
>             | Tan (Expr a)           -- tan(x) = sin(x)/cos(x)
>               deriving (Show, Eq)


The simplify function applies a bunch of mathematical rules to make
expressions a lot simpler. Things like operations with zeroes, merging
operations between two constants, combining like-term powers, handling
negative number multiplication, and other types of common math identities
to reduce expressions to simple form.

The fullSimplifiy applies a recursive traversal of the expression tree
until it is fully "simplified".

> simplify :: (RealFloat a) => Expr a -> Expr a
> simplify (Add (Const a) (Const b))    = Const (a + b)
> simplify (Add a         (Const 0))    = simplify a
> simplify (Add (Const 0)         b)    = simplify b
>
> simplify (Sub (Const a) (Const b))    = Const (a - b)
> simplify (Sub a         (Const 0))    = simplify a
> simplify (Sub (Const 0)         b)    = simplify (neg b) -- negate this?
> simplify (Sub a (Mul (Const (-1)) b)) = Add a b
>
> simplify (Mul (Const a) (Const b)) = Const (a * b)
> simplify (Mul a         (Const 1)) = simplify a
> simplify (Mul (Const 1)         b) = simplify b
> simplify (Mul a         (Const 0)) = Const 0
> simplify (Mul (Const 0)         b) = Const 0
> simplify (Mul (Var l)   (Const c)) = Mul (Const c) (Var l)
> simplify (Mul (Mul (Const (-1)) f) (Mul (Const (-1)) g)) = Mul f g
>
> -- combine equal terms (and when there's neg numbers involved)
> simplify (Mul a                    b) | a==b = Pow a (Const 2)
> simplify (Mul a (Mul (Const (-1)) b)) | a==b = Mul (Const (-1)) (Pow a (Const 2))
> simplify (Mul (Mul (Const (-1)) a) b) | a==b = Mul (Const (-1)) (Pow a (Const 2))
>
> -- division rules (divide by zero results in a NaN)
> simplify (Div (Const a) (Const b)) = Const (a / b)
> simplify (Div (Const 0)         _) = Const 0
> simplify (Div _         (Const 0)) = NaN
> simplify (Div a                 b) | a == b = Const 1   -- (x/x) = 1
> simplify (Div a         (Const 1)) = simplify a
> 
> simplify (Pow (Const a) (Const b)) = Const (a ** b)
> simplify (Pow a         (Const 1)) = simplify a
> simplify (Pow a         (Const 0)) = Const 1
> simplify (Pow (Pow c (Const b)) (Const a)) = Pow c (Const (a*b))
>
>
> -- instead of having (-f + g), lets turn it into (g - f)
> simplify (Add (Mul (Const (-1)) a) b) = Sub b a
>
> -- euler identities of sin(x)^2 + cos(x)^2 = 1
> simplify (Add (Pow (Sin f) (Const 2)) (Pow (Cos g) (Const 2))) | f == g = Const 1
> simplify (Add (Pow (Cos f) (Const 2)) (Pow (Sin g) (Const 2))) | f == g = Const 1
> simplify (Sub (Mul (Const (-1)) (Pow (Sin f) (Const 2))) (Pow (Cos g) (Const 2))) | f == g = Const (-1)
> simplify (Sub (Mul (Const (-1)) (Pow (Cos f) (Const 2))) (Pow (Sin g) (Const 2))) | f == g = Const (-1)
>
>
> simplify (Add a b) = Add (simplify a) (simplify b)
> simplify (Sub a b) = Sub (simplify a) (simplify b)
> simplify (Mul a b) = Mul (simplify a) (simplify b)
> simplify (Div a b) = Div (simplify a) (simplify b)
>
> simplify (Tan a)   = Div (Sin a) (Cos a)
>
> simplify f = f
>
>
> fullSimplify expr = fullSimplify' expr (Const 0)
>   where fullSimplify' cur last | cur == last = cur
>                                | otherwise = let cur' = simplify cur
>                                  in fullSimplify' cur' cur
>


dy/dx tan(x) => Div (Sub (Mul (Const (-1.0)) (Pow (Sin (Var 'x')) (Const 2.0))) (Pow (Cos (Var 'x')) (Const 2.0))) (Pow (Cos (Var 'x')) (Const 2.0))

dy/dx -tan(x) => Div (Add (Pow (Mul (Const (-1.0)) (Sin (Var 'x'))) (Const 2.0)) (Pow (Cos (Var 'x')) (Const 2.0))) (Pow (Cos (Var 'x')) (Const 2.0))


> neg :: (RealFloat a) => Expr a -> Expr a
> neg NaN       = NaN
> neg (Var x)   = (Mul (Const (-1)) (Var x))
> neg (Const c) = Const (-c)
> neg (Add a b) = Add (neg a) (neg b)
> neg (Sub a b) = Sub (neg a) (neg b)
> neg (Mul a b) = Mul (neg a) b
> neg (Div a b) = Div (neg a) b
> neg (Sin f)   = Mul (Const (-1)) (Sin f)
> neg (Cos f)   = Mul (Const (-1)) (Cos f)
> neg (Tan f)   = Div (Mul (Const (-1)) (Sin f)) (Cos f)
> neg (Ln f)    = Mul (Const (-1)) (Ln f)



> derive :: (RealFloat a) => Expr a -> Expr a
> derive NaN                     = NaN
> derive (Const c)               = Const 0
> derive (Var x)                 = Const 1
> derive (Add f g)               = Add (derive f) (derive g)
> derive (Sub f g)               = Sub (derive f) (derive g)
> derive (Mul (Const x) (Var l)) = Const x
> derive (Mul (Var l) (Const x)) = Const x
> derive (Pow (Var l) (Const n)) = Pow (Mul (Const n) (Var l)) (Const (n-1))
> derive (Pow (Mul (Const c) (Var l)) (Const n)) = Pow (Mul (Const (n*c)) (Var l)) (Const (n-1))
> derive (Pow f (Const n)) = Mul (derive f) (Pow (Mul (Const n) f) (Const (n-1)))
>
> derive (Ln (Var c)) = (Div (Const 1) (Var c))
>
> derive (Sin (Var c)) = Cos (Var c)
> derive (Cos (Var c)) = neg (Sin (Var c))
> derive (Sin f)       = Mul (derive f) (Cos f)
> derive (Cos f)       = Mul (neg (derive f)) (Sin f)
>
> derive (Mul f g) = Add (Mul f (derive g)) (Mul (derive f) g)
>
> derive (Div f g) = Div (Sub (Mul g (derive f)) (Mul f (derive g))) (Pow g (Const 2)) 

This function will join a list of strings, to be mostly used with
pretty-printing all equations

> join :: (Foldable t) => t String -> String
> join = foldl (++) ""


> pretty :: (RealFloat a, Show a) => Expr a -> String
> pretty (Const a)              = show a
> pretty (Var a)                = join [(a:"")]
> pretty (Add f g)              = join ["(", (pretty f), " + ", (pretty g), ")"]
> pretty (Sub f g)              = join ["(", (pretty f), " - ", (pretty g), ")"]
> pretty (Mul f (Const 1))      = pretty f
> pretty (Mul (Const 1) f)      = pretty f
> pretty (Mul (Const (-1)) f)   = join ["-", (pretty f)]
> pretty (Mul f (Const (-1)))   = join ["-", (pretty f)]
> pretty (Mul f (Const x))      = join [(show x), (pretty f)]
> pretty (Mul (Const x) f)      = join [(show x), (pretty f)]
> pretty (Mul f g)              = join ["(", (pretty f), " * ", (pretty g), ")"]
> pretty (Div f g)              = join ["(", (pretty f), " / ", (pretty g), ")"]
> pretty (Pow a (Const x))      = join ["[", (pretty a), "^", (show x), "]"]
> pretty (Pow a n)              = join ["[", (pretty a), "^(", (pretty n), ")]"]
> pretty (Sin f)                = join ["sin(", (pretty f), ")"]
> pretty (Cos f)                = join ["cos(", (pretty f), ")"]
> pretty (Tan f)                = join ["tan(", (pretty f), ")"]
> pretty (Ln f)                 = join ["log|", (pretty f), "|"]
> pretty f                      = show f -- <- last resort formatter




> toLatex :: (RealFloat a, Show a) => Expr a -> String
> toLatex (Const a)                         = show a
> toLatex (Var a)                           = join [(a:"")]
> toLatex (Add a b)                         = join ["(", (toLatex a), " + ", (toLatex b), ")"]
> toLatex (Sub a b)                         = join ["(", (toLatex a), " - ", (toLatex b), ")"]
> toLatex (Mul a (Const (-1)))              = join ["-", (toLatex a)]
> toLatex (Mul (Const (-1)) b)              = join ["-", (toLatex b)]
> toLatex (Mul a b)                         = join [(toLatex a), (toLatex b)]
> toLatex (Div a b)                         = join ["\frac{", (toLatex a), "}{", (toLatex b), "}"]
> toLatex (Pow a (Div (Const 1) (Const 2))) = join ["\\sqrt{", (toLatex a), "}{2}"]
> toLatex (Pow a (Div (Const 1) (Const n))) = join ["\\sqrt{", (toLatex a), "}{", (show n), "}"]
> toLatex (Pow a n)                         = join ["[", (toLatex a), "^{", (toLatex n), "}]"]
> toLatex (Sin f)                           = join ["sin(", (toLatex f), ")"]
> toLatex (Cos f)                           = join ["cos(", (toLatex f), ")"]
> toLatex (Tan f)                           = join ["tan(", (toLatex f), ")"]
> toLatex (Ln f)                            = join ["ln(", (toLatex f), ")"]
> toLatex f                                 = show f



We can also turn expressions into LaTeX using similar pattern matching
as the above `pretty` function.


Now to test as many functions out as we can.

> solve :: (RealFloat a, Show a) => String -> Expr a -> IO ()
> solve s f = putStrLn $ (++) s $ pretty $ fullSimplify $ derive $ fullSimplify f


> fullLatex :: (RealFloat a, Show a) => String -> Expr a -> String 
> fullLatex var f = join ["\frac{dy}{d", var, "}", (toLatex f), " = ", (toLatex (fullSimplify (derive (fullSimplify f))))]

> main :: IO ()
> main = do
>   putStrLn "Differentiating Equations program"
>   solve "dy/dx 5 => " $ Const 5
>   solve "dy/dx x => " $ Var 'x'
>   solve "dy/dx 5x => " $ Mul (Const 5) (Var 'x')
>   solve "dy/dx x^2 => " $ Pow (Var 'x') (Const 2)
>   solve "dy/dx 7x^5 => " $ Pow (Mul (Const 7) (Var 'x')) (Const 5)
>   solve "dy/dx 7x^5 + 8x^3 => " (Add (Pow (Mul (Const 7) (Var 'x')) (Const 5)) (Pow (Mul (Const 8) (Var 'x')) (Const 3)))
>   solve "dy/dx ln|x| => " $ Ln (Var 'x')
>   solve "dy/dx cos(x) => " $ Cos (Var 'x')
>   solve "dy/dx -cos(x) => " $ neg (Cos (Var 'x'))
>   solve "dy/dx sin(x)^7 => " $ Pow (Sin (Var 'x')) (Const 7)
>   solve "dy/dx sin(x)*cos(x) => " $ Mul (Sin (Var 'x')) (Cos (Var 'x'))
>   solve "dy/dx sin(x)*cos(2x) => " $ Mul (Sin (Var 'x')) (Cos (Mul (Const 2) (Var 'x')))
>   solve "dy/dx tan(x) => " $ Tan (Var 'x')
>   solve "dy/dx -tan(x) => " $ neg (Tan (Var 'x'))
>   putStrLn "\n"
>   putStrLn $ fullLatex "x" (Pow (Var 'x') (Const 2))

Sample output for readers:

dy/dx 5 => 0
dy/dx 2x => 2
dy/dx x^2 => 2x
dy/dx 7x^5 => [35x^4]
dy/dx 7x^5 + 8x^3 => ([35x^4]+[24x^2])
dy/dx ln|x| => (1/x)
dy/dx sin(x)^7 => ([7sin(x)^6]*cos(x))
dy/dx sin(x)*cos(2x) => ((sin(x)*-sin(2x))+(cos(
x)*cos(2x)))                                   
dy/dx tan(x) => (-1/[cos(x)^2])
dy/dx -tan(x) => -(-1/[cos(x)^2])    <-- wrong still


-- end
              
