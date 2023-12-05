{- Haskell functions have three core components: 
1. type info about the func parameters and return value
2. function name and parameter names
3. an expression that defines the function's behvaior -}

insult :: String -> String -> String
insult name smell = name ++ " smells like " ++ smell ++ " and doesn't floss"

{- Things to note:
1. There's no explicit return since the right hand side of a function is what it returns
2. Spaces and indentation are used to define code blocks; no braces necessary
3. No differences between parameters and return types
4. We can only concatenate (++) on lists of the same type -}

isBigger :: Double -> Double -> Bool
isBigger a b = a > b

average :: [Double] -> Double
average list =
  (sum list) / fromIntegral (length list)

{- fromIntegral is sed to cast the value of length to be divisible, and it maps from Integral->Double -}

get_last_item :: [any_type] -> any_type
get_last_item lst =
  head (reverse lst)

-- Given the following --
f :: Int -> Int
f x = x^2

g :: Int -> Int
g x = 3 * x

-- y = f g 2  syntax error --
z = f 5 * 10

{- Haskell evaluates functions from left to right (left-associative)
Ex. y = f g 2, this would be read as ((f g) 2)
    f 5 * 10, this would be read as (f 5) * 10
-}

-- f(g(x))
compute_f_of_g x = f (g x)

-- f(x * 10)
compute_f_of_x_times_ten x = f (x * 10)

main::IO()
main = do 
    putStr "What is your name?"
    name <- getLine
    putStrLn (insult name "unwashed dog")

-- Local Bindings --

{- The let keyword -}
get_nerd_status gpa study_hrs =
  let
      gpa_part = 1 / (4.01 - gpa)
      study_part = study_hrs * 10
      nerd_score = gpa_part + study_part
  in
      if nerd_score > 100 then
           "You are super nerdy!"
      else "You're a nerd poser."

{- 
The let construct consists of two parts:
1. first part is btw the let and the in; here you define one or more "bindings"
to associate a name with an expression.
2. second part follows in; contains an expression where the bindings are used

Bindings are IMMUTABLE variables - cannot be changed!
-}

{- The where keyword -}
get_nerd_status gpa study_hrs =
  if nerd_score > 100
     then "You are super nerdy!"
     else "You're a nerd poser."
  where
      gpa_part = 1 / (4.01 - gpa)
      study_part = study_hrs * 10
      nerd_score = gpa_part + study_part

{- 
1. first write the code
2. specify the bindings adter the where keyword
-}

{- Rule of thumb
when defining bindings (variables) for a single expression (e.g. the example here), either one is fine
when defining bindings for use across multiple expressions, use where
-}

-- Nested Functions with let or where --
whats_the_behavior_of name =
 if name == "Carey"
    then behaves_like name "twelve year-old"
    else behaves_like name "grown-up"
 where
   behaves_like n what =
      n ++ " behaves like a " ++ what ++ "!"

{- Nested functions can use all of their enclosing function’s bindings. 
So, we can make our example more concise by using name:
-}
whats_the_behavior_of name =
 if name == "Carey"
    then behaves_like "twelve year-old"
    else behaves_like "grown-up"
 where
   behaves_like what =
      name ++ " behaves like a " ++ what ++ "!"


-- Lazy execution with let and where --
{- 
potentially_slow_func arg =
  let
      val1 = really_slow_function arg
      val2 = very_fast_function arg
      val3 = pretty_fast_function arg
  in
      if val3 > 100 then val1
                    else val2

laziness_example arg =
  let
      val1 = getLine
      val2 = getLine
  in
      if arg > 100 then val1 + val2
                   else val2 * val1
-}

-- Control Flow --
{-
if <expression> then <expression>
                else <expression>

*** Every if statement must have an accompanying else ***
-}

ageist_greeting age =
  if age > 30 then "Hey boomer!"
              else "Hey fam!"

-- Guards --
{- Guards are a compact version of the if-then expression
if <condition> then <expr> ==>  | <condition> = <expr>
Ex.
somefunc param1 param2
  | <if-x-is-true> = <run-this>
  | <if-y-is-true> = <run-that>
  | <if-z-is-true> = <run-the-other>
  | otherwise = <run-this-otherwise>
-}

major_guesser salary
  | salary > 150000 = "CS"
  | salary > 120000 = "EE"
  | salary < 30000 = "Any major at USC"
  | otherwise = "Probably Poli-sci"

{- 
1. no equal sign after defining function
2. each guard begins with pipe | 
3. first part is a bool expression; to see if it evaluates to true
    second part is a "payload" expression"; returns if expression is true
4. guards evaluated from top to bottom and returns payload of the first guard that's true
-}

-- Factorial using if/then
fact n =
  if n <= 0 then 1
            else n * fact (n-1)

-- Factorial using guards
fact n
 | n == 0 = 1
 | otherwise = n * fact (n-1)

-- Find sm0l-est value in a list
sm0lest lst
 | lst == [] = error "empty list"
 | length lst == 1 = first
 | otherwise = min first (sm0lest rest)
 where
   first = head lst
   rest = tail lst

qsort lst
 | lst == [] = []
 | otherwise = less_eq ++ [pivot] ++ greater
 where
    pivot = head lst
    rest_lst = tail lst
    less_eq = qsort [a | a <- rest_lst, a <= pivot]
    greater = qsort [a | a <- rest_lst, a > pivot]
