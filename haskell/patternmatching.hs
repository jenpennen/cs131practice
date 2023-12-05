{- 
1. we define multiple versions of the same function
2. each version of the function must have the same number and types of arguments
3. when the function is called, Haskell checks each definition in-order from top to 
bottom, and calls the first function which has matching parameters
-}

-- Pattern matching with constants --
genz_critic :: String -> String -> String
genz_critic "Carey" word =
  "Oh Carey... OK boomer!"
genz_critic name "lit" =
  name ++ ", you're a lit genz-er"
genz_critic name "wrekt" =
  name ++ ", git wrekt cheugy"
genz_critic name word =
  name ++ ", " ++ word ++ " is not gucci!"

{- Pattern matching with constants work with other types, like numbers of lists. 
There;s no limit on how many constant patterns you can have. -}

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

list_len :: [a] -> Int
list_len [] = 0
list_len lst = 1 + list_len (tail lst)

has_this_many_eyes :: Int -> String
has_this_many_eyes 1 = "Cyclops"
has_this_many_eyes 2 = "Pupper"
has_this_many_eyes 3 = "Sphenodon Punctatus"
has_this_many_eyes 4 = "A nerd with glasses"
has_this_many_eyes n = "A freak!"

-- Pattern Matching with Tuples --
{- Since we know how items are in the tuple, we can match with them -}
-- Original way
exp :: (Int,Int) -> Int
exp t = (fst t) ^ (snd t)

-- Version #2: With pattern matching
expv2 :: (Int,Int) -> Int
expv2 (a,b) = a ^ b

-- Version #3: Adding a constant parameter
expv3 :: (Int,Int) -> Int
expv3 (a,0) = 1
expv3 (a,b) = a ^ b

extract_1st :: (type1,type2,type3) -> type1
extract_1st (a,_,_) = a

-- Extract the second value of a tuple
extract_2nd :: (type1,type2,type3) -> type2
extract_2nd (_,b,_) = b

-- Extract the third value of a tuple
extract_3rd :: (type1,type2,type3) -> type3
extract_3rd (_,_,c) = c

{- The underscore _ is a wildcard; anything can match with this item 
so we don't need to care about its type -}

{- To compare two values obtained from pattern matching, they have to be different names
and compared afterwards -}

-- using guards
tuple_eq (a, b)
  | a == b = True
  | otherwise = False

-- or, use (==) directly
tuple_eq (a, b) = a == b

-- Pattern matching with lists --
-- Extract the first item from a list
get_first :: [a] -> a
get_first (x:xs) = x

-- Extract all but the first item of a list
get_rest :: [a] -> [a]
get_rest (x:xs) = xs

-- Extract the second item from a list
get_second :: [a] -> a
get_second (x:y:xs) = y

