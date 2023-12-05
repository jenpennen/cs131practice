-- Find the last element of a list --
{-
λ> myLast [1,2,3,4]
4
λ> myLast ['x','y','z']
'z'
-}

myLast :: [a] -> a
myLast [] = error "No end for empty lists"
myLast [x] = x
myLast (_:xs) = myLast xs

myLast':: [a] -> a
myLast' [] = error "No end for empty lists"
myLast' xs = last xs

-- Find the last-but-one (or second last) element of a list
{-
λ> secLast [1,2,3,4]
3
λ> secLast ['a'..'z']
'y'
-}
secLast :: [a] -> a
secLast [] = error "empty list"
secLast xs = reverse xs !! 1

-- Find the K'th element of a list. --
{-
λ> elementAt [1,2,3] 2
2
λ> elementAt "haskell" 5
'e'
-}

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) i = elementAt xs (i - 1)
elementAt _ _ = error "Index out of bounds"

elementAt' :: [a] -> Int -> a
elementAt' xs n | length xs < n = error "Index out of bounds"
                | otherwise = fst . last $ zip xs [1..n]


--  Find the number of elements in a list. --
{- 
λ> myLength [123, 456, 789]
3
λ> myLength "Hello, world!"
13
-}

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

--Using an accumulator--
myLength' :: [a] -> Int
myLength' list = acc list 0
    where
        acc [] n = n
        acc (_:xs) n = acc xs (n + 1)
