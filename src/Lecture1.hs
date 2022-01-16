{- |
Module                  : Lecture1
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 1 of the Haskell Beginners course.

To complete exercises, you need to complete implementation and add
missing top-level type signatures. You can implement any additional
helper functions. But you can't change the names of the given
functions.

Comments before each function contain explanations and example of
arguments and expected returned values.

It's absolutely okay if you feel that your implementations are not
perfect. You can return to these exercises after future lectures and
improve your solutions if you see any possible improvements.
-}

module Lecture1
    ( makeSnippet
    , sumOfSquares
    , lastDigit
    , minmax
    , subString
    , strSum
    , lowerAndGreater
    ) where

{- | Specify the type signature of the following function. Think about
its behaviour, possible types for the function arguments and write the
-}
makeSnippet :: Int -> [Char] -> [Char]
makeSnippet limit text = take limit ("Description: " ++ text) ++ "..."

{- | Implement a function that takes two numbers and finds sum of
their squares.

>>> sumOfSquares 3 4
25

>>> sumOfSquares (-2) 7
53

Explanation: @sumOfSquares 3 4@ should be equal to @9 + 16@ and this
is 25.
-}
-- DON'T FORGET TO SPECIFY THE TYPE IN HERE
sumOfSquares :: Num a => a -> a -> a
sumOfSquares x y = x*x + y*y -- x^2 + y^2 wouldn't compile?

{- | Implement a function that returns the last digit of a given number.

>>> lastDigit 42
2
>>> lastDigit (-17)
7


-}
-- DON'T FORGET TO SPECIFY THE TYPE IN HERE
lastDigit :: Integral a => a -> a
lastDigit n = mod (abs n) 10

{- | Write a function that takes three numbers and returns the
difference between the biggest number and the smallest one.

>>> minmax 7 1 4
6

Explanation: @minmax 7 1 4@ returns 6 because 7 is the biggest number
and 1 is the smallest, and 7 - 1 = 6.

Try to use local variables (either let-in or where) to implement this
function.
-}
{- todo why are Nums not Ords?
   todo should it be Num b, Num c, Num d instead? too many ords and nums then
-}
minmax :: (Num a, Ord a) => a -> a -> a -> a
minmax x y z =
  let
    minNum = minimum [x, y, z]
    maxNum = maximum [x, y, z]
  in
    maxNum - minNum

{- | Implement a function that takes a string, start and end positions
and returns a substring of a given string from the start position to
the end (including).

>>> subString 3 7 "Hello, world!"
"lo, w"

>>> subString 10 5 "Some very long String"
""

This function can accept negative start and end position. Negative
start position can be considered as zero (e.g. substring from the
first character) and negative end position should result in an empty
string.
-}
subString :: Int -> Int -> String -> String
subString start end str
  | start > end = ""
  | end < 0     = ""
  | otherwise   =
    let
      adjStart = max 0 start {- per spec -}
      -- adjEnd = if adjStart == end then end+1 else end  -- compensating +1 TODO
    in
      take (end - adjStart + 1) (drop adjStart str) {- +1 for inclusive end -}

{- | Write a function that takes a String — space separated numbers,
and finds a sum of the numbers inside this string.

>>> strSum "100    -42  15"
73

The string contains only spaces and/or numbers.
-}
strSum :: String -> Integer
strSum str = 
  {- go one by one through string, accumulating a buffer of elements.
     upon space, append buffer to result list. should handle floating points that way. -}
  let nums = map read (words str) :: [Integer] {- or just use words I guess -}
  in foldr (+) 0 nums
  {- or use list comprehension instead of let/in  -}

{- | Write a function that takes a number and a list of numbers and
returns a string, saying how many elements of the list are strictly
greated than the given number and strictly lower.

>>> lowerAndGreater 3 [1 .. 9]
"3 is greater than 2 elements and lower than 6 elements"

Explanation: the list [1 .. 9] contains 9 elements: [1, 2, 3, 4, 5, 6, 7, 8, 9]
The given number 3 is greater than 2 elements (1 and 2)
and lower than 6 elements (4, 5, 6, 7, 8 and 9).

🕯 HINT: Use recursion to implement this function.
-}
lowerAndGreater :: (Num a, Ord a, Show a) => a -> [a] -> String
lowerAndGreater n list =
  (show n) ++ " is greater than " ++ (show greater) ++
  " elements and lower than " ++ (show lower) ++ " elements"
  where
    -- this recurses through the list, incrementing # nums greater + lower
    accumulator :: (Num a, Ord a) => a -> a -> a -> [a] -> (a, a)
    accumulator numGr numLw _ [] = (numGr, numLw)
    accumulator numGr numLw targNum theList
      | targNum > head theList = accumulator (numGr+1) numLw targNum (tail theList)
      | targNum < head theList = accumulator numGr (numLw+1) targNum (tail theList)
      | otherwise = accumulator (numGr) numLw targNum (tail theList)
    (greater, lower) = accumulator 0 0 n list
  
                         {-
  where
    greater = ?
    lower = ?
-}
  
{- use recursion to impl the two accumulators? hmm...eg acc numLower numGreater list -}
  

{-
  | null list = ""
  | otherwise =
    
     let lowerThan = map (< n) list 
        greaterThan = lowerAndGreater ? ?
    in "3 is greater than 2 elements and lower than 6 elements"
-}
  
                
    
{- why recursion? -}

