{-# LANGUAGE InstanceSigs, ScopedTypeVariables #-}

{- |
Module                  : Lecture3
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 3 of the Haskell Beginners course.

In this module you're going to practice standard Haskell typeclasses:

  * Deriving instances
  * Using typeclasses methods
  * Implementing instances manually
  * Becoming friends with Semigroup, Monoid, Foldable and Functor typeclasses!

-}

module Lecture3
    ( Weekday (..)
    , toShortString
    , next
    , daysTo

    , Gold (..)
    , Reward (..)
    , List1 (..)
    , Treasure (..)

    , appendDiff3
    , apply
    ) where

-- VVV If you need to import libraries, do it after this line ... VVV

-- ^^^ and before this line. Otherwise the test suite might fail  ^^^

-- $setup
-- >>> import Data.Semigroup

{- | Let's define a simple enumeration data type for representing days
of the week.
-}
data Weekday
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Show, Eq, Enum, Bounded)

{- | Write a function that will display only the first three letters
of a weekday.

>>> toShortString Monday
"Mon"
-}

toShortString :: Weekday -> String
toShortString e = take 3 $ show e
-- toShortString Monday = "Mon"
-- toShortString Tuesday = "Tue"
-- toShortString Wednesday = "Wed"
-- toShortString Thursday = "Thu"
-- toShortString Friday = "Fri"
-- toShortString Saturday = "Sat"
-- toShortString Sunday = "Sun"
-- not sure if this was the intent of this exercise?

{- | Write a function that returns next day of the week, following the
given day.

>>> next Monday
Tuesday

♫ NOTE: Implement this function without pattern matching on every
  constructor! Use standard typeclasses instead (you may need to derive
  them first).

🕯 HINT: Check 'Enum' and 'Bounded' typeclasses.

🆙 Bonus challenge 1: Could you implement this function in a such way
  that it'll still work even if you change constructor names and their
  order in the 'Weekday' type?

🆙 Bonus challenge 2: Now, could you improve the implementation so it
  would work for **any** enumeration type in Haskell (e.g. 'Bool',
  'Ordering') and not just 'Weekday'?
-}

-- generalized
next :: forall a. (Enum a, Bounded a, Eq a) => a -> a
next x = if x == maxBound then minBound else succ x

-- suffices
--next :: Weekday -> Weekday
--next Sunday = Monday
--next e = succ e

-- overcomplicated
-- next = toEnum . (`mod` size) . (+ 1) . fromEnum
--   where size = 1 + fromEnum (maxBound :: a)
-- next :: Weekday -> Weekday
-- next = toEnum . (`mod` max) . (+ 1) . fromEnum
--   where max = 1 + fromEnum (maxBound :: Weekday)

{- | Implement a function that calculates number of days from the first
weekday to the second.

>>> daysTo Monday Tuesday
1
>>> daysTo Friday Wednesday
5
-}
daysTo :: Weekday -> Weekday -> Int
daysTo firstDay secondDay =
  let
    size = (fromEnum (maxBound :: Weekday)) + 1
    ringDistance = (fromEnum secondDay) - (fromEnum firstDay) `mod` size
  in
    if ringDistance < 0 -- if it's 1 day behind, it's actually 6 days ahead
    then ringDistance + size
    else ringDistance

{-

In the following block of tasks you need to implement 'Semigroup'
instances for all types and 'Monoid' instances if it's possible to
have a lawful 'Monoid' instance.

-}

newtype Gold = Gold
    { unGold :: Int
    } deriving (Show, Eq)

-- | Addition of gold coins.
instance Semigroup Gold where
  (<>) :: Gold -> Gold -> Gold
  (<>) a b = Gold (unGold a + unGold b)

instance Monoid Gold where
  mempty :: Gold 
  mempty = Gold 0


{- | A reward for completing a difficult quest says how much gold
you'll receive and whether you'll get a special reward.

If you combine multiple rewards, the final reward will contain a
special prize if at least one of the rewards is special.
-}
data Reward = Reward
    { rewardGold    :: Gold
    , rewardSpecial :: Bool
    } deriving (Show, Eq)

instance Semigroup Reward where
--  a <> b = Reward (rewardGold a <> rewardGold b) (rewardSpecial a || rewardSpecial b)
  Reward a b <> Reward c d = Reward (a <> c) (b || d)
  
instance Monoid Reward where
  mempty = Reward mempty False


{- | 'List1' is a list that contains at least one element.
-}
data List1 a = List1 a [a]
    deriving (Show, Eq)

-- | This should be list append.
instance Semigroup (List1 a) where
  List1 x xs <> List1 y ys = List1 x $ xs ++ y:ys
  -- just learned about $. not sure if this is a good use of it.
  -- feels weird to think about applying the left side to the right side in this case
  -- bc it's part of a constructor

{- | Does 'List1' have the 'Monoid' instance? If no then why?

hmm, we would need to identify a neutral element of polymorphic type a for the given
mappend function of list concat. it would need to be appendable to either side of
an arbitrary value of type List1 and not change the value. given that List1 needs
at least 1 element in it, then it seems contradictory to append it to another List1 value
and expect the value not to change -> the result would be a list of longer length.

maybe we could introduce some sentinel value that we agree to ignore if we see it?
we would need to do this for every type a though. maybe if we change List1 to be of
type Maybe a instead...

instance Monoid (List1 a) where
-}

{- | When fighting a monster, you can either receive some treasure or
don't.
-}
data Treasure a
    = NoTreasure
    | SomeTreasure a
    deriving (Show, Eq)

{- | When you append multiple treasures for fighting multiple
monsters, you should get a combined treasure and not just the first
(or last one).

🕯 HINT: You may need to add additional constraints to this instance
  declaration.
-}
instance Semigroup z => Semigroup (Treasure z) where
  -- NoTreasure <> NoTreasure = NoTreasure
  -- NoTreasure <> SomeTreasure a = SomeTreasure a
  -- SomeTreasure a <> NoTreasure = SomeTreasure a
  NoTreasure <> a = a
  a <> NoTreasure = a
  SomeTreasure a <> SomeTreasure b = SomeTreasure (a <> b)

instance Semigroup z => Monoid (Treasure z) where
  mempty = NoTreasure

{- | Abstractions are less helpful if we can't write functions that
use them!

Implement a polymorphic function that takes three elements and appends
together only different elements.

>>> appendDiff3 [1] [3, 2] [0, 5]
[1,3,2,0,5]
>>> appendDiff3 [4] [2, 2] [2, 2]
[4,2,2]
>>> appendDiff3 [1 .. 5] [1 .. 5] [1 .. 5]
[1,2,3,4,5]
>>> appendDiff3 (Product 2) (Product 3) (Product 3)
Product {getProduct = 6}

-}

appendDiff3 :: (Eq a, Semigroup a) => a -> a -> a -> a
appendDiff3 x y z
  | z /= x && z /= y = appendDiff2 (appendDiff2 x y) z
  | otherwise        = appendDiff2 x y 

-- despite ScopedTypeVariables, looks like we can still use them as value variables
appendDiff2 :: (Eq a, Semigroup a) => a -> a -> a
appendDiff2 a b
  | a == b    = a
  | otherwise = a <> b

{-

In the next block of tasks, implement 'Foldable' instances for all
types that can have such an instance.

♫ NOTE: Implement both 'foldr' and 'foldMap' methods. On the one hand,
  'Foldable' is a big typeclass but lets focus on its small part to get
  the main idea. On the other hand, these two methods are quite
  different so it's a good practice.

🕯 HINT: Check kinds of types to see whether it's possible to implement
  an instance of 'Foldable'.

🕯 HINT: If you don't feel comfortable with kinds yet, alternatively
  you can try uncommenting each instance one by one and see the GHC
  error. The compiler will "kindly" tell you if it's impossible to have
  such an instance.

🕯 HINT: Write explicit type signature of methods using InstanceSigs
  (already enabled in this module).

♫ NOTE: Since the instances are commented, the tests are also commented.
  To run tests for your instances, go to the "test/Test/Lecture3.hs" file
  and uncomment all commented tests. But do this only after you
  implement instances! No spoilers :)
-}

-- foldable requires a type to be of kind * -> *
-- only list1 and treasure are of that kind. the rest have arity 0
-- I thought that List1 not being a monoid would prevent it from being
-- foldable, but that isn't actually the case. foldMap creates a monoid
-- out of the structure using the function passed in (which I guess is obvious)

-- instance Foldable Weekday where
-- instance Foldable Gold where
-- instance Foldable Reward where

instance Foldable List1 where
  foldr :: (a -> b -> b) -> b -> List1 a -> b
  foldr f b (List1 a as) = foldr f b (a:as)
--  foldr f b (List1 a []) = f a b
--  foldr f b (List1 a (c:as)) = f a $ foldr f b (List1 c as)

  foldMap :: Monoid m => (a -> m) -> List1 a -> m
  foldMap f (List1 a as) = foldMap f (a:as)
--  foldMap f (List1 a []) = f a -- we never need to use mempty
--  foldMap f (List1 a (b:as)) = f a <> foldMap f (List1 b as)
  
instance Foldable Treasure where
  foldr :: (a -> b -> b) -> b -> Treasure a -> b
  foldr _ b NoTreasure = b
  foldr f b (SomeTreasure a) = f a b
  
  foldMap :: Monoid m => (a -> m) -> Treasure a -> m
  foldMap _ NoTreasure = mempty
  foldMap f (SomeTreasure a) = f a 
    

{-

In the next block of tasks, implement 'Functor' instances for all
types that can have such an instance.

🕯 HINT: At this point, you already know which types can have 'Functor'
  instance and which don't (the same types as for 'Foldable' in this
  case). But comments still mention all types to avoid spoilers ;)
-}

-- instance Functor Weekday where
-- instance Functor Gold where
-- instance Functor Reward where
instance Functor List1 where
  fmap :: (a -> b) -> List1 a -> List1 b
  fmap f (List1 a1 as) = List1 (f a1) (map f as)

instance Functor Treasure where
  fmap :: (a -> b) -> Treasure a -> Treasure b
  fmap _ NoTreasure = NoTreasure
  fmap f (SomeTreasure a) = SomeTreasure (f a)

                 
{- | Functions are first-class values in Haskell. This means that they
can be even stored inside other data types as well!

Now, you have a function inside some 'Functor'. You're a given an
element and you need to apply the function inside the 'Functor' to a
given element.

>>> apply 5 (Just (+ 3))
Just 8
>>> apply 5 Nothing
Nothing
>>> apply [1 .. 10] (Just (drop 7))
Just [8,9,10]
>>> apply 5 [(+ 3), (* 4), div 17]
[8,20,3]

-}
apply :: Functor t => a -> t (a -> b) -> t b
apply e c = fmap (\f -> f e) c
-- apply e f = fmap ($ e) f 
-- this took me many hours to figure out...and I don't understand why it works
-- since the first arg to @$@ needs to be a function. does haskell know to treat
-- @e@ as the second arg based on type? no, that's not it.
-- oh, it must be related to being an infix operator, given the following types:

-- Prelude> :t ($ 3)
-- ($ 3) :: Num a => (a -> b) -> b
-- Prelude> :t (($))
-- (($)) :: (a -> b) -> a -> b
-- Prelude> :t (($) 3)
-- (($) 3) :: Num (a -> b) => a -> b
-- Prelude> :t (3 $)
-- (3 $) :: Num (a -> b) => a -> b

-- I wonder how I would write this implementation if @$@ was not infix...

-- btw, I really wanted to be able to pattern match on the higher-kinded type like:
-- @apply e (T f) = T (f e)@
-- but I couldn't figure out how to do it / if it is possible
-- (is it possible?)

