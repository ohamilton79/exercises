{- |
Module                  : Lecture2
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 2 of the Haskell Beginners course.

As in the previous section, implement functions and provide type
signatures. If the type signature is not already written, write the
most polymorphic type signature you can.

Unlike exercises to Lecture 1, this module also contains more
challenging exercises. You don't need to solve them to finish the
course but you can if you like challenges :)
-}

module Lecture2
    ( -- * Normal
      lazyProduct
    , duplicate
    , removeAt
    , evenLists
    , dropSpaces

    , Knight (..)
    , dragonFight

      -- * Hard
    , isIncreasing
    , merge
    , mergeSort

    , Expr (..)
    , Variables
    , EvalError (..)
    , eval
    , constantFolding
    ) where

-- VVV If you need to import libraries, do it after this line ... VVV
import Data.Char (isSpace)

-- ^^^ and before this line. Otherwise the test suite might fail  ^^^

{- | Implement a function that finds a product of all the numbers in
the list. But implement a lazier version of this function: if you see
zero, you can stop calculating product and return 0 immediately.

>>> lazyProduct [4, 3, 7]
84
-}
lazyProduct :: [Int] -> Int
lazyProduct (0:_) = 0
lazyProduct (x:xs) = x * lazyProduct xs
lazyProduct [] = 1

{- | Implement a function that duplicates every element in the list.

>>> duplicate [3, 1, 2]
[3,3,1,1,2,2]
>>> duplicate "cab"
"ccaabb"
-}
duplicate :: [a] -> [a]
duplicate (x:xs) = x : x : duplicate xs
duplicate [] = []

{- | Implement function that takes index and a list and removes the
element at the given position. Additionally, this function should also
return the removed element.

>>> removeAt 0 [1 .. 5]
(Just 1,[2,3,4,5])

>>> removeAt 10 [1 .. 5]
(Nothing,[1,2,3,4,5])
-}
removeAt :: Int -> [a] -> (Maybe a, [a])
{- 
removeAt index list
    | index < 0 || index >= length list = (Nothing, list)
    | otherwise = (Just (list !! index), take index list ++ drop (index + 1) list)
-}
removeAt index = removeHelper (max index (-1)) []
  where
      removeHelper :: Int -> [a] -> [a] -> (Maybe a, [a])
      removeHelper (-1) [] xs = (Nothing, xs)
      removeHelper 0 ys (x:xs) = (Just x, ys ++ xs)
      removeHelper _ ys [] = (Nothing, ys)
      removeHelper n ys (x:xs) = removeHelper (n - 1) (ys ++ [x]) xs


{- | Write a function that takes a list of lists and returns only
lists of even lengths.

>>> evenLists [[1,2,3], [3,1,2,7], [], [5, 7, 2]]
[[3,1,2,7],[]]

♫ NOTE: Use eta-reduction and function composition (the dot (.) operator)
  in this function.
-}
evenLists :: [[a]] -> [[a]]
evenLists (x:xs) = if (even . length) x
                   then x : evenLists xs
                   else evenLists xs
evenLists [] = []

{- | The @dropSpaces@ function takes a string containing a single word
or number surrounded by spaces and removes all leading and trailing
spaces.

>>> dropSpaces "   hello  "
"hello"
>>> dropSpaces "-200            "
"-200"

♫ NOTE: As in the previous task, use eta-reduction and function
  composition (the dot (.) operator) in this function.

🕯 HINT: look into Data.Char and Prelude modules for functions you may use.
-}

{- The following function works, but I need to come up with a better implementation... -}
dropSpaces :: String -> String
dropSpaces (x:y:xs)
  | isSpace x && isSpace y = dropSpaces xs
  | isSpace x && not (isSpace y) = y : dropSpaces xs
  | not (isSpace x) && isSpace y = [x]
  | not (isSpace x) && not (isSpace y) = x : y : dropSpaces xs
  | otherwise = []
dropSpaces (x:xs)
  | isSpace x = dropSpaces xs
  | otherwise = x : dropSpaces xs
dropSpaces _ = []
{- |

The next task requires to create several data types and functions to
model the given situation.

An evil dragon attacked a village of innocent citizens! After
returning to its lair, the dragon became hungry and ate one of its
treasure chests by accident.

The guild in the village found a brave knight to slay the dragon!
As a reward, the knight can take the treasure chest.

Below is the description of the fight and character specifications:

  * A chest contains a non-zero amount of gold and a possible treasure.
    When defining the type of a treasure chest, you don't know what
    treasures it stores insight, so your chest data type must be able
    to contain any possible treasure.
  * As a reward, knight takes all the gold, the treasure and experience.
  * Experience is calculated based on the dragon type. A dragon can be
    either red, black or green.
  * Red dragons grant 100 experience points, black dragons — 150, and green — 250.
  * Stomachs of green dragons contain extreme acid and they melt any
    treasure except gold. So green dragons has only gold as reward.
    All other dragons always contain treasure in addition to gold.
  * Knight tries to slay dragon with their sword. Each sword strike
    decreases dragon health by the "sword attack" amount. When the
    dragon health becomes zero or less, a dragon dies and the knight
    takes the reward.
  * After each 10 sword strikes, dragon breathes fire and decreases
    knight health by the amount of "dragon fire power". If the
    knight's health becomes 0 or less, the knight dies.
  * Additionally, each sword strike decreases "knight's endurance" by one.
    If knight's endurance becomes zero, they become tired and are not
    able to continue the fight so they run away.

Implement data types to describe treasure, knight and dragon.
And implement a function that takes a knight and a dragon and returns
one of the three possible fight outcomes.

You're free to define any helper functions.

🕯 HINT: If you find the description overwhelming to implement entirely
  from scratch, try modelling the problem in stages.

    1. Implement all custom data types without using polymorphism.
    2. Add @newtype@s for safety where you think is appropriate.
    3. Encode the fight result as a sum type.
    4. Add polymorphism.
    5. Make all invalid states unrepresentable. Think, how you can
       change your types to prevent green dragons from having any
       treasure besides gold (if you already haven't done this).
-}

-- TODO: Implement XP, Dragon colour, and Chest fully

-- some help in the beginning ;)
data Knight = Knight
    { knightHealth    :: Int
    , knightAttack    :: Int
    , knightEndurance :: Int
    }

data Chest = Chest
    {
      chestGold     :: Int
    , chestTreasure :: Bool
    }

data DragonColour = Red | Black | Green

data Dragon = Dragon
    {
      dragonColour    :: DragonColour
    , dragonHealth    :: Int
    , dragonFirePower :: Int
    }

data FightResult = KnightWins | KnightLoses | KnightRunsAway

{- Helper function to handle logic for when knight strikes dragon -}
swordStrike :: Knight -> Dragon -> Int -> FightResult
swordStrike (Knight kHealth kAttack kEndurance) (Dragon dColour dHealth dFirePower) strikeCount
  | kHealth <= 0 = KnightLoses
  | kEndurance <= 0 = KnightRunsAway
  | dHealth <= 0 = KnightWins
  -- dragon breathes fire + knight attacks
  | strikeCount == 10 = swordStrike (Knight (kHealth - dFirePower) kAttack (kEndurance - 1)) (Dragon dColour (dHealth - kAttack) dFirePower) (strikeCount + 1)
  -- knight attacks
  | otherwise = swordStrike (Knight kHealth kAttack (kEndurance - 1)) (Dragon dColour (dHealth - kAttack) dFirePower) (strikeCount + 1)

dragonFight :: Knight -> Dragon -> FightResult
dragonFight (Knight kHealth kAttack kEndurance) (Dragon dColour dHealth dFirePower)
  | kHealth == 0 = KnightLoses
  | kEndurance == 0 = KnightRunsAway
  | otherwise = swordStrike (Knight kHealth kAttack kEndurance) (Dragon dColour dHealth dFirePower) 0

----------------------------------------------------------------------------
-- Extra Challenges
----------------------------------------------------------------------------

{- The following exercises are considered optional. Some of them might be more
challenging. However, you still may find some of them easier than some of the
previous ones. Difficulty is a relative concept.
-}

{- | Write a function that takes a list of numbers and returns 'True'
if all the numbers are in the increasing order (i.e. the list is
sorted).

>>> isIncreasing [3, 1, 2]
False
>>> isIncreasing [1 .. 10]
True
-}
isIncreasing :: [Int] -> Bool
isIncreasing [] = True
isIncreasing [_] = True
isIncreasing (x:y:xs)
  | x <= y = isIncreasing (y:xs)
  | otherwise = False
{-
isIncreasing list = increasingHelper 0 where
    increasingHelper :: Int -> Bool
    increasingHelper index
      | index == length list - 1 = True
      | list !! index > list !! (index + 1) = False
      | otherwise = increasingHelper (index + 1)
-}
{- | Implement a function that takes two lists, sorted in the
increasing order, and merges them into new list, also sorted in the
increasing order.

The lists are guaranteed to be given sorted, so you don't need to
verify that.

>>> merge [1, 2, 4] [3, 7]
[1,2,3,4,7]
-}
merge :: [Int] -> [Int] -> [Int]
merge listA listB
    | null listA && null listB = []
    | null listA = head listB : merge [] (tail listB)
    | null listB = head listA : merge (tail listA) []
    | head listA < head listB = head listA : merge (tail listA) listB
    | otherwise = head listB : merge listA (tail listB)

{- | Implement the "Merge Sort" algorithm in Haskell. The @mergeSort@
function takes a list of numbers and returns a new list containing the
same numbers but in the increasing order.

The algorithm of merge sort is the following:

  1. If the given list has less than 2 elements, it's already sorted.
  2. Otherwise, split list into two lists of the same size.
  3. Sort each of two lists recursively.
  4. Merge two resulting sorted lists to get a new sorted list.

>>> mergeSort [3, 1, 2]
[1,2,3]
-}
mergeSort :: [Int] -> [Int]
mergeSort list
  | length list < 2 = list
  | otherwise = let mid = length list `div` 2
              in merge (mergeSort (take mid list)) (mergeSort (drop mid list))


{- | Haskell is famous for being a superb language for implementing
compilers and interpeters to other programming languages. In the next
tasks, you need to implement a tiny part of a compiler.

We're going to work on a small subset of arithmetic operations.

In programming we write expressions like "x + 1" or "y + x + 10".
Such expressions can be represented in a more structured way (than a
string) using the following recursive Algebraic Data Type:
-}
data Expr
    = Lit Int
    | Var String
    | Add Expr Expr
    deriving (Show, Eq)

{- Now, you can use this data type to describe such expressions:

> x + 1
Add (Var "x") (Lit 1)

> y + x + 10
Add (Var "y") (Add (Var "x") (Lit 10))
-}

{- | We want to evaluate such expressions. We can associate a value
with a variable using a list of pairs.

You can use the @lookup@ function to search in this list by a variable name:

 * https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:lookup
-}
type Variables = [(String, Int)]

{- | Unfortunately, it's not guaranteed that variables in our @Expr@
data type are present in the given list. So we're going to introduce a
separate data for possible evaluation errors.

Normally, this would be a sum type with several constructors
describing all possible errors. But we have only one error in our
evaluation process.
-}
data EvalError
    = VariableNotFound String
    deriving (Show, Eq)

{- | Having all this set up, we can finally implement an evaluation function.
It returns either a successful evaluation result or an error.
-}
eval :: Variables -> Expr -> Either EvalError Int
eval variables expression =
  case expression of
    Lit a -> Right a
    Var a -> case lookup a variables of
                               Nothing -> Left (VariableNotFound a)
                               Just x -> Right x
    Add a b -> case eval variables a of
                   Left err -> Left err
                   Right x -> case eval variables b of
                                  Left err -> Left err
                                  Right y -> Right (x + y)


{- | Compilers also perform optimizations! One of the most common
optimizations is "Constant Folding". It performs arithmetic operations
on all constants known during compile time. This way you can write
more verbose and clear code that works as efficient as its shorter
version.

For example, if you have an expression:

x + 10 + y + 15 + 20

The result of constant folding can be:

x + y + 45

It also can be:

x + 45 + y

Write a function that takes and expression and performs "Constant
Folding" optimization on the given expression.
-}
constantFolding :: Expr -> Expr
-- constantFolding expression = foldingHelper expression 0 where
  -- foldingHelper :: Expr -> Int -> Expr
  {-
  foldingHelper (Lit x) c = Lit (x + c)
  foldingHelper (Var x) 0 = Var x
  foldingHelper (Var x) c = Add (Var x) (Lit c)
  foldingHelper (Add (Lit x) (Lit y)) c = Lit (x + y + c)
  foldingHelper (Add (Var x) (Lit y)) c = Add (Var x) (Lit (y + c))
  foldingHelper (Add (Lit x) (Var y)) c = Add (Var y) (Lit (x + c))
  foldingHelper (Add (Lit x) exprA) c = foldingHelper exprA (c + x)
  foldingHelper (Add (Var x) exprA) c = Add (Var x) (foldingHelper exprA c)
  foldingHelper (Add exprA (Var x)) c = Add (Var x) (foldingHelper exprA c)
  foldingHelper (Add exprA (Lit x)) c = foldingHelper exprA (c + x)
  foldingHelper (Add exprA exprB) c = Add (foldingHelper exprA c) (foldingHelper exprB 0)
  -}
{-constantFolding (Lit x) = Lit x
constantFolding (Var x) = Var x
constantFolding (Add (Lit x) (Lit y)) = Lit (x + y)
constantFolding (Add (Lit x) (Add (Lit y) a)) = constantFolding (Add (Lit (x + y)) a)
constantFolding (Add (Lit x) (Add a (Lit y))) = constantFolding (Add (Lit (x + y)) a)
constantFolding (Add (Add (Lit y) a) (Lit x)) = constantFolding (Add (Lit (x + y)) a)
constantFolding (Add (Add a (Lit y)) (Lit x)) = constantFolding (Add (Lit (x + y)) a)
constantFolding (Add a (Add (Lit x) b)) = constantFolding (Add (Lit x) (Add a b))
constantFolding (Add a (Add b (Lit x))) = constantFolding (Add (Lit x) (Add a b))
constantFolding (Add (Add (Lit x) b) a) = constantFolding (Add (Lit x) (Add a b))
constantFolding (Add (Add b (Lit x)) a) = constantFolding (Add (Lit x) (Add a b))
constantFolding (Add x y) = (Add (constantFolding x) (constantFolding y))
-}
constantFolding (Lit x) = Lit x
constantFolding (Var x) = Var x
constantFolding (Add a (Lit 0)) = constantFolding a
constantFolding (Add (Lit 0) a) = constantFolding a
constantFolding (Add a (Add (Var y) b)) = constantFolding (Add (Var y) ((Add a b)))
constantFolding (Add (Add (Var y) a) b) = constantFolding (Add (Var y) ((Add a b)))
constantFolding (Add a (Add b (Var y))) = constantFolding (Add (Var y) ((Add a b)))
constantFolding (Add (Add a (Var y)) b) = constantFolding (Add (Var y) ((Add a b)))
constantFolding (Add (Lit x) (Lit y)) = Lit (x + y)
{-
constantFolding (Add (Lit x) (Add (Lit y) (Var a))) = Add (Var a) (Lit (x + y))
constantFolding (Add (Lit x) (Add (Var a) (Lit y))) = Add (Var a) (Lit (x + y))
-}
constantFolding (Add a b)
    | constantFolding a == Lit 0 = constantFolding b
    | constantFolding b == Lit 0 = constantFolding a
    | otherwise = Add (constantFolding a) (constantFolding b)
