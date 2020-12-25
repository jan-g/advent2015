module Day12 where

import Data.Function ((&))
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Text.ParserCombinators.ReadP
import Lib
import Data.Maybe (catMaybes)

{-
--- Day 12: JSAbacusFramework.io ---

Santa's Accounting-Elves need help balancing the books after a recent order. Unfortunately, their accounting software
uses a peculiar storage format. That's where you come in.

They have a JSON document which contains a variety of things: arrays ([1,2,3]), objects ({"a":1, "b":2}), numbers, and
strings. Your first job is to simply find all of the numbers throughout the document and add them together.

For example:

    [1,2,3] and {"a":2,"b":4} both have a sum of 6.
    [[[3]]] and {"a":{"b":4},"c":-1} both have a sum of 3.
    {"a":[-1,1]} and [-1,{"a":1}] both have a sum of 0.
    [] and {} both have a sum of 0.

You will not encounter any strings containing numbers.

What is the sum of all numbers in the document?
-}


total ls = sumJson ls

sumJson s =
  case readP_to_S jsonSummer s of
              [] -> Nothing
              [(m, "")] -> Just m

jsonSummer :: ReadP Integer
jsonSummer = stringSummer <++ numberSummer <++ arraySummer <++ objectSummer

stringSummer :: ReadP Integer
stringSummer = do
  skipSpaces
  between (char '"') (char '"') (many $ satisfy (/= '"'))
  return 0

numberSummer :: ReadP Integer
numberSummer = do
  skipSpaces
  intParser

arraySummer :: ReadP Integer
arraySummer = do
  l <- between (skipSpaces >>>> char '[') (skipSpaces >>>> char ']')
               (sepBy jsonSummer (skipSpaces >>>> char ','))
  return $ sum l

objectSummer :: ReadP Integer
objectSummer = do
  l <- between (skipSpaces >>>> char '{') (skipSpaces >>>> char '}')
               (sepBy (stringSummer >>>> skipSpaces >>>> char ':' >>>> jsonSummer) (skipSpaces >>>> char ','))
  return $ sum l


day12 ls = ls & head & total

{-
--- Part Two ---

Uh oh - the Accounting-Elves have realized that they double-counted everything red.

Ignore any object (and all of its children) which has any property with the value "red". Do this only for objects ({...}), not arrays ([...]).

    [1,2,3] still has a sum of 6.
    [1,{"c":"red","b":2},3] now has a sum of 4, because the middle object is ignored.
    {"d":"red","e":[1,2,3,4],"f":5} now has a sum of 0, because the entire structure is ignored.
    [1,"red",5] has a sum of 6, because "red" in an array has no effect.

-}

total2 ls = sumJson2 ls

sumJson2 s =
  case readP_to_S jsonSummer2 s of
              [] -> Nothing
              [(m, "")] -> Just m

jsonSummer2 :: ReadP Integer
jsonSummer2 = stringSummer <++ numberSummer <++ arraySummer2 <++ objectSummer2

arraySummer2 :: ReadP Integer
arraySummer2 = do
  l <- between (skipSpaces >>>> char '[') (skipSpaces >>>> char ']')
               (sepBy jsonSummer2 (skipSpaces >>>> char ','))
  return $ sum l

jsonString :: ReadP String
jsonString = between (skipSpaces >>>> char '"') (char '"') (many $ satisfy (/= '"'))

objectSummer2 :: ReadP Integer
objectSummer2 = do
  l <-
    between
      (skipSpaces >>>> char '{')
      (skipSpaces >>>> char '}')
      (sepBy
         ((stringSummer >>>> skipSpaces >>>> char ':' >>>> jsonString <<!!
           (\s ->
              if s == "red"
                then Nothing
                else Just 0)) <++
          (stringSummer >>>> skipSpaces >>>> char ':' >>>> jsonSummer2 <<!! Just))
         (skipSpaces >>>> char ','))
  if elem Nothing l
    then return 0
    else return $ sum $ catMaybes l
    
day12b ls = ls & head & total2

