module Day17 where

import Data.Function ((&))
import Data.List.Split
import Data.List as L
import Data.Array as A
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes, isJust, fromJust)
import Text.ParserCombinators.ReadP as P
import Numeric (readInt)
import Data.Bits ((.&.), (.|.))

import Lib

{-
--- Day 17: No Such Thing as Too Much ---

The elves bought too much eggnog again - 150 liters this time. To fit it all into your refrigerator, you'll need to move it into smaller containers. You take an inventory of the capacities of the available containers.

For example, suppose you have containers of size 20, 15, 10, 5, and 5 liters. If you need to store 25 liters, there are four ways to do it:

    15 and 10
    20 and 5 (the first 5)
    20 and 5 (the second 5)
    15, 5, and 5

Filling all containers entirely, how many different combinations of containers can exactly fit all 150 liters of eggnog?

To begin, get your puzzle input.
-}

parse :: [String] -> [Integer]
parse ls = ls
         & map read

makes 0 _ = 1
makes t [] = 0
makes t (a:as)
  | t < a = makes t as
  | otherwise = makes (t - a) as + makes t as

day17 ls =
  let sizes = parse ls
  in makes 150 sizes

{-
--- Part Two ---

While playing with all the containers in the kitchen, another load of eggnog arrives! The shipping and receiving department is requesting as many containers as you can spare.

Find the minimum number of containers that can exactly fit all 150 liters of eggnog. How many different ways can you fill that number of containers and still hold exactly 150 litres?

In the example above, the minimum number of containers was two. There were three ways to use that many containers, and so the answer there would be 3.
-}

makes' :: [Integer] -> Integer -> [Integer] -> [[Integer]]
makes' acc 0 _ = [acc]
makes' _ t [] = []
makes' acc t (a:as)
  | t < a = makes' acc t as
  | otherwise = (makes' (a:acc) (t - a) as) ++ (makes' acc t as)

day17b ls =
  let sizes = parse ls
      ways = makes' [] 150 sizes
      numbers = map length ways
      min = minimum numbers
  in length $ filter (==min) numbers
