module Day15 where

import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Text.ParserCombinators.ReadP
import Lib
import Data.List


{-
--- Day 15: Science for Hungry People ---

Today, you set out on the task of perfecting your milk-dunking cookie recipe. All you have to do is find the right
balance of ingredients.

Your recipe leaves room for exactly 100 teaspoons of ingredients. You make a list of the remaining ingredients you
could use to finish the recipe (your puzzle input) and their properties per teaspoon:

    capacity (how well it helps the cookie absorb milk)
    durability (how well it keeps the cookie intact when full of milk)
    flavor (how tasty it makes the cookie)
    texture (how it improves the feel of the cookie)
    calories (how many calories it adds to the cookie)

You can only measure ingredients in whole-teaspoon amounts accurately, and you have to be accurate so you can reproduce
your results in the future. The total score of a cookie can be found by adding up each of the properties (negative
totals become 0) and then multiplying together everything except calories.

For instance, suppose you have these two ingredients:

Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3

Then, choosing to use 44 teaspoons of butterscotch and 56 teaspoons of cinnamon (because the amounts of each ingredient
must add up to 100) would result in a cookie with the following properties:

    A capacity of 44*-1 + 56*2 = 68
    A durability of 44*-2 + 56*3 = 80
    A flavor of 44*6 + 56*-2 = 152
    A texture of 44*3 + 56*-1 = 76

Multiplying these together (68 * 80 * 152 * 76, ignoring calories for now) results in a total score of 62842880, which
happens to be the best score possible given these ingredients. If any properties had produced a negative total, it
would have instead become zero, causing the whole score to multiply to zero.

Given the ingredients in your kitchen and their properties, what is the total score of the highest-scoring cookie you
can make?

-}

parse ls = ls
         & map parseLine
         & catMaybes

parseLine s =
  case readP_to_S lineParser s of
              [] -> Nothing
              [(m, "")] -> Just m

lineParser :: ReadP (String, Integer, Integer, Integer, Integer, Integer)
lineParser = do
  what <- wordParser
  string ": capacity "
  cap <- intParser
  string ", durability "
  dur <- intParser
  string ", flavor "
  fla <- intParser
  string ", texture "
  tex <- intParser
  string ", calories "
  cal <- intParser
  return (what, cap, dur, fla, tex, cal)

amounts x 1 = [[x]]
amounts x d = [am : opt | am <- [0 .. x], opt <- amounts (x - am) (d - 1)]

day15 ls =
  let rules = parse ls
      n = length rules
      options = amounts 100 n
      scores = [score rules option | option <- options]
  in  maximum $ map fst scores


score rules am =
  let (c, d, f, t, cal) = score' (0, 0, 0, 0, 0) (zip am rules)
  in  if c < 0 || d < 0 || f < 0 || t < 0 then (0, cal)
      else (c * d * f * t, cal)
  where 
    score' acc [] = acc
    score' (c, d, f, t, cal) ((n, (_, c', d', f', t', cal')):rest) =
      score' (c + c' * n, d + d' * n, f + f' * n, t + t' * n, cal + cal' * n) rest

{-
--- Part Two ---

Your cookie recipe becomes wildly popular! Someone asks if you can make another recipe that has exactly 500 calories
per cookie (so they can use it as a meal replacement). Keep the rest of your award-winning process the same (100
teaspoons, same ingredients, same scoring system).

For example, given the ingredients above, if you had instead selected 40 teaspoons of butterscotch and 60 teaspoons of
cinnamon (which still adds to 100), the total calorie count would be 40*8 + 60*3 = 500. The total score would go down,
though: only 57600000, the best you can do in such trying circumstances.

Given the ingredients in your kitchen and their properties, what is the total score of the highest-scoring cookie you
can make with a calorie total of 500?

-}

day15b ls =
  let rules = parse ls
      n = length rules
      options = amounts 100 n
      scores = [score rules option | option <- options]
  in  maximum $ map fst $ filter ((== 500) . snd) scores
