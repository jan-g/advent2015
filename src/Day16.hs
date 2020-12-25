module Day16 where

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
--- Day 16: Aunt Sue ---

Your Aunt Sue has given you a wonderful gift, and you'd like to send her a thank you card. However, there's a small problem: she signed it "From, Aunt Sue".

You have 500 Aunts named "Sue".

So, to avoid sending the card to the wrong person, you need to figure out which Aunt Sue (which you conveniently number 1 to 500, for sanity) gave you the gift. You open the present and, as luck would have it, good ol' Aunt Sue got you a My First Crime Scene Analysis Machine! Just what you wanted. Or needed, as the case may be.

The My First Crime Scene Analysis Machine (MFCSAM for short) can detect a few specific compounds in a given sample, as well as how many distinct kinds of those compounds there are. According to the instructions, these are what the MFCSAM can detect:

    children, by human DNA age analysis.
    cats. It doesn't differentiate individual breeds.
    Several seemingly random breeds of dog: samoyeds, pomeranians, akitas, and vizslas.
    goldfish. No other kinds of fish.
    trees, all in one group.
    cars, presumably by exhaust or gasoline or something.
    perfumes, which is handy, since many of your Aunts Sue wear a few kinds.

In fact, many of your Aunts Sue have many of these. You put the wrapping from the gift into the MFCSAM. It beeps inquisitively at you a few times and then prints out a message on ticker tape:

children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1

You make a list of the things you can remember about each Aunt Sue. Things missing from your list aren't zero - you simply don't remember the value.

What is the number of the Sue that got you the gift?

To begin, get your puzzle input.
-}

seeking = Map.fromList [ ("children", 3)
                       , ("cats", 7)
                       , ("samoyeds", 2)
                       , ("pomeranians", 3)
                       , ("akitas", 0)
                       , ("vizslas", 0)
                       , ("goldfish", 5)
                       , ("trees", 3)
                       , ("cars", 2)
                       , ("perfumes", 1)
                       ]

parse ls = ls
         & map (quickParse parseLine)
         & catMaybes
         & Map.fromList

parseLine :: ReadP (String, Map.Map String Integer)
parseLine = do
  word <- many1 (satisfy (/= ':'))
  string ": "
  items <- P.sepBy parseItem (string ", ")
  eof
  return (word, Map.fromList items)

parseItem :: ReadP (String, Integer)
parseItem = do
  word <- many1 (satisfy isAlpha)
  string ": "
  n <- natParser
  return (word, n)

matches s m =
  -- are all elements of m the same as sought?
  let nonMatching = Map.filterWithKey (\k v -> s Map.! k /= v) m
  in  Map.null nonMatching

day16 ls =
  let candidates = parse ls
      sought = Map.filter (matches seeking) candidates
  in (sought, candidates)

{-
--- Part Two ---

As you're about to send the thank you note, something in the MFCSAM's instructions catches your eye. Apparently, it has an outdated retroencabulator, and so the output from the machine isn't exact values - some of them indicate ranges.

In particular, the cats and trees readings indicates that there are greater than that many (due to the unpredictable nuclear decay of cat dander and tree pollen), while the pomeranians and goldfish readings indicate that there are fewer than that many (due to the modial interaction of magnetoreluctance).

What is the number of the real Aunt Sue?
-}

day16b ls =
  let candidates = parse ls
      matcher = Map.map (\v -> (==v)) seeking
      matcher' = matcher
               & Map.insert "trees" (> (seeking Map.! "trees"))
               & Map.insert "cats" (> (seeking Map.! "cats"))
               & Map.insert "goldfish" (< (seeking Map.! "goldfish"))
               & Map.insert "pomeranians" (< (seeking Map.! "pomeranians"))
  in Map.filter (noMatch matcher') candidates
  where noMatch criteria m =
          let diffs = Map.filterWithKey (\k v -> not $ criteria Map.! k $ v) m
          in  Map.null diffs
               