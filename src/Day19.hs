module Day19 where

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
import Debug.Trace (trace)

import Lib

{-
--- Day 19: Medicine for Rudolph ---

Rudolph the Red-Nosed Reindeer is sick! His nose isn't shining very brightly, and he needs medicine.

Red-Nosed Reindeer biology isn't similar to regular reindeer biology; Rudolph is going to need custom-made medicine. Unfortunately, Red-Nosed Reindeer chemistry isn't similar to regular reindeer chemistry, either.

The North Pole is equipped with a Red-Nosed Reindeer nuclear fusion/fission plant, capable of constructing any Red-Nosed Reindeer molecule you need. It works by starting with some input molecule and then doing a series of replacements, one per step, until it has the right molecule.

However, the machine has to be calibrated before it can be used. Calibration involves determining the number of molecules that can be generated in one step from a given starting point.

For example, imagine a simpler machine that supports only the following replacements:

H => HO
H => OH
O => HH

Given the replacements above and starting with HOH, the following molecules could be generated:

    HOOH (via H => HO on the first H).
    HOHO (via H => HO on the second H).
    OHOH (via H => OH on the first H).
    HOOH (via H => OH on the second H).
    HHHH (via O => HH).

So, in the example above, there are 4 distinct molecules (not five, because HOOH appears twice) after one replacement from HOH. Santa's favorite molecule, HOHOHO, can become 7 distinct molecules (over nine replacements: six from H, and three from O).

The machine replaces without regard for the surrounding characters. For example, given the string H2O, the transition H => OO would result in OO2O.

Your puzzle input describes all of the possible replacements and, at the bottom, the medicine molecule for which you need to calibrate the machine. How many distinct molecules can be created after all the different ways you can do one replacement on the medicine molecule?

To begin, get your puzzle input.
-}

parse ls =
  let [rules, [mol]] = ls & splitOn [""]
  in (parseRules rules, parseMolecule mol)

parseRules ls = ls & map (quickParse parseRule) & catMaybes & Map.fromListWith Set.union

parseRule :: ReadP (String, Set.Set [String])
parseRule = do
  m <- many1 (satisfy isAlpha)
  string " => "
  res <- many1 (satisfy isAlpha)
  eof
  return (m, Set.singleton $ parseMolecule res)

parseMolecule :: String -> [String]
parseMolecule "" = []
parseMolecule (a:as)
  | isUpper a = let (rest, others) = span isLower as  :: (String, String)
                in [a:rest] ++ (parseMolecule others)

alternatives :: Map.Map String (Set.Set [String]) -> [String] -> Set.Set [String]
alternatives rs [] = Set.singleton []
alternatives rs (a:as) =
  case Map.lookup a rs of
    Just options -> let changeThis = Set.map (++as) options
                        changeOther = Set.map (a:) (alternatives rs as)
                    in  Set.union changeThis changeOther
    Nothing -> Set.map (a:) (alternatives rs as)

alts rs m = Set.delete m (alternatives rs m)

day19 ls =
  let (rs, mol) = parse ls
  in alts rs mol & Set.size

{-
--- Part Two ---

Now that the machine is calibrated, you're ready to begin molecule fabrication.

Molecule fabrication always begins with just a single electron, e, and applying replacements one at a time, just like the ones during calibration.

For example, suppose you have the following replacements:

e => H
e => O
H => HO
H => OH
O => HH

If you'd like to make HOH, you start with e, and then make the following replacements:

    e => O to get O
    O => HH to get HH
    H => OH (on the second H) to get HOH

So, you could make HOH after 3 steps. Santa's favorite molecule, HOHOHO, can be made in 6 steps.

How long will it take to make the medicine? Given the available replacements and the medicine molecule in your puzzle input, what is the fewest number of steps to go from e to the medicine molecule?
-}

step :: Map.Map String (Set.Set [String]) -> Set.Set [String] -> Set.Set [String]
step rs as = Set.unions (Set.map (alts rs) as)

search stopWhen next acc n
  | stopWhen acc = (acc, n)
  | otherwise    = search stopWhen next (next acc) (succ n)

iterations :: Map.Map String (Set.Set [String]) -> [String] -> ((Set.Set [String], Set.Set [String]), Integer)
iterations rs sought = search (\(old, now) -> Set.member sought now || Set.null now)
                              (\(old, now) ->
                                 let new = step rs now & Set.filter (\n -> length n <= length sought)
                                     prev = Set.union old now
                                 in (prev, new `Set.difference` prev))
                              (Set.empty, Set.singleton ["e"]) 0

-- the above are too tardy on the input. Instead, begin with a longer sequence and make it shorter
expandRules :: Map.Map String (Set.Set [String]) -> [(String, [String])]
expandRules rs = rs
               & Map.toList
               & concatMap (\(k, vs) -> [(k, v) | v <- Set.toList vs])

precursors :: [(String, [String])] -> [String] -> Set.Set ([String])
precursors _ [] = Set.singleton []
precursors rs (x:xs) =
   let reduceFirst = map (\(k, v) -> case stripPrefix v (x:xs) of
                                       Just rest -> Just (k:rest)
                                       Nothing -> Nothing) rs
                   & catMaybes
                   & Set.fromList
       reduceRest = Set.map (x:) (precursors rs xs)
   in Set.union reduceFirst reduceRest

precs :: [(String, [String])] -> [String] -> Set.Set ([String])
precs rs m = Set.delete m (precursors rs m)

shortest :: Set.Set ([String]) -> Set.Set ([String])
shortest ps = ps & Set.filter (\l -> length l == minimum (Set.map length ps))

iterations' :: Map.Map String (Set.Set [String]) -> [String] -> Integer
iterations' rs from =
  let rs' = expandRules rs
      (n, _, _) = until (\(i, ss, prev) -> Set.null ss || Set.member ["e"] ss || prev == ss)
               (\(i, ss, prev) ->
                  let ps = Set.map (precs rs') ss  :: Set.Set (Set.Set [String])
                  in  trace (show (i, Set.size ss)) (i + 1, Set.singleton $ head $ Set.toList $ shortest $ Set.unions ps, ss))
               (0, Set.singleton from, Set.empty)
  in n

day19b ls =
  let (rs, mol) = parse ls
--  in  (length mol) - (length $ filter (=="Rn") mol)
--                   - (length $ filter (=="Ar") mol)
--                   - (length $ filter (=="Y") mol) * 2
--                   - 1
  in iterations' rs mol
