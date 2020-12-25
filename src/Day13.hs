module Day13 where

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
--- Day 13: Knights of the Dinner Table ---

In years past, the holiday feast with your family hasn't gone so well. Not everyone gets along! This year, you resolve,
will be different. You're going to find the optimal seating arrangement and avoid all those awkward conversations.

You start by writing up a list of everyone invited and the amount their happiness would increase or decrease if they
were to find themselves sitting next to each other person. You have a circular table that will be just big enough to
fit everyone comfortably, and so each person will have exactly two neighbors.

For example, suppose you have only four attendees planned, and you calculate their potential happiness as follows:

Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.

Then, if you seat Alice next to David, Alice would lose 2 happiness units (because David talks so much), but David
would gain 46 happiness units (because Alice is such a good listener), for a total change of 44.

If you continue around the table, you could then seat Bob next to Alice (Bob gains 83, Alice gains 54). Finally, seat
Carol, who sits next to Bob (Carol gains 60, Bob loses 7) and David (Carol gains 55, David gains 41). The arrangement
looks like this:

         +41 +46
    +55   David    -2
    Carol       Alice
    +60    Bob    +54
         -7  +83

After trying every other seating arrangement in this hypothetical scenario, you find that this one is the most optimal,
with a total change in happiness of 330.

What is the total change in happiness for the optimal seating arrangement of the actual guest list?
-}


parse ls = ls
         & map parseLine
         & catMaybes

parseLine s =
  case readP_to_S lineParser s of
              [] -> Nothing
              [(m, "")] -> Just m

word :: ReadP String
word = many1 (satisfy (/= ' '))

lineParser :: ReadP ((String, String), Integer)
lineParser = do
  first <- word
  string " would "
  lg <- word
  char ' '
  val <- intParser
  string " happiness units by sitting next to "
  second <- word
  char '.'
  return ((first, second), if lg == "gain" then val else -val)

day13 ls =
  let rules = parse ls
      rules' = Map.fromList rules
      who1 = Set.fromList $ map (\((a,_),_) -> a) rules
      who2 = Set.fromList $ map (\((_,b),_) -> b) rules
      who = Set.union who1 who2 & Set.toList
      perms = [score rules' w | w <- permutations who]
  in  maximum perms


score rules w = score' 0 (w ++ [head w])
  where
    score' acc [] = acc
    score' acc [_] = acc
    score' acc (a:b:xs) =
      score' (fromMaybe 0 (Map.lookup (a, b) rules) + fromMaybe 0 (Map.lookup (b, a) rules) + acc) (b : xs)

{-
--- Part Two ---

In all the commotion, you realize that you forgot to seat yourself. At this point, you're pretty apathetic toward the whole thing, and your happiness wouldn't really go up or down regardless of who you sit next to. You assume everyone else would be just as ambivalent about sitting next to you, too.

So, add yourself to the list, and give all happiness relationships that involve you a score of 0.

What is the total change in happiness for the optimal seating arrangement that actually includes yourself?

-}

day13b ls =
  let rules = parse ls
      rules' = Map.fromList rules
      who1 = Set.fromList $ map (\((a,_),_) -> a) rules
      who2 = Set.fromList $ map (\((_,b),_) -> b) rules
      who = Set.union who1 who2 & Set.toList & ("@":)
      perms = [score rules' w | w <- permutations who]
  in  maximum perms