module Day9 where

import Lib
import Data.Function ((&))
import Data.List.Split
import Data.List
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes)
import Text.ParserCombinators.ReadP

{-
--- Day 9: All in a Single Night ---

Every year, Santa manages to deliver all of his presents in a single night.

This year, however, he has some new locations to visit; his elves have provided him the distances between every pair of
locations. He can start and end at any two (different) locations he wants, but he must visit each location exactly once.
What is the shortest distance he can travel to achieve this?

For example, given the following distances:

London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141

The possible routes are therefore:

Dublin -> London -> Belfast = 982
London -> Dublin -> Belfast = 605
London -> Belfast -> Dublin = 659
Dublin -> Belfast -> London = 659
Belfast -> Dublin -> London = 605
Belfast -> London -> Dublin = 982

The shortest of these is London -> Dublin -> Belfast = 605, and so the answer is 605 in this example.

What is the distance of the shortest route?
-}

parse ls = ls
         & map parseLine
         & catMaybes

data Dist = Route String String Integer
  deriving (Show, Eq, Ord)

parseLine :: String -> Maybe Dist
parseLine s =
  case readP_to_S lineParser s of
    [] -> Nothing
    [(m, "")] -> Just m

lineParser = do
  from <- many1 (satisfy isAlpha)
  string " to "
  to <- many1 (satisfy isAlpha)
  string " = "
  d <- intParser
  return $ Route from to d


src (Route f _ _) = f
dst (Route _ t _) = t


day9 ls =
  let rs = parse ls
      m1 = Map.fromList $ [(s, ds) | s <- Set.toList srcs,
                                    let ds = Map.fromList [(d, dist) | r@(Route _ d dist) <- rs,
                                                                       src r == s]]
      m2 = Map.fromList $ [(s, ds) | s <- Set.toList dsts,
                                    let ds = Map.fromList [(d, dist) | r@(Route d _ dist) <- rs,
                                                                       dst r == s]]
      m = Map.unionWith (Map.union) m1 m2 
      srcs = map src rs & Set.fromList
      dsts = map dst rs & Set.fromList
      places = Set.union srcs dsts
      routes = permutations (Set.toList places)
      scores = map (score m) routes & catMaybes
  in  (m, length routes, minimum scores, scores)


distance :: Map.Map String (Map.Map String Integer) -> String -> String -> Maybe Integer
distance m from to = do
  paths <- Map.lookup from m
  dist <- Map.lookup to paths
  return dist


score :: Map.Map String (Map.Map String Integer) -> [String] -> Maybe Integer
score m (start:rest) =
  score' 0 start rest
  where
    score' :: Integer -> String -> [String] -> Maybe Integer
    score' a _ [] = Just a
    score' a at (next:rest) =
      case distance m at next of
        Nothing -> Nothing
        Just d -> score' (a + d) next rest
           

{-
--- Part Two ---

The next year, just to show off, Santa decides to take the route with the longest distance instead.

He can still start and end at any two (different) locations he wants, and he still must visit each location exactly once.

For example, given the distances above, the longest route would be 982 via (for example) Dublin -> London -> Belfast.

What is the distance of the longest route?
-}

day9b ls =
  let rs = parse ls
      m1 = Map.fromList $ [(s, ds) | s <- Set.toList srcs,
                                    let ds = Map.fromList [(d, dist) | r@(Route _ d dist) <- rs,
                                                                       src r == s]]
      m2 = Map.fromList $ [(s, ds) | s <- Set.toList dsts,
                                    let ds = Map.fromList [(d, dist) | r@(Route d _ dist) <- rs,
                                                                       dst r == s]]
      m = Map.unionWith (Map.union) m1 m2 
      srcs = map src rs & Set.fromList
      dsts = map dst rs & Set.fromList
      places = Set.union srcs dsts
      routes = permutations (Set.toList places)
      scores = map (score m) routes & catMaybes
  in  maximum scores
