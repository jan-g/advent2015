module Day21 where

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
import Control.Monad

import Lib

{-
--- Day 21: RPG Simulator 20XX ---

Little Henry Case got a new video game for Christmas. It's an RPG, and he's stuck on a boss. He needs to know what equipment to buy at the shop. He hands you the controller.

In this game, the player (you) and the enemy (the boss) take turns attacking. The player always goes first. Each attack reduces the opponent's hit points by at least 1. The first character at or below 0 hit points loses.

Damage dealt by an attacker each turn is equal to the attacker's damage score minus the defender's armor score. An attacker always does at least 1 damage. So, if the attacker has a damage score of 8, and the defender has an armor score of 3, the defender loses 5 hit points. If the defender had an armor score of 300, the defender would still lose 1 hit point.

Your damage score and armor score both start at zero. They can be increased by buying items in exchange for gold. You start with no items and have as much gold as you need. Your total damage or armor is equal to the sum of those stats from all of your items. You have 100 hit points.

Here is what the item shop is selling:

Weapons:    Cost  Damage  Armor
Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0

Armor:      Cost  Damage  Armor
Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5

Rings:      Cost  Damage  Armor
Damage +1    25     1       0
Damage +2    50     2       0
Damage +3   100     3       0
Defense +1   20     0       1
Defense +2   40     0       2
Defense +3   80     0       3

You must buy exactly one weapon; no dual-wielding. Armor is optional, but you can't use more than one. You can buy 0-2 rings (at most one for each hand). You must use any items you buy. The shop only has one of each item, so you can't buy, for example, two rings of Damage +3.

For example, suppose you have 8 hit points, 5 damage, and 5 armor, and that the boss has 12 hit points, 7 damage, and 2 armor:

    The player deals 5-2 = 3 damage; the boss goes down to 9 hit points.
    The boss deals 7-5 = 2 damage; the player goes down to 6 hit points.
    The player deals 5-2 = 3 damage; the boss goes down to 6 hit points.
    The boss deals 7-5 = 2 damage; the player goes down to 4 hit points.
    The player deals 5-2 = 3 damage; the boss goes down to 3 hit points.
    The boss deals 7-5 = 2 damage; the player goes down to 2 hit points.
    The player deals 5-2 = 3 damage; the boss goes down to 0 hit points.

In this scenario, the player wins! (Barely.)

You have 100 hit points. The boss's actual stats are in your puzzle input. What is the least amount of gold you can spend and still win the fight?

To begin, get your puzzle input.
-}

data Character = C { hp :: Integer
                   , damage :: Integer
                   , armour :: Integer
                   }
  deriving (Show, Eq)


parse ls =
  let [hp, d, a] = ls
         & map (splitOn ": ")
         & map (!! 1)
         & map read
  in C { hp=hp, damage=d, armour=a }

fight :: Character -> Character -> Either Character Character
fight player enemy =
  let playerDamage = 1 `max` (damage player - armour enemy)
      enemyDamage = 1 `max` (damage enemy - armour player)
      turnsToKillEnemy = (hp enemy + playerDamage - 1) `div` playerDamage
      turnsToKillPlayer = (hp player + enemyDamage - 1) `div` enemyDamage
  in if turnsToKillEnemy <= turnsToKillPlayer
  then Left $ player { hp=hp player - (turnsToKillEnemy - 1) * enemyDamage }
  else Right $ enemy { hp=hp enemy - turnsToKillPlayer * playerDamage }

equipPlayer enemy = do
  weapon@(wn,wc,wd) <-
            [ ("Dagger", 8, 4)
            , ("Shortsword", 10, 5)
            , ("Warhammer", 25, 6)
            , ("Longsword", 40, 7)
            , ("Greataxe", 74, 8)
            ]
  armour@(an,ac,aa) <-
            [ ("Nothing", 0, 0)
            , ("Leather", 13, 1)
            , ("Chainmail", 31, 2)
            , ("Splintmail", 53, 3)
            , ("Bandedmail", 75, 4)
            , ("Platemail", 102, 5)
            ]
  ring1@(r1n,r1c,r1d,r1a) <-
           [ ("No ring 1", 0, 0, 0)
           , ("Damage +1", 25, 1, 0)
           , ("Damage +2", 50, 2, 0)
           , ("Damage +3", 100, 3, 0)
           , ("Defense +1", 20, 0, 1)
           , ("Defense +2", 40, 0, 2)
           , ("Defense +3", 80, 0, 3)
           ]
  ring2@(r2n,r2c,r2d,r2a) <-
           [ ("No ring 2", 0, 0, 0)
           , ("Damage +1", 25, 1, 0)
           , ("Damage +2", 50, 2, 0)
           , ("Damage +3", 100, 3, 0)
           , ("Defense +1", 20, 0, 1)
           , ("Defense +2", 40, 0, 2)
           , ("Defense +3", 80, 0, 3)
           ]
  guard $ ring1 /= ring2
  let player = C { hp=100, damage=wd + r1d + r2d, armour=aa + r1a + r2a }
      cost = wc + ac + r1c + r2c
  case fight player enemy of
    Left _ -> return (cost, wn, an, r1n, r2n)
    Right _ -> mzero

    
day21 ls =
  let enemy = parse ls
  in minimum (equipPlayer enemy)

{-
-}

day21b ls = "hello world"
