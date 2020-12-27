module Day22 where

import Data.Function ((&))
import Data.List.Split
import Data.List as L
import Data.Array as A
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust, fromJust)
import Text.ParserCombinators.ReadP as P
import qualified Data.Heap as Heap
import Debug.Trace (trace)

import Lib

{-
--- Day 22: Wizard Simulator 20XX ---

Little Henry Case decides that defeating bosses with swords and stuff is boring. Now he's playing the game with a wizard. Of course, he gets stuck on another boss and needs your help again.

In this version, combat still proceeds with the player and the boss taking alternating turns. The player still goes first. Now, however, you don't get any equipment; instead, you must choose one of your spells to cast. The first character at or below 0 hit points loses.

Since you're a wizard, you don't get to wear armor, and you can't attack normally. However, since you do magic damage, your opponent's armor is ignored, and so the boss effectively has zero armor as well. As before, if armor (from a spell, in this case) would reduce damage below 1, it becomes 1 instead - that is, the boss' attacks always deal at least 1 damage.

On each of your turns, you must select one of your spells to cast. If you cannot afford to cast any spell, you lose. Spells cost mana; you start with 500 mana, but have no maximum limit. You must have enough mana to cast a spell, and its cost is immediately deducted when you cast it. Your spells are Magic Missile, Drain, Shield, Poison, and Recharge.

    Magic Missile costs 53 mana. It instantly does 4 damage.
    Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit points.
    Shield costs 113 mana. It starts an effect that lasts for 6 turns. While it is active, your armor is increased by 7.
    Poison costs 173 mana. It starts an effect that lasts for 6 turns. At the start of each turn while it is active, it deals the boss 3 damage.
    Recharge costs 229 mana. It starts an effect that lasts for 5 turns. At the start of each turn while it is active, it gives you 101 new mana.

Effects all work the same way. Effects apply at the start of both the player's turns and the boss' turns. Effects are created with a timer (the number of turns they last); at the start of each turn, after they apply any effect they have, their timer is decreased by one. If this decreases the timer to zero, the effect ends. You cannot cast a spell that would start an effect which is already active. However, effects can be started on the same turn they end.

For example, suppose the player has 10 hit points and 250 mana, and that the boss has 13 hit points and 8 damage:

-- Player turn --
- Player has 10 hit points, 0 armor, 250 mana
- Boss has 13 hit points
Player casts Poison.

-- Boss turn --
- Player has 10 hit points, 0 armor, 77 mana
- Boss has 13 hit points
Poison deals 3 damage; its timer is now 5.
Boss attacks for 8 damage.

-- Player turn --
- Player has 2 hit points, 0 armor, 77 mana
- Boss has 10 hit points
Poison deals 3 damage; its timer is now 4.
Player casts Magic Missile, dealing 4 damage.

-- Boss turn --
- Player has 2 hit points, 0 armor, 24 mana
- Boss has 3 hit points
Poison deals 3 damage. This kills the boss, and the player wins.

Now, suppose the same initial conditions, except that the boss has 14 hit points instead:

-- Player turn --
- Player has 10 hit points, 0 armor, 250 mana
- Boss has 14 hit points
Player casts Recharge.

-- Boss turn --
- Player has 10 hit points, 0 armor, 21 mana
- Boss has 14 hit points
Recharge provides 101 mana; its timer is now 4.
Boss attacks for 8 damage!

-- Player turn --
- Player has 2 hit points, 0 armor, 122 mana
- Boss has 14 hit points
Recharge provides 101 mana; its timer is now 3.
Player casts Shield, increasing armor by 7.

-- Boss turn --
- Player has 2 hit points, 7 armor, 110 mana
- Boss has 14 hit points
Shield's timer is now 5.
Recharge provides 101 mana; its timer is now 2.
Boss attacks for 8 - 7 = 1 damage!

-- Player turn --
- Player has 1 hit point, 7 armor, 211 mana
- Boss has 14 hit points
Shield's timer is now 4.
Recharge provides 101 mana; its timer is now 1.
Player casts Drain, dealing 2 damage, and healing 2 hit points.

-- Boss turn --
- Player has 3 hit points, 7 armor, 239 mana
- Boss has 12 hit points
Shield's timer is now 3.
Recharge provides 101 mana; its timer is now 0.
Recharge wears off.
Boss attacks for 8 - 7 = 1 damage!

-- Player turn --
- Player has 2 hit points, 7 armor, 340 mana
- Boss has 12 hit points
Shield's timer is now 2.
Player casts Poison.

-- Boss turn --
- Player has 2 hit points, 7 armor, 167 mana
- Boss has 12 hit points
Shield's timer is now 1.
Poison deals 3 damage; its timer is now 5.
Boss attacks for 8 - 7 = 1 damage!

-- Player turn --
- Player has 1 hit point, 7 armor, 167 mana
- Boss has 9 hit points
Shield's timer is now 0.
Shield wears off, decreasing armor by 7.
Poison deals 3 damage; its timer is now 4.
Player casts Magic Missile, dealing 4 damage.

-- Boss turn --
- Player has 1 hit point, 0 armor, 114 mana
- Boss has 2 hit points
Poison deals 3 damage. This kills the boss, and the player wins.

You start with 50 hit points and 500 mana points. The boss's actual stats are in your puzzle input. What is the least amount of mana you can spend and still win the fight? (Do not include mana recharge effects as "spending" negative mana.)

To begin, get your puzzle input.
-}

data Player = P { php :: Integer
                , mana :: Integer
                , armour :: Integer
                }
  deriving (Show, Eq)

data Enemy = E { ehp :: Integer
               , damage :: Integer
               }
  deriving (Show, Eq)

startingPlayer = P { php=50, mana=500, armour=0 }


parse ls =
  let [hp, d] = ls
              & map (splitOn ": ")
              & map (!! 1)
              & map read
  in E { ehp=hp, damage=d }


data Effect = Effect { name :: String
                     , cost :: Integer
                     , immediate :: (Player, Enemy) -> (Player, Enemy)
                     , tick :: (Player, Enemy) -> (Player, Enemy, Maybe Effect)
                     }

instance Show Effect where
  show = name

instance Eq Effect where
  a == b = (name a) == (name b)

instance Ord Effect where
  a `compare` b = (name a) `compare` (name b)

-- Magic Missile costs 53 mana. It instantly does 4 damage.
missile = Effect { name = "missile"
                 , cost = 53
                 , immediate = \(p,e) -> (p, e { ehp=ehp e - 4 })
                 , tick = \(p,e) -> (p, e, Nothing)
                 }

-- Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit points.
drain = Effect { name = "drain"
               , cost = 73
               , immediate = \(p,e) -> (p { php=php p + 2 }, e { ehp=ehp e - 2 })
               , tick = \(p,e) -> (p, e, Nothing)
               }

-- Shield costs 113 mana. It starts an effect that lasts for 6 turns. While it is active, your armor is increased by 7.
shield = shield0 5
shield0 n = Effect { name = "shield"
                   , cost = 113
                   , immediate = \(p, e) -> (p { armour = armour p + 7 }, e)
                   , tick = \(p, e) -> if n == 0 then (p { armour = armour p - 7 }, e, Nothing)
                                       else (p, e, Just $ shield0 (n-1))
                   }

-- Poison costs 173 mana. It starts an effect that lasts for 6 turns. At the start of each turn while it is active, it deals the boss 3 damage.
poison = poison0 5
poison0 n = Effect { name = "poison"
                   , cost = 173
                   , immediate = id
                   , tick = \(p, e) -> let e' = e { ehp = ehp e - 3 }
                                       in if n == 0 then (p, e', Nothing)
                                          else (p, e', Just $ poison0 (n - 1))
                   }

-- Recharge costs 229 mana. It starts an effect that lasts for 5 turns. At the start of each turn while it is active, it gives you 101 new mana.
recharge = recharge0 4
recharge0 n = Effect { name = "recharge"
                     , cost = 229
                     , immediate = id
                     , tick = \(p, e) -> let p' = p { mana = mana p + 101 }
                                         in if n == 0 then (p', e, Nothing)
                                         else (p', e, Just $ recharge0 (n-1))
                     }

vanquished enemy = ehp enemy <= 0

defeated player = php player <= 0

data State = State { isPlayerTurn :: Bool
                   , player :: Player
                   , enemy :: Enemy
                   , effects :: Set.Set Effect
                   , fired :: [Effect]
                   }
  deriving (Show)


fight :: (Player -> Player) -> Heap.MinPrioHeap Integer State -> Maybe (Integer, State)
fight playerTurnEffect queue =
  -- empty queue? if so, we have no path to victory
  if Heap.null queue then Nothing
  else
  let Just ((spent, state), queue') = Heap.view queue in
  -- has the player won? If so, return now
--  trace (show (spent, state)) $
  if vanquished (enemy state) then Just (spent, state)
  else
  if isPlayerTurn state then
    -- Player's turn:
    -- fire the effects and update the effect map
    let p0 = playerTurnEffect (player state) in
    if defeated p0 then fight playerTurnEffect queue'
    else
    let (p', e', effs') = fireEffects (p0, enemy state, effects state) in
    -- has the player won? If so, return now
    if vanquished e' then Just (spent, state { player=p', enemy = e', effects=effs' })
    else
    -- can the player take a turn? If not, this is a loss
    let candidates = Set.fromList [missile, drain, shield, poison, recharge]
                     `Set.difference` effs'
                   & Set.filter (\eff -> cost eff <= mana p')
    in
--    trace ("candidates: " ++ show candidates) $
    if Set.null candidates then fight playerTurnEffect queue'
    else
    -- for the potential player turns:
    fight playerTurnEffect $ Set.foldl
      (\q eff ->
          --   work out the immediate effect and insert at that cost
--          trace ("  considering: " ++ show eff) $
          let (p'', e'') = (immediate eff) (p', e')
          --   add the effect to the effect map
              effs'' = Set.insert eff effs'
              state'' = state { isPlayerTurn = False
                              , player = p'' { mana = mana p'' - cost eff }
                              , enemy = e''
                              , effects = effs''
                              , fired = fired state ++ [eff]
                              }
          in Heap.insert (spent + cost eff, state'') q)
      queue' candidates
  else   -- Enemy's turn:
    -- fire the effects and update the effect map
    let (p', e', effs') = fireEffects (player state, enemy state, effects state) in
    -- has the player won? If so, return now
    if vanquished e' then Just (spent, state { player=p', enemy=e', effects=effs' })
    else
    -- take the monster's turn
    let p'' = p' { php = php p' - (max 1 (damage e' - armour p')) }
    in
    -- if the enemy has won, this is a loss, otherwise insert the next state at the updated cost
      if defeated p'' then
      fight playerTurnEffect $ queue' 
      else
      fight playerTurnEffect $ Heap.insert (spent, state { isPlayerTurn=True, player=p'', enemy=e', effects=effs' }) queue'

fireEffects :: (Player, Enemy, Set.Set Effect) -> (Player, Enemy, Set.Set Effect)
fireEffects (player, enemy, effects) =
  Set.foldl
    (\(p, e, effs) eff ->
--      trace ("  ticking " ++ (show eff)) $
      let (p', e', spentEffect) = (tick eff) (p, e)
      in case spentEffect of
           Nothing -> {- trace ("  ..finished, p->" ++ show p') $ -}     (p', e', Set.delete eff effs)
           Just eff' -> {- trace ("  ..continues, p->" ++ show p') $ -}  (p', e', Set.insert eff' effs)  -- the new value replaces the older one
    )
    (player, enemy, effects)
    effects



day22 ls =
  let enemy = parse ls
      state = State { isPlayerTurn = True
                    , player = startingPlayer
                    , enemy = enemy
                    , effects = Set.empty
                    , fired = []
                    }
      q = Heap.singleton (0, state)  :: Heap.MinPrioHeap Integer State
  in  fight id q

{-
--- Part Two ---

On the next run through the game, you increase the difficulty to hard.

At the start of each player turn (before any other effects apply), you lose 1 hit point. If this brings you to or below 0 hit points, you lose.

With the same starting stats for you and the boss, what is the least amount of mana you can spend and still win the fight?
-}

day22b ls =
  let enemy = parse ls
      state = State { isPlayerTurn = True
                    , player = startingPlayer
                    , enemy = enemy
                    , effects = Set.empty
                    , fired = []
                    }
      q = Heap.singleton (0, state)  :: Heap.MinPrioHeap Integer State
  in  fight (\p -> p { php = php p - 1 }) q