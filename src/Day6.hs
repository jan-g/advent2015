module Day6 where

import Lib

import Data.Ix
import Data.Function ((&))
import Data.List.Split
{- import qualified Data.Array as A -}
import qualified Data.Array.Unboxed as A
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char

import Text.ParserCombinators.ReadP
import Data.Maybe (catMaybes)

{-
--- Day 6: Probably a Fire Hazard ---

Because your neighbors keep defeating you in the holiday house decorating contest year after year, you've decided to deploy one million lights in a 1000x1000 grid.

Furthermore, because you've been especially nice this year, Santa has mailed you instructions on how to display the ideal lighting configuration.

Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include whether to turn on, turn off, or toggle various inclusive ranges given as coordinate pairs. Each coordinate pair represents opposite corners of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to 9 lights in a 3x3 square. The lights all start turned off.

To defeat your neighbors this year, all you have to do is set up your lights by doing the instructions Santa sent you in order.

For example:

    turn on 0,0 through 999,999 would turn on (or leave on) every light.
    toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off.
    turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.

After following the instructions, how many lights are lit?
-}

type Array = A.UArray

parse ls = ls & map parseI & catMaybes

type Coord = (Int, Int)
data Instruction = TurnOn Coord Coord
                 | TurnOff Coord Coord
                 | Toggle Coord Coord
  deriving (Show, Eq)

parseI :: String -> Maybe Instruction
parseI s = case readP_to_S parseInstruction s of
             [] -> Nothing
             [(e, "")] -> Just e

parseCoord :: ReadP Coord
parseCoord = do
  x <- intParser
  char ','
  y <- intParser
  return (fromInteger x, fromInteger y)

parseInstruction :: ReadP Instruction
parseInstruction = choice [
  (do
    string "turn on "
    tl <- parseCoord
    string " through "
    br <- parseCoord
    return $ TurnOn tl br),
  (do
    string "turn off "
    tl <- parseCoord
    string " through "
    br <- parseCoord
    return $ TurnOff tl br),
  (do
    string "toggle "
    tl <- parseCoord
    string " through "
    br <- parseCoord
    return $ Toggle tl br)
  ]


{- mkArray                 :: (Ix a) => (a -> b) -> (a,a) -> Array a b -}
mkArray f bnds          =  A.array bnds [(i, f i) | i <- range bnds]

mapArray f bnds a = a A.// [(i, f (a A.! i)) | i <- range bnds]

day6 ls =
 let inst = parse ls
     blank = mkArray (const False) ((0,0), (999, 999))
     result = foldl applyInstruction blank inst
 in  A.elems result & filter id & length  

applyInstruction :: Array (Int, Int) Bool -> Instruction -> Array (Int, Int) Bool
applyInstruction a (TurnOn (x0,y0) (x1,y1)) = mapArray (const True) ((x0,y0),(x1,y1)) a
applyInstruction a (TurnOff (x0,y0) (x1,y1)) = mapArray (const False) ((x0,y0),(x1,y1)) a
applyInstruction a (Toggle (x0,y0) (x1,y1)) = mapArray not ((x0,y0),(x1,y1)) a

{-
You just finish implementing your winning light pattern when you realize you mistranslated Santa's message from Ancient Nordic Elvish.

The light grid you bought actually has individual brightness controls; each light can have a brightness of zero or more. The lights all start at zero.

The phrase turn on actually means that you should increase the brightness of those lights by 1.

The phrase turn off actually means that you should decrease the brightness of those lights by 1, to a minimum of zero.

The phrase toggle actually means that you should increase the brightness of those lights by 2.

What is the total brightness of all lights combined after following Santa's instructions?

For example:

    turn on 0,0 through 0,0 would increase the total brightness by 1.
    toggle 0,0 through 999,999 would increase the total brightness by 2000000.
-}

day6b :: [String] -> Int
day6b ls =
 let inst = parse ls
     blank = mkArray (const 0) ((0,0), (999, 999)) :: A.Array (Int, Int) Int
     result = foldl applyInstruction2 blank inst
 in  A.elems result & sum

applyInstruction2 :: A.Array (Int, Int) Int -> Instruction -> A.Array (Int, Int) Int
applyInstruction2 a (TurnOn (x0,y0) (x1,y1)) = mapArray (+ 1) ((x0,y0),(x1,y1)) a
applyInstruction2 a (TurnOff (x0,y0) (x1,y1)) = mapArray (\x -> (x - 1) `max` 0) ((x0,y0),(x1,y1)) a
applyInstruction2 a (Toggle (x0,y0) (x1,y1)) = mapArray (+ 2) ((x0,y0),(x1,y1)) a
