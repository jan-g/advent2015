module Day23 where

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
import Data.Functor (($>))

import Lib

{-
--- Day 23: Opening the Turing Lock ---

Little Jane Marie just got her very first computer for Christmas from some unknown benefactor. It comes with instructions and an example program, but the computer itself seems to be malfunctioning. She's curious what the program does, and would like you to help her run it.

The manual explains that the computer supports two registers and six instructions (truly, it goes on to remind the reader, a state-of-the-art technology). The registers are named a and b, can hold any non-negative integer, and begin with a value of 0. The instructions are as follows:

    hlf r sets register r to half its current value, then continues with the next instruction.
    tpl r sets register r to triple its current value, then continues with the next instruction.
    inc r increments register r, adding 1 to it, then continues with the next instruction.
    jmp offset is a jump; it continues with the instruction offset away relative to itself.
    jie r, offset is like jmp, but only jumps if register r is even ("jump if even").
    jio r, offset is like jmp, but only jumps if register r is 1 ("jump if one", not odd).

All three jump instructions work with an offset relative to that instruction. The offset is always written with a prefix + or - to indicate the direction of the jump (forward or backward, respectively). For example, jmp +1 would simply continue with the next instruction, while jmp +0 would continuously jump back to itself forever.

The program exits when it tries to run an instruction beyond the ones defined.

For example, this program sets a to 2, because the jio instruction causes it to skip the tpl instruction:

inc a
jio a, +2
tpl a
inc a

What is the value in register b when the program in your puzzle input is finished executing?
-}

data Reg = A | B deriving (Show, Eq)

data Instr = Hlf Reg | Tpl Reg | Inc Reg | Jmp Integer | Jie Reg Integer | Jio Reg Integer
  deriving (Show, Eq)

data State = State { a :: Integer
                   , b :: Integer
                   , pc :: Integer
                   }
  deriving (Show, Eq)

type Program = Map.Map Integer Instr

fetch A (State { a=a }) = a
fetch B (State { b=b }) = b

store A s v = s { a = v }
store B s v = s { b = v }


step :: Program -> State -> Either State State
step prog (s@State { a=a, b=b, pc=pc }) =
  case Map.lookup pc prog of
    Nothing -> Left s
    Just (Hlf r) -> let s' = store r s (fetch r s `div` 2) in Right $ s' { pc=succ pc }
    Just (Tpl r) -> let s' = store r s (fetch r s * 3) in Right $ s' { pc=succ pc }
    Just (Inc r) -> let s' = store r s (fetch r s + 1) in Right $ s' { pc=succ pc }
    Just (Jmp d) -> Right $ s { pc=pc + d }
    Just (Jie r d) -> let pc' = if fetch r s `mod` 2 == 0 then pc + d else succ pc in Right $ s { pc=pc' }
    Just (Jio r d) -> let pc' = if fetch r s == 1 then pc + d else succ pc in Right $ s { pc=pc' }

run :: Program -> State -> State
run prog s =
  case step prog s of
    Left s' -> s'
    Right s' -> run prog s'
   

parse :: [String] -> Program
parse ls = ls
         & map (quickParse parseInstr)
         & catMaybes
         & zip [0..]
         & Map.fromList

parseInstr :: ReadP Instr
parseInstr =
  (string "hlf " *> parseReg <<!! Hlf <* eof) <++
  (string "tpl " *> parseReg <<!! Tpl <* eof) <++
  (string "inc " *> parseReg <<!! Inc <* eof) <++
  (string "jmp " *> plusOrMinusParser <<!! Jmp <* eof) <++
  (do 
    string "jie "
    r <- parseReg
    string ", "
    d <- plusOrMinusParser
    eof
    return $ Jie r d) <++
  (do 
    string "jio "
    r <- parseReg
    string ", "
    d <- plusOrMinusParser
    eof
    return $ Jio r d)

parseReg :: ReadP Reg
parseReg =
  (string "a" $> A) <++
  (string "b" $> B)
  

startState = State { a=0, b=0, pc=0 }

day23 ls =
  let prog = parse ls
  in  b $ run prog startState

{-
--- Part Two ---

The unknown benefactor is very thankful for releasi-- er, helping little Jane Marie with her computer. Definitely not to distract you, what is the value in register b after the program is finished executing if register a starts as 1 instead?
-}

day23b ls =
  let prog = parse ls
  in  b $ run prog (store A startState 1)
