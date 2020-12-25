module Day7 where

import Lib

import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Char
import Text.ParserCombinators.ReadP
import Data.Maybe (catMaybes)
import Data.Bits (xor, (.&.), (.|.), shift)

{-
--- Day 7: Some Assembly Required ---

This year, Santa brought little Bobby Tables a set of wires and bitwise logic gates! Unfortunately, little Bobby is a little under the recommended age range, and he needs help assembling the circuit.

Each wire has an identifier (some lowercase letters) and can carry a 16-bit signal (a number from 0 to 65535). A signal is provided to each wire by a gate, another wire, or some specific value. Each wire can only get a signal from one source, but can provide its signal to multiple destinations. A gate provides no signal until all of its inputs have a signal.

The included instructions booklet describes how to connect the parts together: x AND y -> z means to connect wires x and y to an AND gate, and then connect its output to wire z.

For example:

    123 -> x means that the signal 123 is provided to wire x.
    x AND y -> z means that the bitwise AND of wire x and wire y is provided to wire z.
    p LSHIFT 2 -> q means that the value from wire p is left-shifted by 2 and then provided to wire q.
    NOT e -> f means that the bitwise complement of the value from wire e is provided to wire f.

Other possible gates include OR (bitwise OR) and RSHIFT (right-shift). If, for some reason, you'd like to emulate the circuit instead, almost all programming languages (for example, C, JavaScript, or Python) provide operators for these gates.

For example, here is a simple circuit:

123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i

After it is run, these are the signals on the wires:

d: 72
e: 507
f: 492
g: 114
h: 65412
i: 65079
x: 123
y: 456

In little Bobby's kit's instructions booklet (provided as your puzzle input), what signal is ultimately provided
 to wire a?
-}

parse ls = ls & map parseI & catMaybes

data Input = Value Int | Wire String deriving (Show, Eq)
data Op = Copy Input String
        | And Input Input String
        | LShift Input Input String
        | Not Input String
        | Or Input Input String
        | RShift Input Input String
  deriving (Show, Eq)

type Ctx = M.Map String Int

sim :: Ctx -> Op -> Ctx
sim ctx (Copy i s) = M.insert s (val ctx i) ctx
sim ctx (Not i s) = M.insert s ((val ctx i) `xor` 0xffff) ctx
sim ctx (And i1 i2 s) = M.insert s ((val ctx i1) .&. (val ctx i2)) ctx
sim ctx (Or i1 i2 s) = M.insert s ((val ctx i1) .|. (val ctx i2)) ctx
sim ctx (LShift i1 i2 s) = M.insert s (((val ctx i1) `shift` (val ctx i2)) .&. 0xffff) ctx
sim ctx (RShift i1 i2 s) = M.insert s (((val ctx i1) `shift` (-(val ctx i2))) .&. 0xffff) ctx

val :: Ctx -> Input -> Int
val ctx (Value v) = v
val ctx (Wire i) = ctx M.! i

parseI :: String -> Maybe Op
parseI s = case readP_to_S (do x <- parseOp; eof; return x) s of
             [] -> Nothing
             [(e, "")] -> Just e
             x -> error (show x)

parseOp :: ReadP Op
parseOp = choice [
  (do
    i <- parseInput
    string " -> "
    o <- parseOutput
    return $ Copy i o),
  (do
    string "NOT "
    i <- parseInput
    string " -> "
    o <- parseOutput
    return $ Not i o),
  (do
    i1 <- parseInput
    string " AND "
    i2 <- parseInput
    string " -> "
    o <- parseOutput
    return $ And i1 i2 o),
  (do
    i1 <- parseInput
    string " OR "
    i2 <- parseInput
    string " -> "
    o <- parseOutput
    return $ Or i1 i2 o),
  (do
    i1 <- parseInput
    string " LSHIFT "
    i2 <- parseInput
    string " -> "
    o <- parseOutput
    return $ LShift i1 i2 o),
  (do
    i1 <- parseInput
    string " RSHIFT "
    i2 <- parseInput
    string " -> "
    o <- parseOutput
    return $ RShift i1 i2 o)
  ]

parseOutput :: ReadP String
parseOutput = many1 (satisfy isAlpha)

parseInput :: ReadP Input
parseInput = choice [
  (do
    x <- intParser
    return $ Value $ fromInteger x),
  (do
    x <- parseOutput
    return $ Wire x)
  ]


day7 ls = (run ls) M.! "a"

run ls =
  let wires = parse ls
      {- Each input leads to potentially several outputs -}
      leadsTo = foldl (\m op ->
                        let is = inputs op  :: S.Set String
                            o = output op   :: String
                        in  foldl (\m i -> M.insertWith S.union i (S.singleton o) m) m is) M.empty wires :: M.Map String (S.Set String)
      {- Each output comes from one expression -}
      gatesOutputting = foldl (\m op -> M.insert (output op) op m) M.empty wires
      {- Each output has a number of things it's waiting on -}
      needs = foldl (\m op ->
                      let is = inputs op
                          o = output op
                      in M.insertWith (+) o (S.size is) m) M.empty wires
      {- Things which can be fired are those things with 0 waiting inputs -}
      ready = M.filter (== 0) needs & M.keysSet
      ctx = runUntilDone M.empty gatesOutputting leadsTo needs ready
  in ctx {- (ctx, gatesOutputting, leadsTo, needs, ready) -}

runUntilDone :: M.Map String Int              {- the context - values assigned thus far -}
             -> M.Map String Op               {- where a particular value comes from -}
             -> M.Map String (S.Set String)   {- an output is connected to these inputs -}
             -> M.Map String Int              {- an output is waiting on this many inputs -}
             -> S.Set String                  {- these outputs are ready to compute -}
             -> M.Map String Int              {- the result is an updated context -}
runUntilDone ctx gatesOutputting leadsTo needs ready
  | S.null ready = ctx
  | otherwise = let wire = S.elemAt 0 ready
                    ready' = S.deleteAt 0 ready
                    ctx' = sim ctx (gatesOutputting M.! wire)
                    newInput = M.findWithDefault S.empty wire leadsTo {- these have a new input -}
                    needs' = foldr (\i n -> M.adjust pred i n) needs newInput
                    ready'' = foldr (\i r -> if needs' M.! i == 0 then S.insert i r else r) ready' newInput
                in  runUntilDone ctx' gatesOutputting leadsTo needs' ready''

input :: Input -> S.Set String
input (Value _) = S.empty
input (Wire w) = S.singleton w

inputs :: Op -> S.Set String
inputs (Copy i _) = input i
inputs (Not i _) = input i
inputs (And i1 i2 _) = (input i1) `S.union` (input i2)
inputs (Or i1 i2 _) = (input i1) `S.union` (input i2)
inputs (LShift i1 i2 _) = (input i1) `S.union` (input i2)
inputs (RShift i1 i2 _) = (input i1) `S.union` (input i2)

output :: Op -> String
output (Copy _ o) = o
output (Not _ o) = o
output (And _ _ o) = o
output (Or _ _ o) = o
output (LShift _ _ o) = o
output (RShift _ _ o) = o

{-
-}

day7b ls =
  let origA = (run ls) M.! "a"
      ls' = ls ++ [(show origA) ++ " -> b"]
  in  (run ls') M.! "a"