module Day4 where

import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Base16 as B16
import qualified Crypto.Hash.MD5 as MD5

{-
--- Day 4: The Ideal Stocking Stuffer ---

Santa needs help mining some AdventCoins (very similar to bitcoins) to use as gifts for all the economically
 forward-thinking little girls and boys.

To do this, he needs to find MD5 hashes which, in hexadecimal, start with at least five zeroes. The input to the
 MD5 hash is some secret key (your puzzle input, given below) followed by a number in decimal. To mine AdventCoins,
  you must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...) that produces such a hash.

For example:

    If your secret key is abcdef, the answer is 609043, because the MD5 hash of abcdef609043 starts with five zeroes
     (000001dbbfa...), and it is the lowest such number to do so.
    If your secret key is pqrstuv, the lowest number it combines with to make an MD5 hash starting with five zeroes is
     1048970; that is, the MD5 hash of pqrstuv1048970 looks like 000006136ef....
-}

parse ls = ls
         & head
         & splitOn "-"
         & map read

hash0 prefix =
  prefix & BSU.fromString & MD5.update MD5.init
  
hash ctx n =
  let bs = show n & BSU.fromString
  in  MD5.update ctx bs & MD5.finalize & B16.encode

day4 :: [String] -> Int
day4 ls =
 let prefix = ls & head
     ctx = hash0 prefix
     search = BSU.fromString "00000"
     zeroes = takeWhile (\x -> hash ctx x
                             & B.take (B.length search)
                             & (/= search)) [0..]
 in  length zeroes

{-
Now find one that starts with six zeroes.
-}

day4b ls =
 let prefix = ls & head
     ctx = hash0 prefix
     search = BSU.fromString "000000"
     zeroes = takeWhile (\x -> hash ctx x
                             & B.take (B.length search)
                             & (/= search)) [0..]
 in  length zeroes
