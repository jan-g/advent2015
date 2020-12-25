module Day11 where

import Data.Function ((&))
import Data.List (nub)
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char

{-
--- Day 11: Corporate Policy ---

Santa's previous password expired, and he needs help choosing a new one.

To help him remember his new password after the old one expires, Santa has devised a method of coming up with a password
based on the previous one. Corporate policy dictates that passwords must be exactly eight lowercase letters (for
security reasons), so he finds his new password by incrementing his old password string repeatedly until it is valid.

Incrementing is just like counting with numbers: xx, xy, xz, ya, yb, and so on. Increase the rightmost letter one step;
if it was z, it wraps around to a, and repeat with the next letter to the left until one doesn't wrap around.

Unfortunately for Santa, a new Security-Elf recently started, and he has imposed some additional password requirements:

    Passwords must include one increasing straight of at least three letters, like abc, bcd, cde, and so on, up to xyz.
      They cannot skip letters; abd doesn't count.
    Passwords may not contain the letters i, o, or l, as these letters can be mistaken for other characters and are
      therefore confusing.
    Passwords must contain at least two different, non-overlapping pairs of letters, like aa, bb, or zz.

For example:

    hijklmmn meets the first requirement (because it contains the straight hij) but fails the second requirement
      requirement (because it contains i and l).
    abbceffg meets the third requirement (because it repeats bb and ff) but fails the first requirement.
    abbcegjk fails the third requirement, because it only has one double letter (bb).
    The next password after abcdefgh is abcdffaa.
    The next password after ghijklmn is ghjaabcc, because you eventually skip all the passwords that start with
      ghi..., since i is not allowed.

Given Santa's current password (your puzzle input), what should his next password be?

Your puzzle input is vzbxkghb.
-}

day11 ls =
  let pwd = ls & head
  in  nextPass pwd

nextPass pwd = iterate bump pwd & dropWhile (\t -> not (straight t) || not (doubles t)) & head

inc 'z' = ('a', True)
inc c =
  let c' = chr (ord c + 1)
  in  case c' of
        'i' -> ('j', False)
        'o' -> ('p', False)
        'l' -> ('m', False)
        otherwise -> (c', False)

bump xs =
  let xs' = reverse xs
      xs'' = bump' xs' True
  in reverse xs''
  where
    bump' xs False = xs
    bump' "" True = "a"
    bump' (c:cs) True =
      let (c', carry) = inc c
      in  c' : bump' cs carry

straight (a:b:c:xs) =
  let a' = ord a
      b' = ord b
      c' = ord c
  in  a' + 1 == b' && b' + 1 == c' || straight (b:c:xs)
straight _ = False

doubles xs =
  let d = doubles' [] xs & nub
  in  length d > 1
  where
    doubles' l "" = l
    doubles' l (a:b:xs) =
      if a == b then doubles' (a:l) xs
      else doubles' l (b:xs)
    doubles' l _ = l 

{-
--- Part Two ---

Santa's password expired again. What's the next one?
-}

day11b ls =
  let pwd = ls & head
  in  nextPass pwd & bump & nextPass
