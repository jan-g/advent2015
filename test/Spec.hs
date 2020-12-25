import Test.Hspec
import Control.Exception (evaluate)

import Data.Array
import Data.Function ((&))
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP (readP_to_S)

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25


main :: IO ()
main =
  hspec $ do
    describe "Day1" $ do
      it "works" $ do
        Day1.day1b [")"] `shouldBe` 1
        Day1.day1b ["()())"] `shouldBe` 5


    describe "Day2" $ do
      it "works" $ do
        Day2.needed [2,3,4] `shouldBe` 58
        Day2.needed [1,1,10] `shouldBe` 43

        Day2.ribbon [2,3,4] `shouldBe` 34
        Day2.ribbon [1,1,10] `shouldBe` 14

    describe "day 3" $ do
      it "works" $ do
        Day3.day3 [">"] `shouldBe` 2
        Day3.day3 ["^>v<"] `shouldBe` 4
        Day3.day3 ["^v^v^v^v^v"] `shouldBe` 2

        Day3.day3b ["^v"] `shouldBe` 3
        Day3.day3b ["^>v<"] `shouldBe` 3
        Day3.day3b ["^v^v^v^v^v"] `shouldBe` 11

    describe "Day 4" $ do
      it "has a working hash" $ do
        (Day4.hash (Day4.hash0 "abcdef") 609043 & B.take 5) `shouldBe` BSU.fromString "00000"

      it "ex1" $ do
        Day4.day4 ["abcdef"] `shouldBe` 609043

      it "ex2" $ do
        Day4.day4 ["pqrstuv"] `shouldBe` 1048970

    describe "Day 5" $ do
      it "works" $ do
        Day5.nice "ugknbfddgicrmopn" `shouldBe` True
        Day5.nice "aaa" `shouldBe` True
        Day5.nice "jchzalrnumimnmhp" `shouldBe` False
        Day5.nice "haegwjzuvuyypxyu" `shouldBe` False
        Day5.nice "dvszwmarrgswjxmb" `shouldBe` False

      it "works part b" $ do
        Day5.nice2 "qjhvhtzxzqqjkmpb" `shouldBe` True
        Day5.nice2 "xxyxx" `shouldBe` True

        Day5.nice2 "uurcxstgmygtbstg" `shouldBe` False
        Day5.nice2 "ieodomkazucvgmuy" `shouldBe` False


    describe "day 6" $ do
      it "turns on everything" $ do
        Day6.day6 ["turn on 0,0 through 999,999"] `shouldBe` 1000000
        Day6.day6 ["toggle 0,0 through 999,0"] `shouldBe` 1000
        Day6.day6 ["turn off 499,499 through 500,500"] `shouldBe` 0
        Day6.day6 ["turn on 0,0 through 999,999"
                  ,"turn off 499,499 through 500,500"] `shouldBe` 999996

      it "does part b" $ do
        Day6.day6b ["turn on 0,0 through 0,0"] `shouldBe` 1
        Day6.day6b ["toggle 0,0 through 999,999"] `shouldBe` 2000000


    describe "Day 7" $ do
      it "runes the example" $ do
        let input = ["123 -> x"
                    ,"456 -> y"
                    ,"x AND y -> d"
                    ,"x OR y -> e"
                    ,"x LSHIFT 2 -> f"
                    ,"y RSHIFT 2 -> g"
                    ,"NOT x -> h"
                    ,"NOT y -> i"
                    ]
        Day7.run input `shouldBe` Map.fromList [("d", 72),("e", 507),("f", 492),("g", 114),
                                                 ("h", 65412),("i", 65079),("x", 123),("y", 456)]

    describe "Day8" $ do
      it "works out lengths" $ do
        Day8.day8 ["\"\""] `shouldBe` (2, 0, 2)
        Day8.day8 ["\"abc\""] `shouldBe` (5, 3, 2)
        Day8.dequote "\"\\\\\"" `shouldBe` "\\"
        Day8.dequote "\"\\\"\"" `shouldBe` "\""
        Day8.dequote "\"\\x30\"" `shouldBe` "0"

        Day8.dequote "\"aaa\\\"aaa\"" `shouldBe` "aaa\"aaa"
        Day8.day8 ["\"aaa\\\"aaa\""] `shouldBe` (10, 7, 3)
        Day8.day8 ["\"\\x27\""] `shouldBe` (6, 1, 5)

    describe "Day8b" $ do
      it "requotes" $ do
        Day8.requote "" `shouldBe` "\"\""
        Day8.requote "\"\"" `shouldBe` "\"\\\"\\\"\""
        Day8.requote "\"abc\"" `shouldBe` "\"\\\"abc\\\"\""
        Day8.requote "\"aaa\\\"aaa\"" `shouldBe` "\"\\\"aaa\\\\\\\"aaa\\\"\""
        Day8.requote "\"\\x27\"" `shouldBe` "\"\\\"\\\\x27\\\"\""

        Day8.day8b ["\"\""] `shouldBe` (2, 6, 4)

    describe "Day9" $ do
      it "works out routes" $ do
        let input = ["London to Dublin = 464"
                    ,"London to Belfast = 518"
                    ,"Dublin to Belfast = 141"
                    ]

            result = Day9.day9 input
            (m, np, ans, sc) = result

        np `shouldBe` 6
        ans `shouldBe` 605
        length sc `shouldBe` 6

    describe "Day11" $ do
          it "increments counts" $ do
            Day11.bump "aa" `shouldBe` "ab"
            Day11.bump "bh" `shouldBe` "bj"
            Day11.bump "zzzz" `shouldBe` "aaaaa"

          it "detects straights" $ do
            Day11.straight "abc" `shouldBe` True
            Day11.straight "aabcd" `shouldBe` True
            Day11.straight "abd" `shouldBe` False
            Day11.straight "abdcb" `shouldBe` False

          it "detects doubles" $ do
            Day11.doubles "aabb" `shouldBe` True
            Day11.doubles "aaaa" `shouldBe` False
            Day11.doubles "abba" `shouldBe` False
            Day11.doubles "abccdeffg" `shouldBe` True

    describe "Day12" $ do
          it "sums strings" $ do
            readP_to_S Day12.stringSummer "\"\"" `shouldBe` [(0, "")]
            readP_to_S Day12.stringSummer "  \"abahsdbs\"" `shouldBe` [(0, "")]

          it "sums numbers" $ do
            readP_to_S Day12.numberSummer "123" `shouldBe` [(123, "")]
            readP_to_S Day12.numberSummer "-123" `shouldBe` [(-123, "")]

          it "sums arrays" $ do
            readP_to_S Day12.arraySummer "[]" `shouldBe` [(0, "")]
            readP_to_S Day12.arraySummer "[\"\", 123, 321 ]" `shouldBe` [(444, "")]

          it "sums objects" $ do
            readP_to_S Day12.objectSummer "{}" `shouldBe` [(0, "")]
            readP_to_S Day12.objectSummer "{\"\" : \"f\", \"a\" : 123, \"b\":321 }" `shouldBe` [(444, "")]

    describe "Day12b" $ do
          it "sums arrays" $ do
            readP_to_S Day12.arraySummer2 "[]" `shouldBe` [(0, "")]
            readP_to_S Day12.arraySummer2 "[{\"a\":\"red\", \"b\": 3}, 444]" `shouldBe` [(444, "")]

          it "sums objects" $ do
            readP_to_S Day12.objectSummer2 "{}" `shouldBe` [(0, "")]
            readP_to_S Day12.objectSummer2 "{\"\" : \"f\", \"a\" : 123, \"foo\":\"red\" }" `shouldBe` [(0, "")]
            readP_to_S Day12.objectSummer2 "{\"\" : \"f\", \"a\" : 123, \"foo\":\"raed\" }" `shouldBe` [(123, "")]

{-
    describe "Day14" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day14b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day15" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day15b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day16" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day16b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day17" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day17b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day18" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day18b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day19" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day19b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day20" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day20b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day21" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day21b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day22" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day22b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day23" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day23b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
    describe "Day24" $ do it "works" $ do Day6.day6 [] `shouldBe` "hello world"
    describe "Day24b" $ do it "works" $ do Day6.day6b [] `shouldBe` "hello world"
-}

    describe "day 16" $ do
      it "Should match the sought item" $ do
        Day16.matches Day16.seeking Day16.seeking `shouldBe` True