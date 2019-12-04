module Main where

import Day1
import Day2
import Day3
import Day4
import System.IO
import Data.List.Split
import Data.Set
import Text.ParserCombinators.ReadP


day1_fuel :: String -> [Fuel]
day1_fuel = fmap (calculateFuel . read) . lines

day2_instructions :: String -> [Integer]
day2_instructions = fmap read . splitOn ","

day2 :: (Integer, Integer) -> [Integer] -> Integer
day2 t = head . execute t

day2_look :: [Integer] -> Integer
day2_look xs = head $ [ 100*noun + verb |
                        noun <- [0..99],
                        verb <- [0..99],
                        day2 (noun, verb) xs == 19690720 ]

main :: IO ()
main = do
    r_day1 <- readFile "./app/input_day1"
    putStr "[Day 1-1] Fuel needed: "
    let fs = day1_fuel r_day1
    print $ sum fs
    putStr "[Day 1-2] Fuel needed: "
    print (sum fs + (sum . fmap calculateFuelOfFuel) fs)
    r_day2 <- readFile "./app/input_day2"
    let instructions = day2_instructions r_day2
    putStr "[Day 2-1] Result: "
    print $ day2 (12,2) instructions
    putStr "[Day 2-2] Result: "
    print $ day2_look instructions
    r_day3 <- readFile "./app/input_day3"
    putStr "[Day 3] Result: "
    print $ getDistanceAndSteps r_day3
    putStr "[Day 4-1] Result: "
    print getNumberOfPasswords
    putStr "[Day 4-2] Result: "
    print getNumberOfPasswords'

