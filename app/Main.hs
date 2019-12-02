module Main where

import Day1
import System.IO


day1_fuel :: String -> [Fuel]
day1_fuel = fmap (calculateFuel . read) . lines

main :: IO ()
main = do
    r <- readFile "./app/input_day1"
    putStr "[Day 1-1] Fuel needed: "
    let fs = day1_fuel r
    print $ sum fs
    putStr "[Day 1-2] Fuel needed: "
    print (sum fs + (sum . fmap calculateFuelOfFuel) fs)
