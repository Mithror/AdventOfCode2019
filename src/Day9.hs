module Day9
    (
        module Day9
    ) where

import IntCode

day9_result1 :: String -> Integer
day9_result1 = head
             . snd
             . getOutputs
             . flip runProgram (Just 1)
             . parseProgram

day9_result2 :: String -> Integer
day9_result2 = head
             . snd
             . getOutputs
             . flip runProgram (Just 2)
             . parseProgram
             