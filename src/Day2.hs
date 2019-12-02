module Day2
    (
        execute
    ) where

import Data.Sequence
import Data.Foldable
import Control.Applicative

preRun :: (Integer, Integer) -> Seq Integer -> Seq Integer
preRun (noun, verb) = update 1 noun . update 2 verb

execute :: (Integer, Integer) -> [Integer] -> [Integer]
execute t = toList . executeCommandAtIndex 0 . preRun t . fromList

executeCommandAtIndex :: Int -> Seq Integer -> Seq Integer
executeCommandAtIndex i xs =
    case (!?) xs i of
        Just 1 -> executeCommandAtIndex (i + 4) (executeAddCommand i xs)
        Just 2 -> executeCommandAtIndex (i + 4) (executeMulCommand i xs)
        Just 99 -> xs
        _ -> error "Unknown Command"

executeAddCommand :: Int -> Seq Integer -> Seq Integer
executeAddCommand = executeCommand (+)

executeMulCommand :: Int -> Seq Integer -> Seq Integer
executeMulCommand = executeCommand (*)

executeCommand ::
    (Integer -> Integer -> Integer) -> Int -> Seq Integer -> Seq Integer
executeCommand f i xs = update n v xs
    where t1 = (!?) xs (i+1) >>= (!?) xs . fromIntegral
          t2 = (!?) xs (i+2) >>= (!?) xs . fromIntegral
          n = case (!?) xs (i+3) of
              Just n' -> fromIntegral n'
              _ -> error "Can't find update index"
          v = case liftA2 f t1 t2 of
              Just v' -> v'
              _ -> error "blah"