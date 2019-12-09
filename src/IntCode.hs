{-# LANGUAGE OverloadedStrings #-}

module IntCode
    (
        Input
    ,   Output
    ,   Program(..)
    ,   parseProgram
    ,   runProgram
    ,   getOutput
    ,   isHalted
    ) where

import Data.Char
import Data.Maybe
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as P
import qualified Data.Vector as V

data ADDR = POSITION Int
          | IMMEDIATE Integer
          deriving (Show)

type Input = Integer
type Output = Integer

data Instruction = ADD ADDR ADDR Int
                 | MUL ADDR ADDR Int
                 | STORE Int
                 | LOAD  Int
                 | JMPT ADDR ADDR
                 | JMPF ADDR ADDR
                 | LT' ADDR ADDR Int
                 | EQ' ADDR ADDR Int
                 | HALT
                 deriving (Show)

type Memory = V.Vector Integer
type PC = Int
-- type RunningProg = ([Input], Memory, PC, [Output])

data Program = RUNNING { memory :: Memory
                       , pc ::PC
                       , output :: Maybe Output }
             | WAITING { memory :: Memory
                       , pc ::PC
                       , output :: Maybe Output }
             | HALTED { outputs :: Output }
             deriving (Show)

parseProgram :: String -> Program
parseProgram s = WAITING (parseMemory (T.pack s)) 0 Nothing

parseMemory :: T.Text -> Memory
parseMemory t =
    let parsed = P.parse (P.sepBy1 (P.signed P.decimal) (P.char ',')) t in
        case parsed of
            P.Done _ r -> V.fromList r
            P.Partial f -> V.fromList $
                fromMaybe []
                          (P.maybeResult (P.feed parsed ""))
            _ -> V.empty

getOutput :: Program -> (Program, Maybe Output)
getOutput (HALTED o) = (HALTED o, Just o)
getOutput (RUNNING m c o) = (RUNNING m c Nothing, o)
getOutput (WAITING m c o) = (WAITING m c Nothing, o)

isHalted :: Program -> Bool
isHalted HALTED{} = True
isHalted _ = False

runProgram :: Program -> Maybe Input -> Program
runProgram p@HALTED{} _ = p
runProgram p@(WAITING m c o) Nothing = p
runProgram (WAITING m c o) mi = runProgram (RUNNING m c o) mi
runProgram p@(RUNNING m' pc _) i =
    let inst = parseInstruction m' pc
        (p', input) = executeInstruction i p inst in
        runProgram p' input

parseInstruction :: Memory -> PC -> Instruction
parseInstruction p i =
    let inst     = p V.! i
        a        = (inst `div` 10000) /= 0
        b        = (inst `mod` 10000) `div` 1000 /= 0
        c        = (inst `mod` 1000)  `div` 100 /= 0
        de       = (inst `mod` 100)
        addr b a = if b then IMMEDIATE a else POSITION (fromInteger a)
    in
        case de of
            1 -> ADD (addr c (p V.! (i+1)))
                     (addr b (p V.! (i+2)))
                     (fromInteger (p V.! (i+3)))
            2 -> MUL (addr c (p V.! (i+1)))
                     (addr b (p V.! (i+2)))
                     (fromInteger (p V.! (i+3)))
            3 -> STORE $ fromInteger (p V.! (i+1))
            4 -> LOAD  $ fromInteger (p V.! (i+1))
            5 -> JMPT (addr c (p V.! (i+1))) (addr b (p V.! (i+2)))
            6 -> JMPF (addr c (p V.! (i+1))) (addr b (p V.! (i+2)))
            7 -> LT' (addr c (p V.! (i+1)))
                     (addr b (p V.! (i+2)))
                     (fromInteger (p V.! (i+3)))
            8 -> EQ' (addr c (p V.! (i+1)))
                     (addr b (p V.! (i+2)))
                     (fromInteger (p V.! (i+3)))
            99 -> HALT
            _ -> error $ "unknown instruction " ++ show ((a,b,c), de)

executeInstruction :: Maybe Input
                   -> Program
                   -> Instruction
                   -> (Program, Maybe Input)
executeInstruction mi p@(HALTED _) _ = (p, mi)
executeInstruction Nothing p@WAITING{} _ = (p, Nothing)
executeInstruction (Just input) p@(WAITING m pc o) i =
    executeInstruction (Just input) (RUNNING m pc o) i -- try running again
executeInstruction mi (RUNNING p pc outputs) i =
    case i of
        ADD (POSITION a) (POSITION b) c   ->
            (RUNNING (p V.// [(c, (p V.! a) + (p V.! b))]) (pc + 4) outputs, mi)
        ADD (POSITION a) (IMMEDIATE b) c  ->
            (RUNNING (p V.// [(c, (p V.! a) + b)]) (pc + 4) outputs, mi)
        ADD (IMMEDIATE a) (POSITION b) c  ->
            (RUNNING (p V.// [(c, a + (p V.! b))]) (pc + 4) outputs, mi)
        ADD (IMMEDIATE a) (IMMEDIATE b) c ->
            (RUNNING (p V.// [(c, a + b)]) (pc + 4) outputs, mi)
        MUL (POSITION a) (POSITION b) c   ->
            (RUNNING (p V.// [(c, (p V.! a) * (p V.! b))]) (pc + 4) outputs, mi)
        MUL (POSITION a) (IMMEDIATE b) c  ->
            (RUNNING (p V.// [(c, (p V.! a) * b)]) (pc + 4) outputs, mi)
        MUL (IMMEDIATE a) (POSITION b) c  ->
            (RUNNING (p V.// [(c, a * (p V.! b))]) (pc + 4) outputs, mi)
        MUL (IMMEDIATE a) (IMMEDIATE b) c ->
            (RUNNING (p V.// [(c, a * b)]) (pc + 4) outputs, mi)
        STORE a -> case mi of
                        Nothing -> (WAITING p pc outputs, mi)
                        Just b -> (RUNNING (p V.// [(a, b)]) (pc + 2) outputs, Nothing)
        LOAD a  -> (RUNNING p (pc + 2) (Just $ p V.! a), mi)
        JMPT (POSITION a) (POSITION b) ->
            (RUNNING p (if (p V.! a) /= 0 then fromInteger (p V.! b) else pc + 3) outputs, mi)
        JMPT (POSITION a) (IMMEDIATE b) ->
            (RUNNING p (if (p V.! a) /= 0 then fromInteger b else pc + 3) outputs, mi)
        JMPT (IMMEDIATE a) (POSITION b) ->
            (RUNNING p (if a /= 0 then fromInteger (p V.! b) else pc + 3) outputs, mi)
        JMPT (IMMEDIATE a) (IMMEDIATE b) ->
            (RUNNING p (if a /= 0 then fromInteger b else pc + 3) outputs, mi)
        JMPF (POSITION a) (POSITION b) ->
            (RUNNING p (if (p V.! a) == 0 then fromInteger (p V.! b) else pc + 3) outputs, mi)
        JMPF (POSITION a) (IMMEDIATE b) ->
            (RUNNING p (if (p V.! a) == 0 then fromInteger b else pc + 3) outputs, mi)
        JMPF (IMMEDIATE a) (POSITION b) ->
            (RUNNING p (if a == 0 then fromInteger (p V.! b) else pc + 3) outputs, mi)
        JMPF (IMMEDIATE a) (IMMEDIATE b) ->
            (RUNNING p (if a == 0 then fromInteger b else pc + 3) outputs, mi)
        LT' (POSITION a) (POSITION b) c   ->
            (RUNNING (p V.// [(c, if (p V.! a) < (p V.! b) then 1 else 0)]) (pc + 4) outputs, mi)
        LT' (POSITION a) (IMMEDIATE b) c  ->
            (RUNNING (p V.// [(c, if (p V.! a) < b then 1 else 0)]) (pc + 4) outputs, mi)
        LT' (IMMEDIATE a) (POSITION b) c  ->
            (RUNNING (p V.// [(c, if a < (p V.! b) then 1 else 0)]) (pc + 4) outputs, mi)
        LT' (IMMEDIATE a) (IMMEDIATE b) c ->
            (RUNNING (p V.// [(c, if a < b then 1 else 0)]) (pc + 4) outputs, mi)
        EQ' (POSITION a) (POSITION b) c   ->
            (RUNNING (p V.// [(c, if (p V.! a) == (p V.! b) then 1 else 0)]) (pc + 4) outputs, mi)
        EQ' (POSITION a) (IMMEDIATE b) c  ->
            (RUNNING (p V.// [(c, if (p V.! a) == b then 1 else 0)]) (pc + 4) outputs, mi)
        EQ' (IMMEDIATE a) (POSITION b) c  ->
            (RUNNING (p V.// [(c, if a == (p V.! b) then 1 else 0)]) (pc + 4) outputs, mi)
        EQ' (IMMEDIATE a) (IMMEDIATE b) c ->
            (RUNNING (p V.// [(c, if a == b then 1 else 0)]) (pc + 4) outputs, mi)
        HALT -> (HALTED (fromJust outputs), mi)

extractHead :: [a] -> Maybe (a, [a])
extractHead [] = Nothing
extractHead (x:xs) = Just (x, xs)
