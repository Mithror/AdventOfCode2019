{-# LANGUAGE OverloadedStrings #-}

module IntCode
    (
        Input
    ,   Output
    ,   Program(..)
    ,   parseProgram
    ,   runProgram
    ,   getOutputs
    ,   isHalted
    ) where

import Data.Char
import Data.Maybe
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as P
import qualified Data.Vector as V
import qualified Data.Map as M

import Debug.Trace

-- type Memory = V.Vector Integer
type Memory = M.Map Int Integer

newtype ADDR = ADDR { runADDR :: Program -> Integer }
newtype WADDR = WADDR { runWADDR :: Program -> Int }
        --     POSITION Int
        --   | IMMEDIATE Integer
        --  deriving (Show)
-- instance Show ADDR where
--     show a = "_"

type Input = Integer
type Output = Integer

data Instruction = ADD ADDR ADDR WADDR
                 | MUL ADDR ADDR WADDR
                 | STORE WADDR
                 | LOAD ADDR --{ runLoad :: Program -> Integer }
                 | JMPT ADDR ADDR
                 | JMPF ADDR ADDR
                 | LT' ADDR ADDR WADDR
                 | EQ' ADDR ADDR WADDR
                 | RB ADDR --{ runLoad :: Program -> Integer }
                 | HALT
                --  deriving (Show)

type PC = Int
-- type RunningProg = ([Input], Memory, PC, [Output])

data Program = RUNNING { memory :: Memory
                       , pc ::PC
                       , output :: [Output]
                       , rbase :: Integer }
             | WAITING { memory :: Memory
                       , pc ::PC
                       , output :: [Output]
                       , rbase :: Integer }
             | HALTED { outputs :: [Output] }
             deriving (Show)

parseProgram :: String -> Program
parseProgram s = RUNNING (parseMemory (T.pack s)) 0 [] 0

parseMemory :: T.Text -> Memory
parseMemory t =
    let parsed = P.parse (P.sepBy1 (P.signed P.decimal) (P.char ',')) t in
        case parsed of
            P.Done _ r -> M.fromList $ zip [0..] r
            P.Partial f -> M.fromList . zip [0..] $
                fromMaybe []
                          (P.maybeResult (P.feed parsed ""))
            _ -> M.empty

getOutputs :: Program -> (Program, [Output])
getOutputs (HALTED o) = (HALTED o, o)
getOutputs (RUNNING m c o b) = (RUNNING m c [] b, o)
getOutputs (WAITING m c o b) = (WAITING m c [] b, o)

isHalted :: Program -> Bool
isHalted HALTED{} = True
isHalted _ = False

runProgram :: Program -> Maybe Input -> Program
runProgram p@HALTED{} _ = p
runProgram p@WAITING{} Nothing = p
runProgram (WAITING m c o b) mi = runProgram (RUNNING m c o b) mi
runProgram p@(RUNNING m' pc _ _) i =
    let inst = parseInstruction m' pc
        (p', input) = executeInstruction i p inst in
          runProgram p' input

fd :: Memory -> Int -> Integer
fd m i = M.findWithDefault 0 i m

parseInstruction :: Memory -> PC -> Instruction
parseInstruction p i =
    let inst     = p `fd` i
        a        = (inst `div` 10000)
        b        = (inst `mod` 10000) `div` 1000
        c        = (inst `mod` 1000)  `div` 100
        de       = (inst `mod` 100)
        -- addr b a = case b of
        --              0 -> ADDR $ fromIntegral . flip fd (fromInteger a) . memory
        --              1 -> ADDR $ const (fromIntegral a)
        --              2 -> ADDR $ fromInteger . (+a) . rbase
        --              _ -> error "not supported mode"
        waddr b a = case b of
                    0 -> WADDR $ fromInteger . const a
                    2 -> WADDR $ fromInteger . (+a) . rbase
                    _ -> error $ "not supported write mode " ++ show b ++ " " ++ show a
        raddr b a = case b of
                      0 -> ADDR $ flip fd (fromInteger a) . memory
                      1 -> ADDR $ const a
                      2 -> ADDR $ \p -> fd (memory p) (fromInteger $ rbase p + a)
                      _ -> error $ "not supported load mode " ++ show b ++ " " ++ show a
    in
        case de of
            1 -> ADD (raddr c (p `fd` (i+1)))
                     (raddr b (p `fd` (i+2)))
                     (waddr a (p `fd` (i+3)))
            2 -> MUL (raddr c (p `fd` (i+1)))
                     (raddr b (p `fd` (i+2)))
                     (waddr a (p `fd` (i+3)))
            3 -> STORE $  waddr c (p `fd` (i+1))
            4 -> LOAD $ raddr c (p `fd` (i+1))
            5 -> JMPT (raddr c (p `fd` (i+1))) (raddr b (p `fd` (i+2)))
            6 -> JMPF (raddr c (p `fd` (i+1))) (raddr b (p `fd` (i+2)))
            7 -> LT' (raddr c (p `fd` (i+1)))
                     (raddr b (p `fd` (i+2)))
                     (waddr a (p `fd` (i+3)))
            8 -> EQ' (raddr c (p `fd` (i+1)))
                     (raddr b (p `fd` (i+2)))
                     (waddr a (p `fd` (i+3)))
            9 -> RB $ raddr c (p `fd` (i+1))
            99 -> HALT
            _ -> error $ "unknown instruction " ++ show ((a,b,c), de, p, i)

executeInstruction :: Maybe Input
                   -> Program
                   -> Instruction
                   -> (Program, Maybe Input)
executeInstruction mi p@(HALTED _) _ = (p, mi)
executeInstruction Nothing p@WAITING{} _ = (p, Nothing)
-- executeInstruction input p@(WAITING _ _ (Just _) _) _ = (p, input)
executeInstruction input p@(WAITING m pc o b) i =
    executeInstruction input (RUNNING m pc o b) i -- try running again
executeInstruction mi p@(RUNNING m pc outputs base) i =
    case i of
        ADD a b c   ->
            (RUNNING (M.insert (runWADDR c p) (fromIntegral (runADDR a p + runADDR b p)) m) (pc + 4) outputs base, mi)
        MUL a b c   ->
            (RUNNING (M.insert (runWADDR c p) (fromIntegral (runADDR a p * runADDR b p)) m) (pc + 4) outputs base, mi)
        STORE a -> case mi of
                        Nothing -> (WAITING m pc outputs base, mi)
                        Just b -> (RUNNING (M.insert (runWADDR a p) b m) (pc + 2) outputs base, Nothing)
        LOAD a  -> (RUNNING m (pc + 2) (outputs ++ [runADDR a p]) base, mi)
        JMPT a b ->
            (RUNNING m (if runADDR a p /= 0 then fromInteger $ runADDR b p else pc + 3) outputs base, mi)
        JMPF a b ->
            (RUNNING m (if runADDR a p == 0 then fromInteger $ runADDR b p else pc + 3) outputs base, mi)
        LT' a b c   ->
            (RUNNING (M.insert (runWADDR c p) (if runADDR a p < runADDR b p then 1 else 0) m) (pc + 4) outputs base, mi)
        EQ' a b c   ->
            (RUNNING (M.insert (runWADDR c p) (if runADDR a p == runADDR b p then 1 else 0) m) (pc + 4) outputs base, mi)
        RB a -> (RUNNING m (pc + 2) outputs (base + runADDR a p), mi)
        HALT -> (HALTED outputs, mi)
