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
import qualified Data.Map as M

type Memory = M.Map Int Integer
type Input = Integer
type Output = Integer
type PC = Int

data RunState = RUNNING | WAITING | HALTED deriving (Show, Eq)
data Program = Program { runState :: RunState
                       , memory :: Memory
                       , pc :: PC
                       , outputs :: [Output]
                       , rbase :: Integer }
             deriving (Show)

newtype RADDR = RADDR { runRADDR :: Program -> Integer }
newtype WADDR = WADDR { runWADDR :: Program -> Int }

data Instruction = ADD RADDR RADDR WADDR
                 | MUL RADDR RADDR WADDR
                 | STORE WADDR
                 | LOAD RADDR
                 | JMPT RADDR RADDR
                 | JMPF RADDR RADDR
                 | LT' RADDR RADDR WADDR
                 | EQ' RADDR RADDR WADDR
                 | RB RADDR
                 | HALT

parseProgram :: String -> Program
parseProgram s = Program { runState = RUNNING
                         , memory = parseMemory $ T.pack s
                         , pc = 0
                         , outputs = []
                         , rbase = 0
                         }

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
getOutputs p = (p { outputs = [] }, outputs p)

isHalted :: Program -> Bool
isHalted = (==HALTED) . runState

runProgram :: Program -> Maybe Input -> Program
runProgram p minput =
    case runState p of
        HALTED -> p
        WAITING -> if isJust minput then
                        runProgram p { runState = RUNNING} minput
                   else
                        p
        RUNNING ->
            case parseInstruction (memory p) (pc p) of
                ADD a b c ->
                    let p' = p { memory =
                                    M.insert (runWADDR c p)
                                             (runRADDR a p + runRADDR b p)
                                             (memory p)
                                , pc = pc p + 4
                                } in
                    runProgram p' minput
                MUL a b c ->
                    let p' = p { memory =
                                    M.insert (runWADDR c p)
                                             (runRADDR a p * runRADDR b p)
                                             (memory p)
                                , pc = pc p + 4
                                } in
                    runProgram p' minput
                STORE a ->
                    case minput of
                        Nothing -> p
                        Just b ->
                            let p' = p { memory = M.insert (runWADDR a p)
                                                           b
                                                           (memory p)
                                        , pc = pc p + 2
                                        } in
                            runProgram p' Nothing
                LOAD a  ->
                    let p' = p { pc = pc p + 2
                               , outputs = outputs p ++ [runRADDR a p]
                               } in
                    runProgram p' minput
                JMPT a b ->
                    let p' = p { pc = if runRADDR a p /= 0 then
                                        fromInteger $ runRADDR b p
                                      else
                                        pc p + 3
                                } in
                    runProgram p' minput
                JMPF a b ->
                    let p' = p { pc = if runRADDR a p == 0 then
                                        fromInteger $ runRADDR b p
                                      else
                                        pc p + 3
                                } in
                    runProgram p' minput
                LT' a b c   ->
                    let p' = p { memory =
                                    M.insert (runWADDR c p)
                                             (if runRADDR a p < runRADDR b p
                                              then 1 else 0)
                                             (memory p)
                                , pc = pc p + 4
                                } in
                    runProgram p' minput
                EQ' a b c   ->
                    let p' = p { memory =
                                    M.insert (runWADDR c p)
                                             (if runRADDR a p == runRADDR b p
                                              then 1 else 0)
                                             (memory p)
                                , pc = pc p + 4
                                } in
                    runProgram p' minput
                RB a ->
                    let p' = p { pc = pc p + 2
                               , rbase = rbase p + runRADDR a p } in
                    runProgram p' minput
                HALT -> p { runState = HALTED }

fd :: Memory -> PC -> Integer
fd m pc = M.findWithDefault 0 pc m

parseInstruction :: Memory -> PC -> Instruction
parseInstruction m pc =
    let inst     = m `fd` pc
        mode3        = (inst `div` 10000)
        mode2        = (inst `mod` 10000) `div` 1000
        mode1        = (inst `mod` 1000)  `div` 100
        opcode       = (inst `mod` 100)
        waddr mode val =
            case mode of
                0 -> WADDR $ fromInteger . const val
                2 -> WADDR $ fromInteger . (+val) . rbase
                _ -> error $ "unsupported write mode " ++ show mode
        raddr mode val =
            case mode of
                0 -> RADDR $ flip fd (fromInteger val) . memory
                1 -> RADDR $ const val
                2 -> RADDR $ \p -> fd (memory p) (fromInteger $ rbase p + val)
                _ -> error $ "unsupported read mode " ++ show mode
    in
        case opcode of
            1 -> ADD (raddr mode1 (m `fd` (pc+1)))
                     (raddr mode2 (m `fd` (pc+2)))
                     (waddr mode3 (m `fd` (pc+3)))
            2 -> MUL (raddr mode1 (m `fd` (pc+1)))
                     (raddr mode2 (m `fd` (pc+2)))
                     (waddr mode3 (m `fd` (pc+3)))
            3 -> STORE $ waddr mode1 (m `fd` (pc+1))
            4 -> LOAD $ raddr mode1 (m `fd` (pc+1))
            5 -> JMPT (raddr mode1 (m `fd` (pc+1)))
                      (raddr mode2 (m `fd` (pc+2)))
            6 -> JMPF (raddr mode1 (m `fd` (pc+1)))
                      (raddr mode2 (m `fd` (pc+2)))
            7 -> LT' (raddr mode1 (m `fd` (pc+1)))
                     (raddr mode2 (m `fd` (pc+2)))
                     (waddr mode3 (m `fd` (pc+3)))
            8 -> EQ' (raddr mode1 (m `fd` (pc+1)))
                     (raddr mode2 (m `fd` (pc+2)))
                     (waddr mode3 (m `fd` (pc+3)))
            9 -> RB $ raddr mode1 (m `fd` (pc+1))
            99 -> HALT
            _ -> error $ "unknown opcode " ++ show opcode
