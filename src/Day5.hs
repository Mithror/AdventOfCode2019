module Day5
    (
        module Day5
    ) where

import Data.Char
import Data.Maybe
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as P
import qualified Data.Vector as V

type ADDRp = Int
type ADDRi = Integer
type ADDR = Either ADDRp ADDRi
type INPUT = Integer
type OUTPUT = Integer

data Instruction = ADD ADDR ADDR ADDRp
                 | MUL ADDR ADDR ADDRp
                 | STORE ADDRp
                 | LOAD  ADDRp
                 | JMPT ADDR ADDR
                 | JMPF ADDR ADDR
                 | LT' ADDR ADDR ADDRp
                 | EQ' ADDR ADDR ADDRp
                 | HALT
                 deriving (Show)

execute :: INPUT -> String -> [OUTPUT]
execute i s = maybe [] (runProgram i) $ parseProgram (T.pack s)

type Program = V.Vector Integer
type PC = Int
type RunningProg = (INPUT, Program, PC, [OUTPUT])

parseProgram :: T.Text -> Maybe Program
parseProgram = fmap V.fromList . P.maybeResult . parseNumbers
    where parseNumbers = P.parse (P.sepBy1 (P.signed P.decimal) (P.char ','))

runProgram :: INPUT -> Program -> [OUTPUT]
runProgram i p = go i p 0 []
    where go i' p' pc outputs =
            let inst = parseInstruction p' pc in
                case inst of
                    HALT -> outputs
                    _ -> let (i'', p'',pc', outputs') = executeInstruction (i', p', pc, outputs) inst in
                            outputs ++ go i'' p'' pc' outputs'

parseInstruction :: Program -> PC -> Instruction
parseInstruction p i =
    let inst     = p V.! i
        a        = (inst `div` 10000) /= 0
        b        = (inst `mod` 10000) `div` 1000 /= 0
        c        = (inst `mod` 1000)  `div` 100 /= 0
        de       = (inst `mod` 100)
        addr b a = if b then Right a else Left (fromInteger a)
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

executeInstruction :: RunningProg -> Instruction -> RunningProg
executeInstruction (input, p, pc, outputs) i =
    case i of
        ADD (Left a) (Left b) c   ->
            (input, p V.// [(c, (p V.! a) + (p V.! b))], pc + 4, outputs)
        ADD (Left a) (Right b) c  ->
            (input, p V.// [(c, (p V.! a) + b)], pc + 4, outputs)
        ADD (Right a) (Left b) c  ->
            (input, p V.// [(c, a + (p V.! b))], pc + 4, outputs)
        ADD (Right a) (Right b) c ->
            (input, p V.// [(c, a + b)], pc + 4, outputs)
        MUL (Left a) (Left b) c   ->
            (input, p V.// [(c, (p V.! a) * (p V.! b))], pc + 4, outputs)
        MUL (Left a) (Right b) c  ->
            (input, p V.// [(c, (p V.! a) * b)], pc + 4, outputs)
        MUL (Right a) (Left b) c  ->
            (input, p V.// [(c, a * (p V.! b))], pc + 4, outputs)
        MUL (Right a) (Right b) c ->
            (input, p V.// [(c, a * b)], pc + 4, outputs)
        STORE a -> (input, p V.// [(a, input)], pc + 2, outputs)
        LOAD a  -> (input, p, pc + 2, outputs ++ [p V.! a])
        JMPT (Left a) (Left b) ->
            (input, p, if (p V.! a) /= 0 then fromInteger (p V.! b) else pc + 3, outputs)
        JMPT (Left a) (Right b) ->
            (input, p, if (p V.! a) /= 0 then fromInteger b else pc + 3, outputs)
        JMPT (Right a) (Left b) ->
            (input, p, if a /= 0 then fromInteger (p V.! b) else pc + 3, outputs)
        JMPT (Right a) (Right b) ->
            (input, p, if a /= 0 then fromInteger b else pc + 3, outputs)
        JMPF (Left a) (Left b) ->
            (input, p, if (p V.! a) == 0 then fromInteger (p V.! b) else pc + 3, outputs)
        JMPF (Left a) (Right b) ->
            (input, p, if (p V.! a) == 0 then fromInteger b else pc + 3, outputs)
        JMPF (Right a) (Left b) ->
            (input, p, if a == 0 then fromInteger (p V.! b) else pc + 3, outputs)
        JMPF (Right a) (Right b) ->
            (input, p, if a == 0 then fromInteger b else pc + 3, outputs)
        LT' (Left a) (Left b) c   ->
            (input, p V.// [(c, if (p V.! a) < (p V.! b) then 1 else 0)], pc + 4, outputs)
        LT' (Left a) (Right b) c  ->
            (input, p V.// [(c, if (p V.! a) < b then 1 else 0)], pc + 4, outputs)
        LT' (Right a) (Left b) c  ->
            (input, p V.// [(c, if a < (p V.! b) then 1 else 0)], pc + 4, outputs)
        LT' (Right a) (Right b) c ->
            (input, p V.// [(c, if a < b then 1 else 0)], pc + 4, outputs)
        EQ' (Left a) (Left b) c   ->
            (input, p V.// [(c, if (p V.! a) == (p V.! b) then 1 else 0)], pc + 4, outputs)
        EQ' (Left a) (Right b) c  ->
            (input, p V.// [(c, if (p V.! a) == b then 1 else 0)], pc + 4, outputs)
        EQ' (Right a) (Left b) c  ->
            (input, p V.// [(c, if a == (p V.! b) then 1 else 0)], pc + 4, outputs)
        EQ' (Right a) (Right b) c ->
            (input, p V.// [(c, if a == b then 1 else 0)], pc + 4, outputs)
        _  -> (input, p, pc + 1, outputs)