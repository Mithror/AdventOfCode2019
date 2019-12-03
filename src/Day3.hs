module Day3
    (
        getDistanceAndSteps
    ) where

import Numeric.Natural
import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative
import Data.Map as M
import Data.List

data Instruction = R Natural
                 | L Natural
                 | D Natural
                 | U Natural
                 deriving (Show, Eq)

type Line = [Instruction]

readInstructions :: ReadP Line
readInstructions = sepBy readInstruction (char ',')

readInstruction :: ReadP Instruction
readInstruction = do
    c <- char 'R' <|> char 'L' <|> char 'D' <|> char 'U'
    n <- read <$> many1 (satisfy isDigit)
    case c of
        'R' -> return $ R n
        'L' -> return $ L n
        'D' -> return $ D n
        'U' -> return $ U n
        _ -> error "fail instruction"

type Coord = (Integer, Integer)
type MyMap = M.Map Coord Natural

executeInstruction :: Coord
                   -> Natural
                   -> Instruction
                   -> MyMap
                   -> (MyMap, Coord, Natural)
executeInstruction (x,y) steps (R n) s = (go 1 s, (x + fromIntegral n, y), steps + n)
    where go n' s'
            | n' == fromIntegral n + 1 = s'
            | otherwise = M.insertWith min (x + n', y) (steps + fromInteger n') (go (n'+ 1) s')
executeInstruction (x,y) steps (L n) s = (go 1 s, (x - fromIntegral n,y), steps + n)
    where go n' s'
            | n' == fromIntegral n + 1 = s'
            | otherwise = M.insertWith min (x - n', y) (steps + fromInteger n') (go (n'+ 1) s')
executeInstruction (x,y) steps (U n) s = (go 1 s, (x,y + fromIntegral n), steps + n)
    where go n' s'
            | n' == fromIntegral n + 1 = s'
            | otherwise = M.insertWith min (x, y + n') (steps + fromInteger n') (go (n'+ 1) s')
executeInstruction (x,y) steps (D n) s = (go 1 s, (x,y - fromIntegral n), steps + n)
    where go n' s'
            | n' == fromIntegral n + 1 = s'
            | otherwise = M.insertWith min (x, y - n') (steps + fromInteger n') (go (n'+ 1) s')

executeAllInstructions :: Line -> Coord -> Natural -> MyMap
executeAllInstructions [] _ _ = M.empty
executeAllInstructions (x:xs) c steps =
    let (s, c', steps') = executeInstruction c steps x M.empty in
        M.unionWith min s (executeAllInstructions xs c' steps')

executeForAllLines :: [Line] -> Coord -> Natural -> [MyMap]
executeForAllLines [] _ _ = []
executeForAllLines (x:xs) c steps =
    executeAllInstructions x c steps : executeForAllLines xs c steps

getIntersection :: [MyMap] -> M.Map Coord [Natural]
getIntersection [] = M.empty
getIntersection (x:xs) = Prelude.foldr (M.intersectionWith (:)) (M.map (:[]) x) xs

getDistance :: Coord -> Integer
getDistance (x,y) = abs x + abs y

getSmallest :: M.Map Coord [Natural] -> Integer
getSmallest = fst . M.findMin . M.mapKeys getDistance

getSmallest' :: M.Map Coord [Natural] -> Integer
getSmallest' m = let m' = M.map (fromIntegral . sum) m in minimum m'

getDistanceAndSteps :: String -> (Integer, Integer)
getDistanceAndSteps s = (getSmallest c, getSmallest' c)
    where c = getIntersection cs
          cs = executeForAllLines ls (0,0) 0
          ls = fst . last . readP_to_S readInstructions <$> lines s
