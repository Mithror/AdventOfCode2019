module Day3
    (
        module Day3
    ) where

import Numeric.Natural
import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative
import Data.Set as S

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

executeInstruction :: Coord
                   -> Instruction
                   -> S.Set Coord
                   -> (S.Set Coord, Coord)
executeInstruction (x,y) (R n) s = (go (fromIntegral n) s, (x,y + fromIntegral n))
    where go 0  s' = s'
          go n' s'  = S.insert (x,y + n') (go (n' - 1) s')
executeInstruction (x,y) (L n) s = (go (fromIntegral n) s, (x,y - fromIntegral n))
    where go 0  s' = s'
          go n' s'  = S.insert (x,y - n') (go (n' - 1) s')
executeInstruction (x,y) (U n) s = (go (fromIntegral n) s, (x + fromIntegral n,y))
    where go 0  s' = s'
          go n' s'  = S.insert (x + n',y) (go (n' - 1) s')
executeInstruction (x,y) (D n) s = (go (fromIntegral n) s, (x - fromIntegral n,y))
    where go 0  s' = s'
          go n' s'  = S.insert (x - n',y) (go (n' - 1) s')

executeAllInstructions :: Line -> Coord -> S.Set Coord
executeAllInstructions [] _ = S.empty
executeAllInstructions (x:xs) c =
    let (s, c') = executeInstruction c x S.empty in
        union s (executeAllInstructions xs c')

executeForAllLines :: [Line] -> Coord -> [S.Set Coord]
executeForAllLines [] _ = []
executeForAllLines (x:xs) c =
    executeAllInstructions x c : executeForAllLines xs c

getIntersection :: [S.Set Coord] -> S.Set Coord
getIntersection [] = S.empty
getIntersection (x:xs) = Prelude.foldr S.intersection x xs

getDistance :: Coord -> Integer
getDistance (x,y) = abs x + abs y

getSmallest :: S.Set Coord -> Integer
getSmallest = S.findMin . S.map getDistance

getNumber :: String -> Integer
getNumber s = getSmallest c
    where c = getIntersection cs
          cs = executeForAllLines ls (0,0)
          ls = fst . last . readP_to_S readInstructions <$> lines s
