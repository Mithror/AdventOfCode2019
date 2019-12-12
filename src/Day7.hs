module Day7
    (
        module Day7
    ) where

import IntCode
import Data.List
import Debug.Trace
import Data.Maybe

findMaxAmp :: String -> Integer
findMaxAmp p = maximum (fmap (`calculateAmpForPhases` p) (permutations [0..4]))

calculateAmpForPhases :: [Integer] -> String -> Integer
calculateAmpForPhases phases p = foldr f 0 $ reverse phases
        where f a b = calculateAmp a b p

calculateAmp :: Integer -> Integer -> String -> Integer
calculateAmp phase input = head
                         . snd
                         . getOutputs
                         . flip runProgram (Just input)
                         . flip runProgram (Just phase)
                         . parseProgram

findMaxAmpWithFB :: String -> Integer
findMaxAmpWithFB s =
    let p = parseProgram s
        allInits = fmap (\a -> feedPhases (zip a (replicate 5 p)))
                        (permutations [5..9])
        allRuns = fmap (runRest 0) allInits
    in
        maximum allRuns

feedPhases :: [(Input, Program)] -> [Program]
feedPhases = fmap (\(a, p) -> runProgram p (Just a))

runRest :: Input -> [Program] -> Output
runRest i ps =
    if isHalted (last ps)
    then i
    else let (ps', o) = runLoop i ps in runRest o ps'

runLoop :: Input -> [Program] -> ([Program], Output)
runLoop i [] = ([],i)
runLoop i (x:xs) =
    let (p, os) = getOutputs $ runProgram x (Just i)
        (ps, o') = runLoop (head os) xs in
            (p : ps, o')