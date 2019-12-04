module Day4
    (
      getNumberOfPasswords
    , getNumberOfPasswords'
    ) where

import Data.List

generatePasswords_Part1 :: [Integer]
generatePasswords_Part1 =
    [ x | x <- [145852..616942]
        , has2Same (show x)
        , increases ((read :: String -> Integer) . (:[]) <$> show x)]
    where
        has2Same xs = let ys = zip xs (tail xs ++ " ") in
            any (uncurry (==)) ys
        increases [] = True
        increases xs = let ys = zip xs (tail xs ++ [maximum xs]) in
            all (uncurry (<=)) ys

getNumberOfPasswords :: Int
getNumberOfPasswords = length generatePasswords_Part1

generatePasswords_Part2 :: [Integer]
generatePasswords_Part2 = filter f generatePasswords_Part1
    where f x = 2 `elem` (length <$> group (show x))

getNumberOfPasswords' :: Int
getNumberOfPasswords' = length generatePasswords_Part2