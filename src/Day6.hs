{-# LANGUAGE OverloadedStrings #-}

module Day6
    (
        module Day6
    ) where

import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Applicative (liftA2, (<|>))
import Data.Maybe
import Data.Semigroup
import Data.List ((\\))

type DistanceOrbits = [T.Text]
type OrbitMap = M.Map T.Text DistanceOrbits

parseOrbits :: P.Parser OrbitMap
parseOrbits =
    M.fromListWith (++) <$> P.many1 parseOrbitLine

parseOrbitLine :: P.Parser (T.Text, DistanceOrbits)
parseOrbitLine = do
    key <- P.takeWhile (/= ')')
    P.char ')'
    val <- P.takeWhile (/= '\n')
    P.char '\n'
    return (key, [val])

countOrbits :: String -> Integer
countOrbits s = let m = parseOrbits' s in
    countOrbits' ("COM", 0) m

parseOrbits' :: String -> OrbitMap
parseOrbits' s = let parsed = P.parse parseOrbits (T.pack s) in
    case parsed of
        P.Done _ r -> r
        P.Partial f ->
            fromMaybe M.empty
                      (P.maybeResult (P.feed parsed ""))
        _ -> M.empty

countOrbits' :: (T.Text, Integer) -> OrbitMap -> Integer
countOrbits' (root, currDist) m =
    let mxs = m M.!? root in
        case mxs of
            Nothing -> currDist
            Just xs -> foldr (\a b -> b + countOrbits' (a, currDist + 1) m ) currDist xs

pathFromRoot :: T.Text -> OrbitMap -> [T.Text]
pathFromRoot result m = fromMaybe [] $ go "COM" []
    where go x out =
            if x == result
            then Just (out ++ [result])
            else case m M.!? x of
                    Nothing -> Nothing
                    Just ys -> getFirst <$>
                        foldr ((<>) . fmap First)
                            Nothing
                            (fmap (\a -> go a (out ++ [x])) ys)

nOrbits :: String -> Integer
nOrbits s =
    let m = parseOrbits' s
        p1 = pathFromRoot "YOU" m
        p2 = pathFromRoot "SAN" m
    in
        fromIntegral $ length (p1 \\ p2) + length (p2 \\ p1) - 2