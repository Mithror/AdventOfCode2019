module Day8
    (
        module Day8
    ) where

import Data.List
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as P
import Control.Applicative ((<$>))

import Debug.Trace

width = 25
height = 6

nElements = width * height

day8_result1 :: String -> Integer
day8_result1 = calculateResult . findLayerMost0 . parseLayers

day8_result2 :: String -> IO ()
day8_result2 = displayImage . visual . getImage . parseLayers

parseLayers :: String -> [String]
parseLayers [] = []
parseLayers xs =
    let (a, r) = splitAt nElements xs in a : parseLayers r

findLayerMost0 :: [String] -> String
findLayerMost0 [] = error "empty layers"
findLayerMost0 [x] = x
findLayerMost0 (x:xs) =
    let c1 = length . filter (=='0') $ x
        y  = findLayerMost0 xs
        c2 = length . filter (=='0') $ y
    in if c1 < c2 then x else y

calculateResult :: String -> Integer
calculateResult s =
    let c1 = length . filter (=='1') $ s
        c2 = length . filter (=='2') $ s
    in fromIntegral $ c1 * c2

getImage :: [String] -> String
getImage = foldl' f (repeat '2')
    where f a b = g <$> zip a b
          g ('2', a) = a
          g ('1', _) = '1'
          g ('0', _) = '0'
          g (_, _) = '?'

visual :: String -> String
visual [] = []
visual ('1':xs) = '#' : visual xs
visual ('0':xs) = ' ' : visual xs
visual (_:xs) = '?' : visual xs

displayLine :: String -> IO String
displayLine [] = return []
displayLine s = do
    putStrLn $ take width s
    return $ drop width s

displayImage :: String -> IO ()
displayImage [] = return ()
displayImage s = displayLine s >>= displayImage



