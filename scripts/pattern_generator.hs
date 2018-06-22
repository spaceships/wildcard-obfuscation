#!/usr/bin/runghc

import System.Environment
import Text.Printf
import Control.Monad hiding (join)
import System.IO
import System.Exit
import Data.Char
import Data.List
import Data.Ord

main = do
    args <- getArgs
    let strs = case head args of
            "-n" -> map show $ generate (tail (map read args))
            _    -> genStrs (map read args)
    mapM putStrLn strs

sortByStars :: [String] -> [String]
sortByStars = sortBy starsCmp
  where
    nstars = map (\c -> if c == '*' then 1 else 0)
    starsCmp s1 s2 = compare (nstars s1) (nstars s2)

generate :: [Int] -> [[Int]]
generate thresholds = sequence [ [ 0..max ] | max <- thresholds ]

genStrs :: [Int] -> [String]
genStrs thresholds = map (concat . map (printf "%08b")) (generate thresholds)

-- hamming distance
delta :: String -> String -> Int
delta s1 s2 = sum (zipWith ham s1 s2)
  where
    ham c1 c2 = abs (ord c1 - ord c2)

-- put a star where the strings differ
star :: String -> String -> String
star s1 s2 = zipWith (\c1 c2 -> if c1 == c2 then c1 else '*') s1 s2

-- keep combining until there is nothing left to combine
combine :: [String] -> [String]
combine xs = case combineRec xs 0 of
    (ys, 0) -> ys
    (ys, _) -> combine ys

combineRec :: [String] -> Int -> ([String], Int)
combineRec [] n = ([], n)
combineRec (x:xs) n = case find ((==) 1 . delta x) xs of
    Just y  -> combineRec (star x y : delete y xs) (n+1)
    Nothing -> let (ys,n') = combineRec xs n in (x:ys, n')
