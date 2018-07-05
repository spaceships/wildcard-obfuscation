#!/usr/bin/runghc

import System.Environment
import Control.Monad hiding (join)
import System.IO
import System.Exit
import Data.Char
import Data.List
import Data.Ord
import Data.Bits

main = do
    (nbits, comb, args) <- processArgs (8, False, []) <$> getArgs
    let strs = genStrs nbits (map read args)
    putStr $ "(" ++ intercalate "|" (if comb then combine strs else strs) ++ ")"

processArgs :: (Int, Bool, [String]) -> [String] -> (Int, Bool, [String])
processArgs t [] = t
processArgs t@(nbits, combineFlag, positional) args = case head args of
    "-c" -> processArgs (nbits, True, positional) (tail args)
    "-b" -> processArgs (read (head (tail args)), combineFlag, positional) (tail (tail args))
    arg  -> processArgs (nbits, combineFlag, positional ++ [arg]) (tail args)

sortByStars :: [String] -> [String]
sortByStars = sortBy starsCmp
  where
    nstars = map (\c -> if c == '*' then 1 else 0)
    starsCmp s1 s2 = compare (nstars s1) (nstars s2)

generate :: [Int] -> [[Int]]
generate thresholds = sequence [ [ 0..max ] | max <- thresholds ]

genStrs :: Int -> [Int] -> [String]
genStrs nbits thresholds = map (concat . map (asBinary nbits)) (generate thresholds)

-- hamming distance
delta :: String -> String -> Int
delta s1 s2 = sum (zipWith ham s1 s2)
  where
    ham '*' '*' = 0
    ham '*' _ = 1
    ham _ '*' = 1
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
combineRec (x:xs) n = case find (\y -> delta x y <= 1) xs of
    Just y  -> combineRec (star x y : delete y xs) (n+1)
    Nothing -> let (ys,n') = combineRec xs n in (x:ys, n')

asBinary :: Int -> Int -> String
asBinary nbits x = reverse (helper nbits x)
  where
    helper 0 _ = ""
    helper n y = let b = if testBit y 0 then '1' else '0' in b : helper (n-1) (shiftR y 1)
