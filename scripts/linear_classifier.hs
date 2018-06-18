#!/usr/bin/runghc

import System.Environment
import Text.Printf
import Control.Monad hiding (join)
import System.IO
import System.Exit
import Data.Char
import Data.List

main = do
    args <- getArgs
    when (length args /= 5) $ do
        hPutStrLn stderr "5 integer arguments required"
        exitFailure

    let strs = generate (map read args)
    mapM putStrLn strs

generate :: [Int] -> [String]
generate thresholds = map concat (sequence bytes)
  where
    bytes = [ combine (map (printf "%08b") [ 0..max ]) | max <- thresholds ]

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
