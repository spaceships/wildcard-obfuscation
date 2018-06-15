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

    let vals = sequence [ [ 0..max ] | max <- map read args :: [Int] ]
        strs = (map.concatMap) (printf "%08b") vals

    mapM putStrLn (nub (combine (combine strs)))

-- hamming distance of two strings
delta :: String -> String -> Int
delta xs ys = sum (zipWith ham xs ys)
  where ham x y = abs (ord x - ord y)

join :: String -> String -> Maybe String
join xs ys
    | delta xs ys == 1 = Just (zipWith (\x y -> if x == y then x else '*') xs ys)
    | otherwise        = Nothing

combine :: [String] -> [String]
combine xs = do
    x <- xs
    y <- xs
    guard (x /= y)
    case join x y of
        Nothing -> [x,y]
        Just z -> [z]
