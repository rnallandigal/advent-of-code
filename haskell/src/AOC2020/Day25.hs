{-# LANGUAGE DataKinds #-}

module AOC2020.Day25 (day25a) where

import Math.NumberTheory.Moduli.Class (Mod (..), getVal, (^%))
import Math.NumberTheory.Moduli.Multiplicative
import Math.NumberTheory.Moduli.Singleton (CyclicGroup (..), cyclicGroup)

import Lib (pair)

type M = 20201227

input :: String -> (Mod M, Mod M)
input = pair . map (fromInteger . read) . lines

solve :: Mod M -> Mod M -> Integer
solve cardKey doorKey = getVal $ doorKey ^% dlog where
    dlog          = discreteLogarithm m msg cardKey'
    Just m        = cyclicGroup :: Maybe (CyclicGroup Integer M)
    Just msg      = isPrimitiveRoot m 7
    Just cardKey' = isMultElement cardKey

day25a :: String -> String
day25a = show . uncurry solve . input
