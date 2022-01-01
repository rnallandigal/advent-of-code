{-# LANGUAGE TupleSections #-}

module Lib where

import           Control.Monad.Zip
import           Data.Bifunctor
import           Data.Foldable (foldrM)
import           Data.Functor ((<&>))
import           Data.List (sortOn)
import           Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Text.Lazy (unpack)
import           Linear.V2
import qualified Text.Pretty.Simple as T

-- pretty print data with the given terminal width
pShow :: (Show a) => Int -> a -> String
pShow w = unpack . T.pShowOpt T.defaultOutputOptionsNoColor
    { T.outputOptionsIndentAmount   = 2
    , T.outputOptionsPageWidth      = w
    , T.outputOptionsCompact        = True
    , T.outputOptionsCompactParens  = True }

-- read an integer with an optional sign
readInt :: String -> Int
readInt = read . filter (/='+')

-- get all unique subsets of length k which sum to t
ksum :: Int -> Int -> [Int] -> [[Int]]
ksum k t = filter ((==t) . sum) . flip choose k

-- get the first duplicated element
firstDuplicate :: (Ord a) => [a] -> Maybe a
firstDuplicate = helper S.empty where
    helper _ [] = Nothing
    helper s (x:xs)
        | x `S.member` s = Just x
        | otherwise      = helper (S.insert x s) xs

-- get a frequency map of items
freqMap :: (Ord a) => [a] -> M.Map a Int
freqMap = M.fromListWith (+) . map (,1)

-- get all subsets of a given length k
choose :: [a] -> Int -> [[a]]
_       `choose` 0 = [[]]
[]      `choose` _ = []
(x:xs)  `choose` k = ((x:) <$> (xs `choose` (k - 1))) ++ (xs `choose` k)

-- check if a value is between two values
between :: (Ord a) => a -> (a, a) -> Bool
x `between` (l, h) = x >= l && x <= h

-- count the number of elements that satisfy the predicate
count :: Foldable f => (a -> Bool) -> f a -> Int
count f = foldr (\x a -> a + if f x then 1 else 0) 0

-- group elements of a list into pairs
pairs :: [a] -> [(a, a)]
pairs = map (\[x, y] -> (x, y)) . chunksOf 2

pair :: [a] -> (a, a)
pair (x:y:_) = (x, y)

-- fixed point of a function with an initial condition
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x = let y = f x in if x == y then x else fixedPoint f y

-- collect distinct outcomes under perturbation
outcomes :: (a -> [b]) -> [a] -> [[b]]
outcomes p = foldr (\x -> concatMap (\y -> map (:y) $ p x)) [[]]

-- find all viable assignments satisfying the relation using BFS
assign :: (Ord k, Ord v) => [(k, S.Set v)] -> [M.Map k v]
assign relation = map snd $ foldrM bfs (S.empty, M.empty) candidates where
    candidates = reverse $ sortOn (length . snd) relation
    bfs (item, picks) acc@(picked, matching) = do
        pick <- S.toList $ picks S.\\ picked
        return $ bimap (S.insert pick) (M.insert item pick) acc

-- integer square root
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

-- lay items on an x-y cordinate grid
indexed :: [[a]] -> [(V2 Int, a)]
indexed = concat
        . zipWith (\i -> map (\(j, c) -> (V2 i j, c))) [0..]
        . map (zip [0..])

-- get a bounding box of the given points
bounds :: (Foldable a, MonadZip b, Ord c, Bounded (b c))
       => a (b c) -> (b c, b c)
bounds = foldr minmax (maxBound, minBound) where
    minmax x (l, h) = (mzipWith min x l, mzipWith max x h)
