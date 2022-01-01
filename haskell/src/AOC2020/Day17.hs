module AOC2020.Day17 (day17a, day17b) where

import qualified Data.IntSet as S
import           Data.List (foldl', unfoldr)
import           Linear.V2

import           Lib (count, indexed, outcomes)

extent :: Int
extent = 100

serialize :: Int -> [Int] -> Int
serialize d = foldl' (\i p -> extent * i + (p + (extent `div` 2))) 0

deserialize :: Int -> Int -> [Int]
deserialize d = reverse . map (subtract (extent `div` 2)) . unfoldr gen where
    gen 0 = Nothing
    gen x = Just (x `mod` extent, x `div` extent)

input :: Int -> String -> S.IntSet
input d = S.fromList . map up . filter ((=='#') . snd) . indexed . lines where
    up (V2 y x, _) = serialize d $ replicate (d - 2) 0 ++ [y, x]

simulate :: Int -> S.IntSet -> S.IntSet
simulate d s = S.filter rules $ candidates s where
    neighbors' = tail . outcomes (\p -> fmap (+p) [0, -1, 1])
    neighbors  = map (serialize d) . neighbors' . deserialize d
    candidates = S.foldr (\p g -> foldr S.insert g $ neighbors p) S.empty
    rules p
        | p `S.member` s && (active == 2 || active == 3) = True
        | p `S.member` s                                 = False
        | active == 3                                    = True
        | otherwise                                      = False
        where
            active = count (`S.member` s) $ neighbors p

day17a :: String -> String
day17a = show . S.size . (!! 6) . iterate (simulate 3) . input 3 where

day17b :: String -> String
day17b = show . S.size . (!! 6) . iterate (simulate 4) . input 4 where
