module AOC2020.Day23 (day23a, day23b) where

import           Control.Monad (replicateM_)
import           Control.Monad.Loops (whileM_)
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Char (digitToInt, intToDigit, isSpace)
import           Data.List (find)
import           Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

input :: String -> V.Vector Int
input = V.fromList . map digitToInt . filter (not . isSpace)

run :: Int -> V.Vector Int -> StateT Int (ST s) (V.Vector Int)
run n order = do
    let m = V.length order
    cups <- MV.new (m + 1)
    V.iforM_ order $ \i a -> MV.write cups a (order V.! ((i + 1) `mod` m))
    replicateM_ n $ do
        curr <- get
        a <- MV.read cups curr
        b <- MV.read cups a
        c <- MV.read cups b
        let dest = fromJust $ find (not . flip elem [a, b, c]) candidates where
            candidates = tail $ iterate (\x -> if x == 1 then m else x - 1) curr
        MV.exchange cups dest a >>= MV.exchange cups c >>= \x -> do
            MV.write cups curr x
            put x
    V.freeze cups

day23a :: String -> String
day23a s = soln $ runST $ evalStateT (run 100 order) (V.head order) where
    soln cups = map intToDigit . takeWhile (/=1) . tail $ iterate (cups V.!) 1
    order = input s

day23b :: String -> String
day23b s = soln $ runST $ evalStateT (run 10000000 order) (V.head order) where
    soln cups = show $ (cups V.! 1) * (cups V.! (cups V.! 1))
    order = let v = input s
                l = V.length v
            in  v V.++ V.enumFromN (l + 1) (1000000 - l)
