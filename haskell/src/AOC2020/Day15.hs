module AOC2020.Day15 (day15a, day15b) where

import           Control.Monad.Loops (whileM_)
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.List.Split (splitOn)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import           GHC.Int (Int32)

input :: String -> V.Vector Int
input = V.fromList . map read . splitOn ","

data GameState = GameState !Int32 !Int32

game :: Int32 -> V.Vector Int -> GameState
game turns nums = runST $ flip execStateT (GameState 0 0) $ do
    spoken <- MV.new (fromIntegral turns)
    V.iforM_ nums $ \i val -> MV.write spoken val (fromIntegral $ i + 1)
    put $ GameState (fromIntegral $ V.length nums) (fromIntegral $ V.last nums)
    whileM_ (gets $ \(GameState i _) -> i < turns) $ do
        GameState i mostRecentlySpoken <- get
        lastSpoken <- MV.unsafeExchange spoken (fromIntegral mostRecentlySpoken) i
        let nextSpoken | lastSpoken == 0 = 0
                       | otherwise       = i - lastSpoken
        put $ GameState (i + 1) nextSpoken

day15a :: String -> String
day15a = show . (\(GameState _ x) -> x) . game 2020 . input

day15b :: String -> String
day15b = show . (\(GameState _ x) -> x) . game 30000000 . input
