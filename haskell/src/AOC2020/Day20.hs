{-# LANGUAGE DeriveGeneric #-}

module AOC2020.Day20 (day20a, day20b) where

import           Data.Bifunctor
import           Data.List (foldl')
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Vector as V
import           GHC.Generics (Generic)
import           Linear.V2
import           Prelude hiding (Left, Right)

import           Lib (bounds, indexed, isqrt, pShow)

type Point         = V2 Int
type Tile          = M.Map Point Char
type TileNum       = Int
type Transform     = Int
type Jigsaw        = M.Map TileNum Tile
type BorderHash    = Int
type Border        = Int
type Orientation   = (TileNum, Transform)

data TransformType = Original | FlipX | FlipY | Rotate180
                   | FlipDescDiag | RotateLeft | RotateRight | FlipAscDiag
                   deriving (Eq, Ord, Show, Generic, Enum)

data BorderType    = Top | Bottom | FlippedTop | FlippedBottom
                   | Left | FlippedLeft | Right | FlippedRight
                   deriving (Eq, Ord, Show, Generic, Enum)

input :: String -> Jigsaw
input = M.fromList . map (camera . lines) . splitOn "\n\n" where
    camera (x:xs) = (tilenum x, tile xs)
    tilenum = read . drop 5 . init
    tile = M.fromList . concat . zipWith (\y -> zipWith (pt y) [0..]) [0..]
    pt y x c = (V2 x y, c)

transforms :: Int -> V.Vector (Point -> Point)
transforms m = V.fromList               -- transform    -> border
    [ \(V2 x y) -> V2 (    x) (    y)   -- original     -> top
    , \(V2 x y) -> V2 (    x) (m - y)   -- flip x-axis  -> bottom
    , \(V2 x y) -> V2 (m - x) (    y)   -- flip y-axis  -> flipped top
    , \(V2 x y) -> V2 (m - x) (m - y)   -- rotate 180   -> flipped bottom
    , \(V2 x y) -> V2 (    y) (    x)   -- flip y = -x  -> left
    , \(V2 x y) -> V2 (    y) (m - x)   -- rotate left  -> flipped left, right
    , \(V2 x y) -> V2 (m - y) (    x)   -- rotate right -> right, flipped left
    , \(V2 x y) -> V2 (m - y) (m - x) ] -- flip y = x   -> flipped right

invT :: V.Vector Transform
invT = V.fromList [ 0, 1, 2, 3, 4, 6, 5, 7 ]

composeT :: V.Vector (V.Vector Transform)
composeT = V.fromList $ map V.fromList
    [ [ 0, 1, 2, 3, 4, 5, 6, 7 ]
    , [ 1, 0, 3, 2, 6, 7, 4, 5 ]
    , [ 2, 3, 0, 1, 5, 4, 7, 6 ]
    , [ 3, 2, 1, 0, 7, 6, 5, 4 ]
    , [ 4, 5, 6, 7, 0, 1, 2, 3 ]
    , [ 5, 4, 7, 6, 2, 3, 0, 1 ]
    , [ 6, 7, 4, 5, 1, 0, 3, 2 ]
    , [ 7, 6, 5, 4, 3, 2, 1, 0 ] ]

tileBorders :: Tile -> V.Vector BorderHash
tileBorders tile = V.map (serialize . border) $ transforms 9 where
    border f = [ tile M.! (f $ V2 x 0) | x <- [0,1..9] ]
    serialize = foldr (\x acc -> (fromEnum $ x == '#') + acc * 2) 0

borderMap :: M.Map TileNum (V.Vector BorderHash)
          -> M.Map BorderHash (V.Vector Orientation)
borderMap = M.foldrWithKey invert M.empty where
    invert i = flip $ V.ifoldr (\t b -> M.insertWith (<>) b (pure (i, t)))

graph :: Jigsaw -> (M.Map Orientation Orientation, M.Map TileNum (S.Set Border))
graph = second corners
    . M.foldrWithKey neighbors (pure M.empty)
    . borderMap
    . M.map tileBorders
    where
        neighbors b ts = case V.length ts of
            1 -> let (i, t) = V.head ts
                 in  second (M.alter (Just . S.insert t . fromMaybe (S.empty)) i)
            2 -> let (u, v) = (V.head ts, V.last ts)
                 in  first (\es -> foldr (uncurry M.insert) es [(u, v), (v, u)])
        corners = M.filter ((==4) . S.size)

orient :: S.Set Border -> Transform
orient s | border Left Top     = fromEnum Original
         | border Top Right    = fromEnum RotateLeft
         | border Right Bottom = fromEnum FlipAscDiag
         | border Bottom Left  = fromEnum RotateRight
         where border a b = all ((`S.member` s) . fromEnum) [a, b]

arrange :: Jigsaw -> M.Map Point Orientation
arrange jigsaw = foldl' place grid (sequence $ pure [0..(d - 1)]) where
    (edges, corners) = graph jigsaw
    grid = M.singleton (pure 0) . second orient . head $ M.toList corners
    place :: M.Map Point Orientation -> Point -> M.Map Point Orientation
    place m (V2 0 0) = m
    place m p        = let ((i, t), (b, b')) = prev p m
                           bt = (composeT V.! (fromEnum b)) V.! (invT V.! t)
                           (i', bt') = edges M.! (i, bt)
                           t' = (composeT V.! (invT V.! bt')) V.! (fromEnum b')
                       in  M.insert p (i', t') m
    prev (V2 x 0) m = (m M.! (V2 (x - 1) 0), (Right, Left))
    prev (V2 x y) m = (m M.! (V2 x (y - 1)), (Bottom, Top))
    d = isqrt $ M.size jigsaw

assemble :: Jigsaw -> S.Set Point
assemble jigsaw = toSet . M.foldrWithKey outer M.empty $ arrange jigsaw where
    outer p (i, t) acc = M.foldrWithKey inner acc tile where
        inner p' = M.insert (8 * p + p')
        tile = removeBorder $ M.mapKeys ((transforms 9) V.! t) $ jigsaw M.! i
    removeBorder = M.mapKeys (subtract 1) . M.filterWithKey interior where
        interior p _ = minimum p > 0 && maximum p < 9
    toSet = S.fromList . M.keys . M.filter (=='#')

monster :: S.Set Point
monster = S.fromList . map fst . filter ((=='#') . snd) $ indexed
    [ "                  # "
    , "#    ##    ##    ###"
    , " #  #  #  #  #  #   " ]

roughness :: S.Set Point -> S.Set Point -> Int
roughness haystack needle = minimum $ V.map (S.size . match) ps where
    ps      = V.map (`S.map` needle) $ transforms m
    [n, m]  = map ((+1) . maximum . snd . bounds) [haystack, needle]
    match p = foldr check haystack $ sequence (pure [1-m..n+m-1]) where
        check offset tile
            | p' `S.isSubsetOf` tile = tile S.\\ p'
            | otherwise              = tile
            where p' = S.mapMonotonic (+offset) p

day20a :: String -> String
day20a = show . product . M.keys . snd . graph . input

day20b :: String -> String
day20b = show . flip roughness monster . assemble . input
