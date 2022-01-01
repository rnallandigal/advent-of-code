module AOC2018.Day04 (day04a, day04b) where

import           Data.Function (on)
import           Data.Functor.Identity
import           Data.List (maximumBy, sort)
import qualified Data.Map.Strict as M
import           Text.Parsec

import           Lib (freqMap)

type Parser = ParsecT String () Identity

data Action = Guard Int | Asleep | Wakeup
    deriving (Show)

parseInput :: String -> [(Int, Action)]
parseInput s = case parse (lineP `sepEndBy` char '\n') "" s of
    Left err -> error $ show err
    Right xs -> xs

lineP :: Parser (Int, Action)
lineP = (,)
    <$> (read <$> (count 15 anyChar *> count 2 digit <* count 2 anyChar))
    <*> actionP

actionP :: Parser Action
actionP =
        Guard . read <$> (string "Guard #" *> many1 digit <* string " begins shift")
    <|> Asleep <$ string "falls asleep"
    <|> Wakeup <$ string "wakes up"

recordMinutes :: [(Int, Action)] -> M.Map Int [Int]
recordMinutes = rec M.empty (-1) (-1) where
    rec m _ _ []                = m
    rec m _ _ ((_, Guard g):xs) = rec m g (-1) xs
    rec m g _ ((a, Asleep):xs)  = rec m g a xs
    rec m g a ((w, Wakeup):xs)  = rec (M.insertWith (++) g [a..w-1] m) g (-1) xs

input :: String -> M.Map Int (M.Map Int Int)
input = M.map freqMap . recordMinutes . parseInput . unlines . sort . lines

day04a :: String -> String
day04a = show
    . (\(g, ms) -> g * (fst . maximumBy (compare `on` snd) . M.toList $ ms))
    . maximumBy (compare `on` (sum . snd))
    . M.toList
    . input

day04b :: String -> String
day04b = show
    . (\(g, (m, _)) -> g * m)
    . maximumBy (compare `on` (snd . snd))
    . M.toList
    . M.map (maximumBy (compare `on` snd) . M.toList)
    . input
