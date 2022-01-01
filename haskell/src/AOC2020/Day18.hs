module AOC2020.Day18 (day18a, day18b) where

import Text.Parsec

data Exp = Atom Int | Mul Exp Exp | Add Exp Exp deriving Show

eval :: Exp -> Int
eval (Atom x)  = x
eval (Mul x y) = (eval x) * (eval y)
eval (Add x y) = (eval x) + (eval y)

mulP  = char '*' *> pure Mul
addP  = char '+' *> pure Add
atomP = (Atom . read) <$> (many1 digit)

day18a :: String -> String
day18a = show . sum . map (eval . parseExp . filter (/=' ')) . lines where
    parseExp = (\(Right exp) -> exp) . parse expP ""
    expP     = termP `chainl1` (mulP <|> addP)
    termP    = subexpP <|> atomP
    subexpP  = char '(' *> expP <* char ')'

day18b :: String -> String
day18b = show . sum . map (eval . parseExp . filter (/=' ')) . lines where
    parseExp = (\(Right exp) -> exp) . parse expP ""
    expP     = termP `chainl1` mulP
    termP    = factorP `chainl1` addP
    factorP  = subexpP <|> atomP
    subexpP  = char '(' *> expP <* char ')'
