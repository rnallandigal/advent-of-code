module Command (Command (..), readCommand) where

import Options.Applicative

data Command = Problem (Int, Int, Int)
             | All

readCommand :: IO Command
readCommand = execParser $ info
    (commandP <**> helper)
     ( fullDesc
    <> progDesc "Run advent of code problems"
    <> header "Advent of Code Runner" )

commandP :: Parser Command
commandP = problemP <|> allP

problemP :: Parser Command
problemP = fmap Problem $ (,,) <$> yearP <*> dayP <*> partP

allP :: Parser Command
allP = All <$ switch
     ( short 'a'
    <> long "all"
    <> help "Run all problems")

yearP :: Parser Int
yearP = option auto $
       short 'y'
    <> long "year"
    <> help "the year number (ex. 2018) of the problem to run"
    <> metavar "YEAR"

dayP :: Parser Int
dayP = option auto $
       short 'd'
    <> long "day"
    <> help "the day number (1 - 25) of the problem to run"
    <> metavar "DAY"

partP :: Parser Int
partP = option auto $
       short 'p'
    <> long "part"
    <> help "the part (1, 2) of the problem to run"
    <> metavar "PART"
