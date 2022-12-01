module Command (Command (..), readCommand) where

import Options.Applicative

data Command = List
             | Run String

readCommand :: IO Command
readCommand = execParser $ info
    (commandP <**> helper)
     ( fullDesc
    <> progDesc "Run advent of code problems"
    <> header "Advent of Code Runner" )

commandP :: Parser Command
commandP = listP <|> runP

listP :: Parser Command
listP = List <$ switch
     ( short 'l'
    <> long "list"
    <> help "list all available problems" )

runP :: Parser Command
runP = fmap Run $ argument str $
       value ""
    <> showDefault
    <> help "select the problems to run using a regular expression, i.e. \"2022.0[123]\""
    <> metavar "FILTER"
