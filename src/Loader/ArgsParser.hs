module Loader.ArgsParser where

import           Options.Applicative

import           Loader.Types

modeParser :: Parser Mode
modeParser =
  (File <$> strOption
      (  long "file"
      <> short 'f'
      <> metavar "PATH"
      <> help "File with books"
      )
  )
  <|> pure Interactive

optionsParser :: Parser Options
optionsParser =
  Options
    <$> modeParser
    <*> switch
          (  long "dry-run"
          <> short 'n'
          <> help "Proof parsing"
          )

optsInfo :: ParserInfo Options
optsInfo =
  info (optionsParser <**> helper)
    (  fullDesc
    <> progDesc "Load books into DB"
    <> header   "Loader — books loader utility"
    )