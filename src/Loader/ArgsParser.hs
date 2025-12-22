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

insertOptionsParser :: Parser InsertOptions
insertOptionsParser =
  InsertOptions
    <$> modeParser
    <*> switch
          (  long "dry-run"
          <> short 'n'
          <> help "Proof parsing"
          )

actionParser :: Parser Action
actionParser =
  hsubparser
    ( command "insert"
        (info
          (Insert <$> insertOptionsParser)
          (progDesc "Insert books")
        )
   <> command "delete"
        (info
          (Delete <$> argument str (metavar "NAME"))
          (progDesc "Delete book by name")
        )
    )

optsInfo :: ParserInfo Action
optsInfo =
  info (actionParser <**> helper)
    (  fullDesc
    <> progDesc "Load books into DB"
    <> header   "Loader — books loader utility"
    )