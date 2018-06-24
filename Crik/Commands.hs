module Crik.Commands
(
  commandsParser
) where

import Data.Semigroup ((<>))
import Options.Applicative

import Crik.CrudCommands (CrudCommand, crudCommandParser)
import Crik.Commands.Library
import Crik.Commands.Video
import Crik.Commands.Types

commandsParser :: ParserInfo (Command)
commandsParser =
  info (((CrudCommand <$> crudCommandParser) <|>
        (LibraryCommand <$> libraryCommandParser) <|>
        (VideoCommand <$> videoCommandParser)) <**> helper)
    (fullDesc <> progDesc "A program that does things" <> header "program - a thing") -- <>
  --info (Command <$> libraryCommandParser <**>) (fullDesc <> progDesc "Foo" <> header "bar")
