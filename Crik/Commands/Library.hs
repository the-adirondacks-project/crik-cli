module Crik.Commands.Library
(
  libraryCommandParser
) where

import Options.Applicative (Parser, subparser)

import Crik.Commands.Library.Create
import Crik.Commands.Library.Types

libraryCommandParser :: Parser (LibrarySubCommand)
libraryCommandParser =
  subparser (
    libraryCreateCommandParser
  )
