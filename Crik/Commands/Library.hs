module Crik.Commands.Library
(
  libraryCommandParser
) where

import Data.Semigroup ((<>))
import Options.Applicative (Parser, subparser)

import Crik.Commands.Library.Create
import Crik.Commands.Library.Info
import Crik.Commands.Library.List
import Crik.Commands.Library.Types

libraryCommandParser :: Parser (LibrarySubCommand)
libraryCommandParser =
  subparser (
    libraryCreateCommandParser <>
    libraryListCommandParser <>
    libraryInfoCommandParser
  )
