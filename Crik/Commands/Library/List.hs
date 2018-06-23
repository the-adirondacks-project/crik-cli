{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}

module Crik.Commands.Library.List
(
  libraryListCommandParser
) where

import Data.Semigroup ((<>))
import Options.Applicative

import Crik.Commands.Library.Types

libraryListCommandParser :: Mod CommandFields LibrarySubCommand
libraryListCommandParser =
  command "library-list" (info (LibraryList <$> libraryListParser)
    (progDesc ("Lists all libraries")))

libraryListParser :: Parser (LibraryListOptions)
libraryListParser = pure LibraryListOptions
