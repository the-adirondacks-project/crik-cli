{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}

module Crik.Commands.Library.Create
(
  libraryCreateCommandParser
) where

import Data.Semigroup ((<>))
import Options.Applicative

import Crik.Commands.Library.Types

libraryCreateCommandParser :: Mod CommandFields LibrarySubCommand
libraryCreateCommandParser =
  command "library-create" (info (LibraryCreate <$> libraryCreateParser)
    (progDesc ("Creates a new library")))

libraryCreateParser :: Parser (LibraryCreateOptions)
libraryCreateParser = do
  name <- strOption $
    long "name" <>
    metavar "<name>" <>
    help "Name for new library"

  libraryType <- strOption $
    long "type" <>
    metavar "<type>" <>
    help "Type of library. Either 'http' or 'directory'"

  location <- strOption $
    long "location" <>
    metavar "<location>" <>
    help ("Location of the library. For 'http' libraries provide a base url. For 'directory'"
      <> " libraries provide the directory.")

  return $ LibraryCreateOptions name libraryType location
