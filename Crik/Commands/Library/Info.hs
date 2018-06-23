{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}

module Crik.Commands.Library.Info
(
  libraryInfoCommandParser
) where

import Control.Applicative ((<|>))
import Data.Semigroup ((<>))
import Options.Applicative

import Crik.Commands.Library.Types

libraryInfoCommandParser :: Mod CommandFields LibrarySubCommand
libraryInfoCommandParser =
  command "library-info" (info (LibraryInfo <$> libraryInfoParser)
    (progDesc ("Info about a library")))

libraryInfoParser :: Parser (LibraryInfoOptions)
libraryInfoParser = do
  name <-
    (strOption $
      long "name" <>
      metavar "<name>" <>
      help "library name") <|>
    (strArgument $
      metavar "<name>" <>
      help "library name")

  return $ LibraryInfoOptions name
