{-# LANGUAGE OverloadedStrings #-}

module Crik.Commands.Library.Types
(
  LibraryCreateOptions(..)
, LibraryListOptions(..)
, LibrarySubCommand(..)
, LibraryType(..)
) where

import Data.Text (Text)
import Text.ParserCombinators.ReadP (look, pfail)
import GHC.Read (Read, readPrec)

import Crik.Types.Library

data LibraryCreateOptions =
  LibraryCreateOptions {
    libraryName :: Text,
    libraryType :: LibraryType,
    libraryLocation :: Text
  } deriving (Show)

data LibraryListOptions =
  LibraryListOptions
  deriving (Show)

data LibrarySubCommand =
  LibraryCreate {
    libraryCreateOptions :: LibraryCreateOptions
  } |
  LibraryList {
    libraryListOptions :: LibraryListOptions
  }
  deriving (Show)
