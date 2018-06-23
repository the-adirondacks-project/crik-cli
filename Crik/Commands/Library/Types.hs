{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Crik.Commands.Library.Types
(
  LibraryCreateOptions(..)
, LibraryInfoOptions(..)
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

data LibraryInfoOptions =
  LibraryInfoOptions {
    libraryName :: Text --TODO: We want to be able to use id or name
  }
  deriving (Show)

data LibrarySubCommand =
  LibraryCreate {
    libraryCreateOptions :: LibraryCreateOptions
  } |
  LibraryList {
    libraryListOptions :: LibraryListOptions
  } |
  LibraryInfo {
    libraryInfoOptions :: LibraryInfoOptions
  }
  deriving (Show)
