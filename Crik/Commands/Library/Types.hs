{-# LANGUAGE OverloadedStrings #-}

module Crik.Commands.Library.Types
(
  LibrarySubCommand(..)
, LibraryCreateOptions(..)
, LibraryType(..)
) where

import Data.String (IsString(..))
import Data.Text (Text)
import Text.ParserCombinators.ReadP (look, pfail)
import GHC.Read (Read, readPrec)

data LibraryType =
  HTTP |
  Directory
  deriving (Show)

instance IsString LibraryType where
  fromString text = case text of
    "http" -> HTTP
    "directory" -> Directory

data LibraryCreateOptions =
  LibraryCreateOptions {
    libraryName :: Text,
    libraryType :: LibraryType,
    libraryLocation :: Text
  } deriving (Show)

data LibrarySubCommand =
  LibraryCreate {
    libraryCreateOptions :: LibraryCreateOptions
  }
  deriving (Show)
