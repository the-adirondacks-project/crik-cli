{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}

module Crik.CrudCommands
(
  CrudCommand(..)
, CrudSubCommand(..)
, crudCommandParser
) where

import Data.Semigroup ((<>))
import Options.Applicative

import Crik.Types
import Crik.Types.Video
import Crik.Types.VideoFile
import Crik.Types.VideoLibrary

data CrudCommand =
  VideoCommand (CrudSubCommand (Video NoId) VideoId) |
  FileCommand (CrudSubCommand (VideoFile NoId) VideoFileId) |
  LibraryCommand (CrudSubCommand (VideoLibrary NoId) VideoLibraryId)
  deriving (Show)

data CrudSubCommand item id = Get id | GetAll | Create item | Update item deriving (Show)

crudCommandParser :: Parser CrudCommand
crudCommandParser =
  subparser (
    command "files" (info
      (FileCommand <$> crudSubCommandParser "file" addFileParser)
      (progDesc "Gets all files")
    ) <>
    command "videos" (info
      (VideoCommand <$> crudSubCommandParser "video" addVideoParser)
      (progDesc "Foo")
    ) <>
    command "libraries" (info
      (LibraryCommand <$> crudSubCommandParser "library" addLibraryParser)
      (progDesc "Gets all libraries"))
  )

crudSubCommandParser :: String -> (Parser item) -> Parser (CrudSubCommand item id)
crudSubCommandParser typeName addFunction = subparser (
    command "list" (info (pure GetAll) (progDesc ("Lists all" ++ typeName))) <>
    command "add" (info (Create <$> addFunction) (progDesc "Adds a video"))
  ) <|> (pure GetAll)

addFileParser :: Parser (VideoFile NoId)
addFileParser = do
  videoId <- option auto $
    long "video" <>
    metavar "VIDEO ID" <>
    help "Id for associated video"

  url <- option str $
    long "url" <>
    metavar "<file-url>" <>
    help "URL to access this file"

  libraryId <- option auto $
    long "library" <>
    metavar "<library-id>" <>
    help "Id for associated video"

  storageId <- option str $
    long "storage-id" <>
    metavar "<storage-id>" <>
    help "Id for associated video"

  pure $
    VideoFile
    NoId
    (VideoId videoId)
    url
    (VideoLibraryId libraryId)
    (VideoFileStorageId storageId)

addLibraryParser :: Parser (VideoLibrary NoId)
addLibraryParser = do
  url <- strOption $
    long "url" <>
    metavar "<url>" <>
    help "URL for library"

  pure $
    VideoLibrary
    NoId
    url

addVideoParser :: Parser (Video NoId)
addVideoParser = do
  name <- strOption $
    long "name" <>
    metavar "<name>" <>
    help "Name for video"

  pure $
    Video
    NoId
    name
