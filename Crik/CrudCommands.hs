module Crik.CrudCommands
(
  CrudCommand(..)
, CrudSubCommand(..)
, crudCommandParser
) where

import Data.Semigroup ((<>))
import Options.Applicative (Parser, (<|>), subparser, command, info, progDesc, strOption, metavar, long, help)

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
    command "videos" (info (VideoCommand <$> videosParser) (progDesc "Foo")) <>
    command "files" (info (pure $ FileCommand GetAll) (progDesc "Gets all files")) <>
    command "libraries" (info (pure $ LibraryCommand GetAll) (progDesc "Gets all files"))
  )

videosParser :: Parser (CrudSubCommand (Video NoId) VideoId)
videosParser = subparser (
    command "list" (info (pure GetAll) (progDesc "Lists all videos")) <>
    command "add" (info (Create <$> addVideoParser) (progDesc "Adds a video"))
  ) <|> (pure GetAll)

addVideoParser :: Parser (Video NoId)
addVideoParser = Video NoId <$> strOption (
    long "name" <>
    metavar "NAME" <>
    help "Name for video"
  )
