{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Crik.CrudCommands
(
  CrudCommand(..)
, CrudSubCommand(..)
, crudCommandParser
) where

import Data.Semigroup ((<>))
import Options.Applicative

import Crik.TH.DeriveWrappedRead
import Crik.Types
import Crik.Types.Video
import Crik.Types.VideoFile
import Crik.Types.VideoLibrary

data CrudCommand =
  VideoCommand (CrudSubCommand (Video NoId) VideoId) |
  FileCommand (CrudSubCommand (VideoFile NoId) VideoFileId) |
  LibraryCommand (CrudSubCommand (VideoLibrary NoId) VideoLibraryId)
  deriving (Show)

data CrudSubCommand item id =
  Get id |
  GetAll |
  Create item |
  Update item |
  Delete id
  deriving (Show)

deriveWrappedRead ''VideoId
deriveWrappedRead ''VideoFileId
deriveWrappedRead ''VideoLibraryId

crudCommandParser :: Parser CrudCommand
crudCommandParser =
  subparser (
    (crudCommandParserHelper FileCommand "file" "files" addFileParser deleteFileParser) <>
    (crudCommandParserHelper LibraryCommand "library" "libraries" addLibraryParser undefined) <>
    (crudCommandParserHelper VideoCommand "video" "videos" addVideoParser undefined)
  )
  where
    crudCommandParserHelper ::
      (CrudSubCommand item id -> CrudCommand) ->
      String ->
      String ->
      (Parser item) ->
      (Parser id) ->
      Mod CommandFields CrudCommand
    crudCommandParserHelper crudCommand typeName typeNamePlural addFunction deleteFunction =
      command typeNamePlural (info
        (crudCommand <$>
          crudSubCommandParser typeName typeNamePlural addFunction deleteFunction <**> helper)
        (progDesc $ "List, create, update, or delete " ++ typeNamePlural)
      )

crudSubCommandParser ::
  String ->
  String ->
  (Parser item) ->
  (Parser id) ->
  Parser (CrudSubCommand item id)
crudSubCommandParser typeName typeNamePlural addFunction deleteFunction = subparser
  (
    command "list" (info (pure GetAll <**> helper) (progDesc ("Lists all " ++ typeNamePlural))) <>
    command "add" (info (Create <$> addFunction <**> helper) (progDesc ("Adds a new " ++ typeName))) <>
    command "remove" (info
      (Delete <$> deleteFunction <**> helper)
      (progDesc $ "Deletes a " ++ typeName)
    )
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

deleteFileParser :: Parser (VideoFileId)
deleteFileParser = do
  id <- option auto $
    long "id" <>
    metavar "<id>" <>
    help "Id of file to delete"

  pure (VideoFileId id)
