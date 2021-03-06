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

import Crik.TH.DeriveWrapped
import Crik.Types
import Crik.Types.Video
import Crik.Types.File
import Crik.Types.Library

deriveWrappedRead ''VideoId
deriveWrappedRead ''FileId
deriveWrappedRead ''FileStorageId
deriveWrappedRead ''LibraryId

data CrudCommand =
  VideoCrudCommand (CrudSubCommand (Video NoId) VideoId) |
  FileCrudCommand (CrudSubCommand (File NoId) FileId) |
  LibraryCrudCommand (CrudSubCommand (Library NoId) LibraryId)
  deriving (Show)

data (Read id) => CrudSubCommand item id =
  Get id |
  GetAll |
  Create item |
  Update item |
  Delete id
  deriving (Show)

crudCommandParser :: Parser CrudCommand
crudCommandParser =
  subparser (
    (crudCommandParserHelper FileCrudCommand "file" "files" addFileParser deleteFileParser) <>
    (crudCommandParserHelper LibraryCrudCommand "library" "libraries" addLibraryParser undefined) <>
    (crudCommandParserHelper VideoCrudCommand "video" "videos" addVideoParser undefined)
  )
  where
    crudCommandParserHelper :: (Read id) =>
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

crudSubCommandParser :: (Read id) =>
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
  ) <|> (Get <$> (getSingleParser typeName <**> helper)) <|> (pure GetAll)

getSingleParser :: (Read id) => String -> Parser (id)
getSingleParser typeName = do
  id <- option auto $
    long "id" <>
    metavar "<id>" <>
    help ("Show an individual " ++ typeName)

  pure id

addFileParser :: Parser (File NoId)
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

  storageId <- option auto $
    long "storage-id" <>
    metavar "<storage-id>" <>
    help "Id for associated video"

  pure $ File NoId videoId url libraryId storageId

addLibraryParser :: Parser (Library NoId)
addLibraryParser = do
  url <- strOption $
    long "url" <>
    metavar "<url>" <>
    help "URL for library"

  name <- strOption $
    long "name" <>
    metavar "<name>" <>
    help "name for library"

  pure $ Library NoId url name

addVideoParser :: Parser (Video NoId)
addVideoParser = do
  name <- strOption $
    long "name" <>
    metavar "<name>" <>
    help "Name for video"

  pure $ Video NoId name

deleteFileParser :: Parser (FileId)
deleteFileParser = do
  id <- option auto $
    long "id" <>
    metavar "<id>" <>
    help "Id of file to delete"

  pure id
