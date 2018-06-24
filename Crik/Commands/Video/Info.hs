{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}

module Crik.Commands.Video.Info
(
  videoInfoCommandParser
) where

import Control.Applicative ((<|>))
import Data.Semigroup ((<>))
import Options.Applicative

import Crik.Commands.Video.Types

videoInfoCommandParser :: Mod CommandFields VideoSubCommand
videoInfoCommandParser =
  command "video-info" (info (VideoInfo <$> videoInfoParser)
    (progDesc ("Info about a video")))

videoInfoParser :: Parser (VideoInfoOptions)
videoInfoParser = do
  name <-
    (strOption $
      long "name" <>
      metavar "<name>" <>
      help "video name") <|>
    (strArgument $
      metavar "<name>" <>
      help "video name")

  return $ VideoInfoOptions name
