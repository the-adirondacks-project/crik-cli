{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Crik.Commands.Video.Types
(
  VideoInfoOptions(..)
, VideoSubCommand(..)
) where

import Data.Text (Text)

import Crik.Types.Video

data VideoInfoOptions =
  VideoInfoOptions {
    videoName :: Text --TODO: We want to be able to use id or name
  }
  deriving (Show)

data VideoSubCommand =
  VideoInfo VideoInfoOptions
  deriving (Show)
