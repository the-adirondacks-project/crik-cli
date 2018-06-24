module Crik.Commands.Video
(
  videoCommandParser
) where

import Data.Semigroup ((<>))
import Options.Applicative (Parser, subparser)

import Crik.Commands.Video.Info
import Crik.Commands.Video.Types

videoCommandParser :: Parser (VideoSubCommand)
videoCommandParser =
  subparser (
    videoInfoCommandParser
  )
