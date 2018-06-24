module Crik.Commands.Types
(
  Command(..)
) where

import Crik.CrudCommands
import Crik.Commands.Library.Types
import Crik.Commands.Video.Types

data Command =
  CrudCommand CrudCommand |
  LibraryCommand LibrarySubCommand |
  VideoCommand VideoSubCommand
  deriving (Show)
