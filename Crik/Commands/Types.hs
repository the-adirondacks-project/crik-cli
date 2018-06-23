module Crik.Commands.Types
(
  Command(..)
) where

import Crik.CrudCommands
import Crik.Commands.Library.Types

data Command =
  CrudCommand {
    crudCommand :: CrudCommand
  } |
  LibraryCommand {
    libraryCommand :: LibrarySubCommand
  } deriving (Show)