{-# LANGUAGE TemplateHaskell #-}

module Crik.TH.DeriveWrappedRead
(
  deriveWrappedRead
) where

import GHC.Read (Read, readPrec)
import Language.Haskell.TH (Dec, Name, Q, conT, conE, lookupValueName, nameBase)

deriveWrappedRead :: Name -> Q [Dec]
deriveWrappedRead name = [d|
  instance Read $a where
    readPrec = readPrec >>= (return . $b)
  |]
   where
     a = conT name
     b = do
      Just valueName <- lookupValueName (nameBase name)
      (conE valueName)
