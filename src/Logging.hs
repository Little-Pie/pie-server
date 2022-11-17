{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Logging where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data LoggingLevel = Debug | Release | Warning | Error
  deriving (Eq, Ord, Generic, FromJSON)
