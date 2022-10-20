{-# LANGUAGE OverloadedStrings #-}

module Logging where

import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), Value(String))

data LoggingLevel = Debug | Release | Warning | Error
  deriving (Eq, Ord)

instance FromJSON LoggingLevel where
  parseJSON (String loggingLevel) = case loggingLevel of
    "Debug" -> pure Debug
    "Release" -> pure Release
    "Warning" -> pure Warning
    "Error" -> pure Error
    _ -> mzero
  parseJSON _ = mzero
