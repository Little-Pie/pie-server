{-# LANGUAGE OverloadedStrings #-}

module Logging where

import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), Value(String))

data LoggingLevel = Debug | Release | Warning | Error

instance FromJSON LoggingLevel where
  parseJSON (String loggingLevel) = pure $ case loggingLevel of
    "Debug" -> Debug
    "Release" -> Release
    "Warning" -> Warning
    "Error" -> Error
    _ -> mzero
  parseJSON _ = mzero
