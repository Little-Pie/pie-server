{-# LANGUAGE OverloadedStrings #-}

module Config where

import Logging (LoggingLevel)
import qualified Data.ByteString.Char8 as BS
import Data.Aeson (Value(..), FromJSON, decodeStrict, parseJSON, (.:))
import Control.Monad (mzero)

data Config = Config {limit :: Int
                     ,offset :: Int
                     ,connectHost :: String
                     ,connectDatabase :: String
                     ,connectUser :: String
                     ,connectPassword :: String
                     ,loggingLevel :: LoggingLevel
                     }

instance FromJSON Config where
  parseJSON (Object config) = Config <$>
                              config .: "limit" <*>
                              config .: "offset" <*>
                              config .: "connectHost" <*>
                              config .: "connectDatabase" <*>
                              config .: "connectUser" <*>
                              config .: "connectPassword" <*>
                              config .: "loggingLevel"
  parseJSON _ = mzero

getConfig :: IO (Maybe Config)
getConfig = do
  rawJSON <- BS.readFile "config.json"
  let result = decodeStrict rawJSON :: Maybe Config
  case result of
    Nothing -> return Nothing
    Just conf -> return $ Just conf