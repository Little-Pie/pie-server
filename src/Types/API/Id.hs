{-# LANGUAGE OverloadedStrings #-}

module Types.API.Id where

import Data.Aeson 

data IdRequest = IdRequest {id :: Int}

instance FromJSON IdRequest where
  parseJSON (Object idRequest) = IdRequest <$> idRequest .: "id"