{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Network.Wai
import Network.HTTP.Types (status200)

application _ respond = respond $ responseLBS status200 [("Content-Type", "text/plain")]
                       "Hello World"