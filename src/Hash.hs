module Hash where

import Crypto.Hash (Digest, Tiger, hash)
import qualified Data.ByteString.Char8 as BS

makeStringHash :: String -> String
makeStringHash password = show hashedPassword
  where
    hashedPassword = hash $ BS.pack password :: Digest Tiger
