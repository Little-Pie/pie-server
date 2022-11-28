{-# LANGUAGE OverloadedStrings #-}

module Authorization where

import Config (App)
import qualified Data.ByteString.Base64 as BASE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import DbQuery.User (getUserByLogin)
import Hash (makeStringHash)
import Helpers (responseUnauthorized)
import Network.Wai (Response)
import Types.Entities.User (User (..))

authorize :: Maybe BS.ByteString -> App (LBS.ByteString, Maybe User)
authorize mbBase64LoginAndPassword = case mbBase64LoginAndPassword of
  Nothing -> pure ("Found no header for Authorization", Nothing)
  Just base64LoginAndPassword -> case BASE.decode base64LoginAndPassword of
    Left err -> pure (LBSC.pack err, Nothing)
    Right loginPassword -> do
      let login' = BS.takeWhile (/= ':') loginPassword
      let password' = BS.drop 1 $ BS.dropWhile (/= ':') loginPassword
      users <- getUserByLogin (BS.unpack login')
      case users of
        [] -> pure ("Wrong login", Nothing)
        [user] -> do
          let loginPassword' = BS.pack $ login user ++ ":" ++ password user
          if login' <> ":" <> BS.pack (makeStringHash $ BS.unpack password') == loginPassword'
            then pure ("User is authorized", Just user)
            else pure ("Wrong password", Nothing)
        _ -> pure ("Something went wrong", Nothing)

withAuthorization :: Maybe BS.ByteString -> (User -> App Response) -> App Response
withAuthorization mbBase64LoginAndPassword f = do
  (str, mbUser) <- authorize mbBase64LoginAndPassword
  case mbUser of
    Nothing -> responseUnauthorized str
    Just authorizedUser -> f authorizedUser
