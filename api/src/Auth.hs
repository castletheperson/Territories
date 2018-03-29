{-# LANGUAGE OverloadedStrings #-}

module Auth (withAuth) where

import Types

import Snap.Core
import Network.Wreq
import Crypto.JWT
import Control.Lens
import Control.Monad ((>=>))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(parseJSON))
import Data.Aeson.Lens (key, values, _String)

import Data.Text (Text, pack)
import Data.Text.Lens (packed)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString (ByteString, stripPrefix)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64.URL as B64

newtype Auth0JWKSet = Auth0JWKSet { auth0JWKSet :: JWKSet }

instance FromJSON Auth0JWKSet where
    parseJSON = fmap Auth0JWKSet . parseJSON . fixJWKSet
      where
        fixJWKSet = key "keys" . values . key "x5t" . _String %~ fixX5T
        fixX5T = decodeUtf8 . B64.encode . fst . B16.decode . B64.decodeLenient . encodeUtf8

jwksURL = "https://territories.auth0.com/.well-known/jwks.json"
jwtAudience = "https://jw-maps.com/api"

withAuth :: (User -> Snap a) -> Snap a
withAuth action = do
    authHeader <- getHeader "Authorization" <$> getRequest
    compactJWT <- case authHeader >>= stripPrefix "Bearer " of
        Just token -> return (fromStrict token)
        Nothing -> reject 400 "Bad Request" "Missing authorization header"

    Auth0JWKSet jwks <- liftIO $ view responseBody <$> (asJSON =<< get jwksURL)
    claims <- liftIO . runExceptT $ do
        jwt <- decodeCompact compactJWT
        let settings = defaultJWTValidationSettings (== jwtAudience)
        verifyClaims settings jwks jwt

    case claims of
        Right claims -> case claims ^? claimSub . _Just . string . packed of
            Just userId -> action (User { userId = userId })
            Nothing -> reject 400 "Bad Request" "No user id was provided"
        Left err -> reject 401 "Unauthorized" (show (err :: JWTError))

reject :: Int -> ByteString -> String -> Snap a
reject code status err = do
    modifyResponse $ setResponseStatus code status
    writeText (pack err)
    finishWith =<< getResponse