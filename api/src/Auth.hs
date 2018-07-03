{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Auth (withAuth) where

import Types
import Database (getUserByName)

import Snap.Core
import Network.Wreq
import Crypto.JWT
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Except (runExceptT)
import Data.Aeson (FromJSON(parseJSON))
import Data.Aeson.Lens (key, values, _String)
import Data.Configurator.Types (Config)

import           Data.String (fromString)
import           Data.Text (pack)
import           Data.Text.Lens (packed)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.ByteString (ByteString, stripPrefix)
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64.URL as B64

newtype Auth0JWKSet = Auth0JWKSet JWKSet
instance FromJSON Auth0JWKSet where
    parseJSON = fmap Auth0JWKSet . parseJSON . fixJWKSet
      where
        fixJWKSet = key "keys" . values . key "x5t" . _String %~ fixX5T
        fixX5T = decodeUtf8 . B64.encode . fst . B16.decode . B64.decodeLenient . encodeUtf8

withAuth :: (MonadReader Config m, MonadSnap m) => (User -> m a) -> m a
withAuth action = do
    authHeader <- getHeader "Authorization" <$> getRequest
    compactJWT <- case authHeader >>= stripPrefix "Bearer " of
        Just token -> return (fromStrict token)
        Nothing -> reject 400 "Missing authorization header"

    jwksURL <- configured "auth.jwks"
    jwtAudience <- fromString <$> configured "auth.audience"
    Auth0JWKSet jwks <- liftIO $ view responseBody <$> (asJSON =<< get jwksURL)
    claimsOrError <- liftIO . runExceptT $ do
        jwt <- decodeCompact compactJWT
        let settings = defaultJWTValidationSettings (== jwtAudience)
        verifyClaims settings jwks jwt

    case claimsOrError of
        Right claims -> case claims ^? claimSub . _Just . string . packed of
            Just uName -> getUserByName uName >>= action
            Nothing -> reject 400 "No user id was provided"
        Left err -> reject 401 (show (err :: JWTError))

reject :: (MonadSnap m) => Int -> String -> m a
reject code err = do
    modifyResponse $ setResponseCode code
    writeText (pack err)
    finishWith =<< getResponse
