{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Main where

import Auth
import Database
import Types

import Snap.Core
import Control.Applicative ((<|>))
import Control.Monad.Reader (MonadReader)
import Data.Aeson (ToJSON, encode, eitherDecode)
import Data.Configurator.Types (Config)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (pack)

main :: IO ()
main = serveApp $
    ifTop pass <|>
    route [ ("territories", withAuth viewTerritories)
          , ("territory/:name", method GET (withAuth viewTerritory))
          , ("saveTerritory", methods [POST, PUT, PATCH] (withAuth saveTerritory))
          ]

viewTerritories :: (MonadReader Config m, MonadSnap m) => User -> m ()
viewTerritories user = getTerritories user >>= writeJSON

viewTerritory :: (MonadReader Config m, MonadSnap m) => User -> m ()
viewTerritory user = do
  Just name <- getParam "name"
  terr <- getTerritoryByName user (decodeUtf8 name)
  writeJSON terr

saveTerritory :: (MonadReader Config m, MonadSnap m) => User -> m ()
saveTerritory user = do
  eitherTerr <- eitherDecode <$> readRequestBody 50000
  case eitherTerr of
      Left msg -> do
          modifyResponse $ setResponseCode 400
          writeText (pack ("Invalid Territory: " ++ msg))
      Right terr -> do
          success <- putTerritory user terr
          if success
              then do
                  modifyResponse $ setResponseCode 200
                  writeLBS "Success!"
              else do
                  modifyResponse $ setResponseCode 500
                  writeLBS "Error: could not save."

writeJSON :: (MonadSnap m, ToJSON a) => a -> m ()
writeJSON val = do
  modifyResponse $ setContentType "application/json"
  writeLBS (encode val)
