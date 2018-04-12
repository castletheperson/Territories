{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Main where

import Auth
import Database
import Types

import Snap.Core
import Control.Applicative ((<|>))
import Control.Monad.Reader (MonadReader)
import Data.Aeson (ToJSON, encode)
import Data.Configurator.Types (Config)
import Data.Text.Encoding (decodeUtf8)

main :: IO ()
main = serveApp $
    ifTop pass <|>
    route [ ("territories", withAuth viewTerritories)
          , ("territory/:name", withAuth viewTerritory)
          ]

viewTerritories :: (MonadReader Config m, MonadSnap m) => User -> m ()
viewTerritories user = getTerritories user >>= writeJSON

viewTerritory :: (MonadReader Config m, MonadSnap m) => User -> m ()
viewTerritory user = do
  Just name <- getParam "name"
  terr <- getTerritoryByName user (decodeUtf8 name)
  writeJSON terr

writeJSON :: (MonadSnap m, ToJSON a) => a -> m ()
writeJSON val = do
  modifyResponse $ setContentType "application/json"
  writeLBS (encode val)
