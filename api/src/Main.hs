{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Main where

import Auth
import Database
import Types

import Snap.Core
import Control.Applicative ((<|>))
import Control.Monad.Reader (MonadReader)
import Data.Aeson (encode)
import Data.Configurator.Types (Config)

main :: IO ()
main = serveApp $
    ifTop pass <|>
    route [ ("users", viewUsers)
          , ("user", withAuth viewUser)
          ]

viewUsers :: (MonadReader Config m, MonadSnap m) => m ()
viewUsers = do
    users <- getUsers
    modifyResponse $ setContentType "application/json"
    writeLBS (encode users)

viewUser :: (MonadSnap m) => User -> m ()
viewUser = writeText . userId