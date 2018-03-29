{-# LANGUAGE OverloadedStrings #-}

module Main where

import Auth
import Database
import Types

import Snap.Core
import Snap.Http.Server (quickHttpServe)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.Configurator (autoReload, autoConfig)
import Data.Configurator.Types (Config)

main :: IO ()
main = do
    config <- fst <$> autoReload autoConfig ["db/connection.cfg"]
    quickHttpServe $ ifTop pass <|>
        route [ ("users", viewUsers config)
              , ("user", withAuth viewUser)
              ]

viewUsers :: Config -> Snap ()
viewUsers config = do
    users <- liftIO $ getUsers config
    modifyResponse $ setContentType "application/json"
    writeLBS (encode users)

viewUser :: User -> Snap ()
viewUser = writeText . userId