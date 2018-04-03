{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Database (getUsers) where

import Types

import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.Maybe (catMaybes)
import Data.Configurator
import Data.Configurator.Types (Config)
import Database.MySQL.Base
import System.IO.Streams (toList)

getDBConnection :: (MonadIO m, MonadReader Config m) => m MySQLConn
getDBConnection = do
    dbHost <- configured "db.host"
    dbPort <- configured "db.port"
    dbUser <- configured "db.user"
    dbPass <- configured "db.password"
    dbSchema <- configured "db.schema"
    liftIO $ connect defaultConnectInfoMB4
        { ciHost = dbHost
        , ciPort = fromInteger dbPort
        , ciUser = dbUser
        , ciPassword = dbPass
        , ciDatabase = dbSchema
        }

getUsers :: (MonadIO m, MonadReader Config m) => m [User]
getUsers = do
    conn <- getDBConnection
    liftIO $ do
        (_, resultStream) <- query_ conn "SELECT id FROM users"
        users <- catMaybes . map toUser <$> toList resultStream
        close conn
        return users

toUser :: [MySQLValue] -> Maybe User
toUser [MySQLText uid] = Just (User { userId = uid})
toUser _               = Nothing