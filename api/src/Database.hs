{-# LANGUAGE OverloadedStrings #-}

module Database (getUsers) where

import Types

import Data.Maybe (catMaybes)
import Data.Configurator
import Data.Configurator.Types (Config)
import Database.MySQL.Base
import System.IO.Streams (toList)

getDBConnection :: Config -> IO MySQLConn
getDBConnection config = do
    dbHost <- lookupDefault "localhost" config "host"
    dbPort <- lookupDefault 3306 config "port"
    dbUser <- lookupDefault "root" config "user"
    dbPass <- lookupDefault "" config "password"
    connect defaultConnectInfoMB4
        { ciHost = dbHost
        , ciPort = fromInteger dbPort
        , ciUser = dbUser
        , ciPassword = dbPass
        , ciDatabase = "territories"
        }

getUsers :: Config -> IO [User]
getUsers config = do
    conn <- getDBConnection config
    (_, resultStream) <- query_ conn "SELECT id FROM users"
    users <- catMaybes . map toUser <$> toList resultStream
    close conn
    return users

toUser :: [MySQLValue] -> Maybe User
toUser [MySQLText uid] = Just (User { userId = uid})
toUser _               = Nothing