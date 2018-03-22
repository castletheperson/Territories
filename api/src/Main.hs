{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import Data.Maybe
import Control.Applicative hiding (empty)
import Control.Monad.IO.Class (liftIO)

import Snap.Core
import Snap.Http.Server hiding (Config)

import Data.Aeson
import GHC.Generics
import Data.Text (Text)

import Data.Configurator
import Data.Configurator.Types (Config)

import Database.MySQL.Base
import System.IO.Streams (toList)

data User = User
    { userName :: Text
    , firstName :: Text
    , lastName :: Text
    } deriving (Generic, Show)

instance ToJSON User where

main :: IO ()
main = do
    config <- fst <$> autoReload autoConfig ["db/connection.cfg"]
    quickHttpServe $ ifTop pass <|>
        route [ ("users", viewUsers config)
              ]

viewUsers :: Config -> Snap ()
viewUsers config = do
    users <- liftIO $ getUsers config
    modifyResponse $ setContentType "application/json"
    writeLBS (encode users)

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
    (_, resultStream) <- query_ conn "SELECT username, fname, lname FROM users"
    users <- catMaybes . map toUser <$> toList resultStream
    close conn
    return users

toUser :: [MySQLValue] -> Maybe User
toUser [MySQLText u, MySQLText f, MySQLText l] = Just (User u f l)
toUser _                                       = Nothing
