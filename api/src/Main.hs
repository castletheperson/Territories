{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import Data.Maybe
import GHC.Generics
import Control.Monad.IO.Class
import Snap.Core hiding (pass)
import Snap.Http.Server
import Data.Text (Text)
import Data.Aeson
--import Data.Configurator
import Database.MySQL.Base
import System.IO.Streams (toList)

data User = User
    { userName :: Text
    , firstName :: Text
    , lastName :: Text
    } deriving (Generic, Show)

instance ToJSON User where

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = do
    modifyResponse (setContentType "application/json")
    let users = [User "4castle" "Castle" "Kerr", User "aikman" "Aikman" "Ewalt"]
    --users <- liftIO getUsers
    mapM_ (writeLBS . encode) users

getUsers :: IO [User]
getUsers = do
    --config <- load [Required "database.cfg"]
    --dbUser <- require config "user"
    --dbPass <- require config "password"
    let dbUser = "root"
    let dbPass = "test"
    conn <- connect defaultConnectInfo { ciUser = dbUser, ciPassword = dbPass, ciDatabase = "territories" }
    (_, resultStream) <- query_ conn "SELECT username, firstName, lastName FROM user"
    users <- catMaybes . map toUser <$> toList resultStream
    close conn
    return users

toUser :: [MySQLValue] -> Maybe User
toUser [MySQLText u, MySQLText f, MySQLText l] = Just (User u f l)
toUser _                                       = Nothing
