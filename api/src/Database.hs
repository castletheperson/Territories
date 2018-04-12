{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Database (getTerritories, getTerritoryByName, getUserByName) where

import Types

import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.Maybe (catMaybes)
import Data.Configurator
import Data.Configurator.Types (Config)
import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Data.Text (Text)

withDBConnection :: (MonadIO m, MonadReader Config m) => (MySQLConn -> IO a) -> m a
withDBConnection action = do
    dbHost <- configured "db.host"
    dbPort <- configured "db.port"
    dbUser <- configured "db.user"
    dbPass <- configured "db.password"
    dbSchema <- configured "db.schema"
    liftIO $ do
        conn <- connect defaultConnectInfoMB4
                { ciHost = dbHost
                , ciPort = fromInteger dbPort
                , ciUser = dbUser
                , ciPassword = dbPass
                , ciDatabase = dbSchema
                }
        result <- action conn
        close conn
        return result

getUserByName :: (MonadIO m, MonadReader Config m) => Text -> m User
getUserByName uName = withDBConnection $ \conn -> do
    stmt <- prepareStmt conn "SELECT id, name FROM users WHERE name = ?"
    (_, resultStream) <- queryStmt conn stmt [MySQLText uName]
    user <- (toUser =<<) <$> Streams.read resultStream
    case user of
        Just u -> return u
        Nothing -> do
          ok <- putUserByName conn uName
          return $ User { userId = fromIntegral (okLastInsertID ok), userName = uName }

putUserByName :: MySQLConn -> Text -> IO OK
putUserByName conn uName = do
    stmt <- prepareStmt conn "INSERT INTO users (name) VALUES (?)"
    executeStmt conn stmt [MySQLText uName]

getTerritories :: (MonadIO m, MonadReader Config m) => User -> m [Territory]
getTerritories user = withDBConnection $ \conn -> do
    stmt <- prepareStmt conn "SELECT id, userId, name, instructions, created, updated FROM territories WHERE userId = ?"
    (_, resultStream) <- queryStmt conn stmt [MySQLInt32 (userId user)]
    catMaybes . map toTerritory <$> Streams.toList resultStream

getTerritoryByName :: (MonadIO m, MonadReader Config m) => User -> Text -> m (Maybe Territory)
getTerritoryByName user tName = withDBConnection $ \conn -> do
    stmt <- prepareStmt conn "SELECT id, userId, name, instructions, created, updated FROM territories WHERE userId = ? AND name = ?"
    (_, resultStream) <- queryStmt conn stmt [MySQLInt32 (userId user), MySQLText tName]
    (toTerritory =<<) <$> Streams.read resultStream

toTerritory :: [MySQLValue] -> Maybe Territory
toTerritory
    [ MySQLInt32 tId
    , MySQLInt32 uId
    , MySQLText tName
    , MySQLText tInstructions
    , MySQLTimeStamp tCreated
    , MySQLTimeStamp tUpdated
    ] = Just $ Territory
        { terrId = tId
        , terrUserId = uId
        , terrName = tName
        , terrInstructions = tInstructions
        , terrPoints = []
        , terrCreated = tCreated
        , terrUpdated = tUpdated
        }
toTerritory _ = Nothing

toUser :: [MySQLValue] -> Maybe User
toUser
    [ MySQLInt32 uId
    , MySQLText uName
    ] = Just $ User
        { userId = uId
        , userName = uName
        }
toUser _ = Nothing
