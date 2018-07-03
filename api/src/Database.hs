{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Database (getTerritories, getTerritoryByName, putTerritory, getUserByName) where

import Types

import Control.Monad.Reader
import Data.Maybe (catMaybes)
import Data.Configurator.Types (Config)
import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Data.Text (Text, unpack)
import Data.Time.LocalTime (localTimeToUTC, utc)
import qualified Data.ByteString.Lazy as BL

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
    stmt <- prepareStmt conn "SELECT id, userId, name, instructions, boundary, created, updated FROM territories WHERE userId = ?"
    (_, resultStream) <- queryStmt conn stmt [MySQLInt32 (userId user)]
    catMaybes . map toTerritory <$> Streams.toList resultStream

getTerritoryByName :: (MonadIO m, MonadReader Config m) => User -> Text -> m (Maybe Territory)
getTerritoryByName user tName = withDBConnection $ \conn -> do
    stmt <- prepareStmt conn "SELECT id, userId, name, instructions, boundary, created, updated FROM territories WHERE userId = ? AND name = ?"
    (_, resultStream) <- queryStmt conn stmt [MySQLInt32 (userId user), MySQLText tName]
    (toTerritory =<<) <$> Streams.read resultStream

putTerritory :: (MonadIO m, MonadReader Config m) => User -> Territory -> m Bool
putTerritory user terr = withDBConnection $ \conn -> do
    ok <- if terrId terr < 0 || terrUserId terr /= userId user
        then insertTerritory conn (terr { terrUserId = userId user })
        else updateTerritory conn terr
    return (okAffectedRows ok == 1)

insertTerritory :: MySQLConn -> Territory -> IO OK
insertTerritory conn terr = do
    stmt <- prepareStmt conn "INSERT INTO territories (userId, name, instructions, boundary) VALUES (?,?,?,?)"
    executeStmt conn stmt [MySQLInt32 (terrUserId terr), MySQLText (terrName terr), MySQLText (terrInstructions terr), MySQLText (terrBoundary terr)]

updateTerritory :: MySQLConn -> Territory -> IO OK
updateTerritory conn (Territory tId _ tName tInstructions tBoundary _ _) = do
    stmt <- prepareStmt conn "UPDATE territories SET name = ?, instructions = ?, boundary = ? WHERE id = ?"
    executeStmt conn stmt [MySQLText tName, MySQLText tInstructions, MySQLText tBoundary, MySQLInt32 tId]

toTerritory :: [MySQLValue] -> Maybe Territory
toTerritory
    [ MySQLInt32 tId
    , MySQLInt32 uId
    , MySQLText tName
    , MySQLText tInstructions
    , MySQLText tBoundary
    , MySQLTimeStamp tCreated
    , MySQLTimeStamp tUpdated
    ] = Just $ Territory
        { terrId = tId
        , terrUserId = uId
        , terrName = tName
        , terrInstructions = tInstructions
        , terrBoundary = tBoundary
        , terrCreated = localTimeToUTC utc tCreated
        , terrUpdated = localTimeToUTC utc tUpdated
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
