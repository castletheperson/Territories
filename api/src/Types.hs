{-# LANGUAGE FlexibleContexts, DeriveGeneric, OverloadedStrings #-}

module Types (User(..), Territory(..), App, serveApp, configured) where

import Snap.Core
import Snap.Http.Server (quickHttpServe)
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import GHC.Generics
import Data.Text (Text)
import Data.Configurator
import Data.Configurator.Types
import Data.Time.Clock
import Data.Int (Int32)
import Data.Char

type App a = ReaderT Config Snap a

serveApp :: App () -> IO ()
serveApp app = do
    config <- fst <$> autoReload autoConfig ["app.cfg"]
    quickHttpServe . flip runReaderT config $ app

configured :: (MonadReader Config m, MonadIO m, Configured a) => Name -> m a
configured name = do
    config <- ask
    liftIO $ require config name

data User = User
    { userId :: Int32
    , userName :: Text
    } deriving (Generic, Show)

instance FromJSON User
instance ToJSON User

data Territory = Territory
    { terrId :: Int32
    , terrUserId :: Int32
    , terrName :: Text
    , terrInstructions :: Text
    , terrBoundary :: Text
    , terrCreated :: UTCTime
    , terrUpdated :: UTCTime
    } deriving (Generic, Show)

instance FromJSON Territory where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = \(_:_:_:_:x:xs) -> toLower x : xs
        }

instance ToJSON Territory where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = \(_:_:_:_:x:xs) -> toLower x : xs
        }
