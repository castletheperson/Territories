{-# LANGUAGE FlexibleContexts, DeriveGeneric, OverloadedStrings #-}

module Types (User(..), App, serveApp, configured) where

import Snap.Core
import Snap.Http.Server (quickHttpServe)
import Control.Applicative
import Control.Monad.Reader
import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import Data.Configurator
import Data.Configurator.Types

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
    { userId :: Text
    } deriving (Generic, Show)

instance FromJSON User
instance ToJSON User