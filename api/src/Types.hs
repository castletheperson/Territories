{-# LANGUAGE DeriveGeneric #-}

module Types (User(..)) where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)

data User = User
    { userId :: Text
    } deriving (Generic, Show)

instance FromJSON User where
instance ToJSON User where