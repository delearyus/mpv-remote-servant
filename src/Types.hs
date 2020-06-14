{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

data Command = Command { command :: [String] } deriving (Eq,Show,Generic)
instance ToJSON Command
