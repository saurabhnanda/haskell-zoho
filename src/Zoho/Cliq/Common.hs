{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Common types shared across Zoho Cliq modules
module Zoho.Cliq.Common
  ( BotUniqueName(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

-- | Bot unique name
newtype BotUniqueName = BotUniqueName { rawBotUniqueName :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON BotUniqueName where
  parseJSON = withText "BotUniqueName" (pure . BotUniqueName)

instance ToJSON BotUniqueName where
  toJSON (BotUniqueName t) = String t
