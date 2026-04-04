{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Common types shared across Zoho Cliq modules
module Zoho.Cliq.Common
  ( BotUniqueName(..)
  , Zuid(..)  -- re-exported from Zoho.Types
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Web.HttpApiData (ToHttpApiData, FromHttpApiData)
import Zoho.Types (Zuid(..))

-- | Bot unique name
newtype BotUniqueName = BotUniqueName { rawBotUniqueName :: Text }
  deriving (Eq, Show, Generic, ToHttpApiData, FromHttpApiData)

instance FromJSON BotUniqueName where
  parseJSON = withText "BotUniqueName" (pure . BotUniqueName)

instance ToJSON BotUniqueName where
  toJSON (BotUniqueName t) = String t
