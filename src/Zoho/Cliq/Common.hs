{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Common types shared across Zoho Cliq modules
module Zoho.Cliq.Common
  ( BotUniqueName(..)
  , UserId(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Web.HttpApiData (ToHttpApiData, FromHttpApiData)

-- | Bot unique name
newtype BotUniqueName = BotUniqueName { rawBotUniqueName :: Text }
  deriving (Eq, Show, Generic, ToHttpApiData, FromHttpApiData)

instance FromJSON BotUniqueName where
  parseJSON = withText "BotUniqueName" (pure . BotUniqueName)

instance ToJSON BotUniqueName where
  toJSON (BotUniqueName t) = String t

-- | Cliq user identifier
newtype UserId = UserId { rawUserId :: Text }
  deriving (Eq, Show, Generic, ToHttpApiData, FromHttpApiData)

instance FromJSON UserId where
  parseJSON = withText "UserId" (pure . UserId)

instance ToJSON UserId where
  toJSON (UserId t) = String t
