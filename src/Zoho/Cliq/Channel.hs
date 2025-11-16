{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Zoho.Cliq.Channel
  ( -- * Types
    ChannelId(..)
  , ChannelUniqueName(..)
  , Channel(..)
  , ListChannelsOptions(..)

  -- * API Functions
  , listChannels
  ) where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Aeson
import qualified Data.Aeson.Casing as Casing
import Data.ByteString (ByteString)
import Data.String.Conv (toS)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics
import Network.HTTP.Client (Request)
import qualified URI.ByteString as U
import Zoho.Types (Error, ResponseWrapper, zohoPrefixTyp, unwrapResponse)
import qualified Zoho.OAuth as ZO
import qualified Zoho.ZohoM as ZM

-- | Channel identifier
newtype ChannelId = ChannelId { rawChannelId :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON ChannelId where
  parseJSON = withText "ChannelId" (pure . ChannelId)

instance ToJSON ChannelId where
  toJSON (ChannelId t) = String t

-- | Channel unique name
newtype ChannelUniqueName = ChannelUniqueName { rawChannelUniqueName :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON ChannelUniqueName where
  parseJSON = withText "ChannelUniqueName" (pure . ChannelUniqueName)

instance ToJSON ChannelUniqueName where
  toJSON (ChannelUniqueName t) = String t

-- | A channel
data Channel = Channel
  { channelId :: !(Maybe ChannelId)
  , channelName :: !(Maybe Text)
  , channelUniqueName :: !(Maybe Text)
  , channelDescription :: !(Maybe Text)
  , channelCreatedTime :: !(Maybe POSIXTime)
  } deriving (Eq, Show, Generic)

instance FromJSON Channel where
  parseJSON = genericParseJSON (zohoPrefixTyp Casing.camelCase)

instance ToJSON Channel where
  toJSON = genericToJSON (zohoPrefixTyp Casing.camelCase)

$(makeLensesWith abbreviatedFields ''Channel)

-- | Options for listing channels
data ListChannelsOptions = ListChannelsOptions
  { listJoined :: !(Maybe Bool)     -- ^ Filter joined channels
  , listPinned :: !(Maybe Bool)     -- ^ Filter pinned channels
  , listLimit :: !(Maybe Int)       -- ^ Max number to return
  , listNextToken :: !(Maybe Text)  -- ^ Pagination token
  } deriving (Eq, Show, Generic)

$(makeLensesWith abbreviatedFields ''ListChannelsOptions)

-- | Helper to create Cliq API endpoint
mkCliqEndpoint :: ByteString -> U.URI
mkCliqEndpoint p = ZO.mkEndpoint (U.Host "cliq.zoho.com") ("/api/v2" <> p)

-- | List channels - Request builder
listChannelsRequest :: ListChannelsOptions -> Request
listChannelsRequest opts =
  let endpoint = mkCliqEndpoint "/channels"
      queryParams =
        ZO.applyOptionalQueryParam "joined" (boolToLower <$> listJoined opts) $
        ZO.applyOptionalQueryParam "pinned" (boolToLower <$> listPinned opts) $
        ZO.applyOptionalQueryParam "limit" (show <$> listLimit opts) $
        ZO.applyOptionalQueryParam "next_token" (listNextToken opts)
        []
  in ZO.prepareGet endpoint queryParams []
  where
    boolToLower :: Bool -> Text
    boolToLower True = "true"
    boolToLower False = "false"

-- | List channels
listChannels :: (ZM.HasZoho m)
             => ListChannelsOptions
             -> m (Either Error [Channel])
listChannels opts = do
  result :: Either Error (ResponseWrapper "channels" [Channel]) <- ZM.runRequestAndParseResponse $ listChannelsRequest opts
  pure $ fmap unwrapResponse result
