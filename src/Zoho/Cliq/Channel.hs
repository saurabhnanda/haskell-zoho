{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Zoho.Cliq.Channel
  ( -- * Types
    ChannelId(..)
  , ChannelUniqueName(..)
  , Channel(..)
  , CreateChannelReq(..)
  , CreateChannelResponse(..)
  , ListChannelsOptions(..)

  -- * Re-exports from Common
  , BotUniqueName(..)
  , Zuid(..)

  , ChannelMember(..)

  -- * API Functions
  , createChannel
  , listChannels
  , associateBotWithChannel
  , addMembersToChannel
  , getChannelMembers
  , removeMemberFromChannel
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
import Web.HttpApiData (ToHttpApiData, FromHttpApiData)
import Zoho.Cliq.Common (BotUniqueName(..), Zuid(..))
import Zoho.Types (Error, ResponseWrapper, zohoPrefixTyp, unwrapResponse)
import qualified Zoho.OAuth as ZO
import qualified Zoho.ZohoM as ZM

-- | Channel identifier
newtype ChannelId = ChannelId { rawChannelId :: Text }
  deriving (Eq, Show, Generic, ToHttpApiData, FromHttpApiData)

instance FromJSON ChannelId where
  parseJSON = withText "ChannelId" (pure . ChannelId)

instance ToJSON ChannelId where
  toJSON (ChannelId t) = String t

-- | Channel unique name
newtype ChannelUniqueName = ChannelUniqueName { rawChannelUniqueName :: Text }
  deriving (Eq, Show, Generic, ToHttpApiData, FromHttpApiData)

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
  parseJSON = genericParseJSON (zohoPrefixTyp Casing.snakeCase)

instance ToJSON Channel where
  toJSON = genericToJSON (zohoPrefixTyp Casing.snakeCase)

$(makeLensesWith abbreviatedFields ''Channel)

-- | Request to create a channel
-- Note: No lenses generated - API boundary types use record accessors directly
data CreateChannelReq = CreateChannelReq
  { createName :: !Text                 -- ^ Channel name (max 50 characters)
  , createDescription :: !(Maybe Text)  -- ^ Channel description
  , createUserIds :: !(Maybe [Text])    -- ^ User IDs to add to channel (optional, omit for private channels with just creator)
  , createTeamIds :: !(Maybe [Text])    -- ^ Team IDs (optional)
  , createLevel :: !(Maybe Text)        -- ^ Channel level (optional, e.g., "team")
  } deriving (Eq, Show, Generic)

instance ToJSON CreateChannelReq where
  toJSON = genericToJSON (zohoPrefixTyp Casing.snakeCase)

-- | Response from creating a channel
-- Note: No lenses generated - API boundary types use record accessors directly
data CreateChannelResponse = CreateChannelResponse
  { createChannelId :: !ChannelId    -- ^ Created channel ID
  , createChatId :: !Text            -- ^ Chat ID for the channel
  , createUniqueName :: !Text        -- ^ Unique name of the channel
  } deriving (Eq, Show, Generic)

instance FromJSON CreateChannelResponse where
  parseJSON = genericParseJSON (zohoPrefixTyp Casing.snakeCase)

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

-- | Create a channel - Request builder
createChannelRequest :: CreateChannelReq -> Request
createChannelRequest req =
  let endpoint = mkCliqEndpoint "/channels"
  in ZO.prepareJSONPost endpoint [] [] req

-- | Create a channel
createChannel :: (ZM.HasZoho m)
              => CreateChannelReq
              -> m (Either Error CreateChannelResponse)
createChannel req = ZM.runRequestAndParseResponse $ createChannelRequest req

-- | Associate a bot with a channel - Request builder
-- POST /api/v2/bots/{BOT_UNIQUE_NAME}/associate
-- Payload: { "channel_unique_name": {CHANNEL_UNIQUE_NAME} }
associateBotWithChannelRequest :: BotUniqueName -> ChannelUniqueName -> Request
associateBotWithChannelRequest (BotUniqueName botName) (ChannelUniqueName channelName) =
  let endpoint = mkCliqEndpoint $ "/bots/" <> toS botName <> "/associate"
      payload = object ["channel_unique_name" .= channelName]
  in ZO.prepareJSONPost endpoint [] [] payload

-- | Associate a bot with a channel
-- Returns 204 No Content on success
associateBotWithChannel :: (ZM.HasZoho m)
                        => BotUniqueName      -- ^ Bot's unique name
                        -> ChannelUniqueName  -- ^ Channel's unique name
                        -> m (Either Error ())
associateBotWithChannel botName channelName =
  ZM.runRequestAndParseOptionalResponse () Prelude.id $ associateBotWithChannelRequest botName channelName

-- | A channel member
-- Response: {"user_id":"56087523","email_id":"saurabh@vacationlabs.com","name":"saurabh","user_role":"super_admin"}
data ChannelMember = ChannelMember
  { memberUserId :: !Zuid
  , memberName :: !Text
  , memberEmailId :: !(Maybe Text)
  , memberUserRole :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance FromJSON ChannelMember where
  parseJSON = genericParseJSON (zohoPrefixTyp Casing.snakeCase)

-- | Get channel members
-- GET /api/v2/channels/{CHANNEL_ID}/members
getChannelMembersRequest :: ChannelId -> Request
getChannelMembersRequest (ChannelId cid) =
  let endpoint = mkCliqEndpoint $ "/channels/" <> toS cid <> "/members"
  in ZO.prepareGet endpoint [] []

getChannelMembers :: (ZM.HasZoho m)
                  => ChannelId
                  -> m (Either Error [ChannelMember])
getChannelMembers chan = do
  result :: Either Error (ResponseWrapper "members" [ChannelMember]) <- ZM.runRequestAndParseResponse $ getChannelMembersRequest chan
  pure $ fmap unwrapResponse result

-- | Add members to a channel
-- POST /api/v2/channels/{CHANNEL_ID}/members with { "user_ids": ["123456", "223456"] }
-- Max 100 users per request, 10 requests/min
addMembersToChannelRequest :: ChannelId -> [Zuid] -> Request
addMembersToChannelRequest (ChannelId cid) userIds =
  let endpoint = mkCliqEndpoint $ "/channels/" <> toS cid <> "/members"
      payload = object ["user_ids" .= userIds]
  in ZO.prepareJSONPost endpoint [] [] payload

-- | Returns 204 No Content on success
addMembersToChannel :: (ZM.HasZoho m)
                    => ChannelId
                    -> [Zuid]    -- ^ User IDs to add (max 100)
                    -> m (Either Error ())
addMembersToChannel chan userIds =
  ZM.runRequestAndParseOptionalResponse () Prelude.id $ addMembersToChannelRequest chan userIds

-- | Remove a member from a channel
-- DELETE /api/v2/channels/{CHANNEL_ID}/members/{USER_ID}
removeMemberFromChannelRequest :: ChannelId -> Zuid -> Request
removeMemberFromChannelRequest (ChannelId cid) (Zuid uid) =
  let endpoint = mkCliqEndpoint $ "/channels/" <> toS cid <> "/members/" <> toS uid
  in ZO.prepareDelete endpoint [] [] Nothing

-- | Returns 204 No Content on success
removeMemberFromChannel :: (ZM.HasZoho m)
                        => ChannelId
                        -> Zuid     -- ^ User ID to remove
                        -> m (Either Error ())
removeMemberFromChannel chan uid =
  ZM.runRequestAndParseOptionalResponse () Prelude.id $ removeMemberFromChannelRequest chan uid
