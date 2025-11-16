{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Zoho.Cliq.Message
  ( -- * Types
    ChatId(..)
  , MessageId(..)
  , BotUniqueName(..)
  , MessageType(..)
  , MessageSender(..)
  , MessageContent(..)
  , Message(..)
  , PostMessageReq(..)
  , PostMessageResponse(..)
  , EditMessageReq(..)
  , GetMessagesOptions(..)

  -- * Re-exports from Zoho.Cliq.Channel
  , module Zoho.Cliq.Channel

  -- * API Functions
  , getMessages
  , getMessage
  , postMessageToChannel
  , postMessageToChannelAsBot
  , postMessageToChat
  , postMessageToUser
  , editMessage
  , deleteMessage
  , getReactions
  ) where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Aeson
import qualified Data.Aeson.Casing as Casing
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.HashMap.Strict (HashMap)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics
import Network.HTTP.Client (Request)
import qualified URI.ByteString as U
import Zoho.Cliq.Channel
import Zoho.Types (Error, ResponseWrapper, zohoPrefixTyp, unwrapResponse)
import qualified Zoho.OAuth as ZO
import qualified Zoho.ZohoM as ZM

-- | Chat identifier
newtype ChatId = ChatId { rawChatId :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON ChatId where
  parseJSON = withText "ChatId" (pure . ChatId)

instance ToJSON ChatId where
  toJSON (ChatId t) = String t

-- | Message identifier
newtype MessageId = MessageId { rawMessageId :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON MessageId where
  parseJSON = withText "MessageId" (pure . MessageId)

instance ToJSON MessageId where
  toJSON (MessageId t) = String t

-- | Bot unique name
newtype BotUniqueName = BotUniqueName { rawBotUniqueName :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON BotUniqueName where
  toJSON (BotUniqueName t) = String t

-- | Message type
data MessageType = TextMessage | FileMessage | OtherMessageType Text
  deriving (Eq, Show, Generic)

instance FromJSON MessageType where
  parseJSON = withText "MessageType" $ \t -> case t of
    "text" -> pure TextMessage
    "file" -> pure FileMessage
    other -> pure $ OtherMessageType other

instance ToJSON MessageType where
  toJSON TextMessage = String "text"
  toJSON FileMessage = String "file"
  toJSON (OtherMessageType t) = String t

-- | Message sender
data MessageSender = MessageSender
  { senderName :: !Text
  , senderId :: !Text
  } deriving (Eq, Show, Generic)

instance FromJSON MessageSender where
  parseJSON = genericParseJSON (zohoPrefixTyp Casing.camelCase)

instance ToJSON MessageSender where
  toJSON = genericToJSON (zohoPrefixTyp Casing.camelCase)

$(makeLensesWith abbreviatedFields ''MessageSender)

-- | Message content (simplified - can be extended for file attachments)
data MessageContent = MessageContent
  { contentText :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance FromJSON MessageContent where
  parseJSON = genericParseJSON (zohoPrefixTyp Casing.camelCase)

instance ToJSON MessageContent where
  toJSON = genericToJSON (zohoPrefixTyp Casing.camelCase)

$(makeLensesWith abbreviatedFields ''MessageContent)

-- | A message in a chat/channel
data Message = Message
  { messageSender :: !(Maybe MessageSender)
  , messageId :: !(Maybe MessageId)
  , messageTime :: !(Maybe POSIXTime)
  , messageTyp :: !(Maybe MessageType)  -- 'Typ' will be converted to 'type' by zohoPrefixTyp
  , messageContent :: !(Maybe MessageContent)
  } deriving (Eq, Show, Generic)

instance FromJSON Message where
  parseJSON = genericParseJSON (zohoPrefixTyp Casing.camelCase)

instance ToJSON Message where
  toJSON = genericToJSON (zohoPrefixTyp Casing.camelCase)

$(makeLensesWith abbreviatedFields ''Message)

-- | Options for getting messages
data GetMessagesOptions = GetMessagesOptions
  { optionsFromtime :: !(Maybe Integer)  -- ^ Time in milliseconds
  , optionsTotime :: !(Maybe Integer)    -- ^ Time in milliseconds
  , optionsLimit :: !(Maybe Int)         -- ^ Max 100
  } deriving (Eq, Show, Generic)

$(makeLensesWith abbreviatedFields ''GetMessagesOptions)

-- | Request body for posting a message
data PostMessageReq = PostMessageReq
  { reqText :: !Text
  , reqReplyTo :: !(Maybe MessageId)
  , reqSyncMessage :: !(Maybe Bool)
  , reqMarkAsRead :: !(Maybe Bool)
  } deriving (Eq, Show, Generic)

instance ToJSON PostMessageReq where
  toJSON = genericToJSON (zohoPrefixTyp Casing.camelCase)

$(makeLensesWith abbreviatedFields ''PostMessageReq)

-- | Response from POST message endpoint (when sync_message = true)
newtype PostMessageResponse = PostMessageResponse
  { postResponseMessageId :: MessageId  -- Different prefix to avoid conflict with Message's messageId
  } deriving (Eq, Show, Generic)

instance FromJSON PostMessageResponse where
  parseJSON = withObject "PostMessageResponse" $ \o ->
    PostMessageResponse <$> o .: "message_id"

$(makeLensesWith abbreviatedFields ''PostMessageResponse)

-- | Request body for editing a message
data EditMessageReq = EditMessageReq
  { editText :: !Text
  , editNotifyEdit :: !(Maybe Bool)
  } deriving (Eq, Show, Generic)

instance ToJSON EditMessageReq where
  toJSON = genericToJSON (zohoPrefixTyp Casing.camelCase)

$(makeLensesWith abbreviatedFields ''EditMessageReq)


-- | Helper to create Cliq API endpoint
mkCliqEndpoint :: ByteString -> U.URI
mkCliqEndpoint p = ZO.mkEndpoint (U.Host "cliq.zoho.com") ("/api/v2" <> p)

-- | Get messages from a chat - Request builder
getMessagesRequest :: ChatId -> GetMessagesOptions -> Request
getMessagesRequest cid opts =
  let endpoint = mkCliqEndpoint $ "/chats/" <> toS (rawChatId cid) <> "/messages"
      queryParams =
        ZO.applyOptionalQueryParam "fromtime" (show <$> optionsFromtime opts) $
        ZO.applyOptionalQueryParam "totime" (show <$> optionsTotime opts) $
        ZO.applyOptionalQueryParam "limit" (show <$> optionsLimit opts)
        []
  in ZO.prepareGet endpoint queryParams []

-- | Get messages from a chat
getMessages :: (ZM.HasZoho m)
            => ChatId
            -> GetMessagesOptions
            -> m (Either Error [Message])
getMessages cid opts = do
  result :: Either Error (ResponseWrapper "data" [Message]) <- ZM.runRequestAndParseResponse $ getMessagesRequest cid opts
  pure $ fmap unwrapResponse result

-- | Get a single message - Request builder
getMessageRequest :: ChatId -> MessageId -> Request
getMessageRequest cid mid =
  let endpoint = mkCliqEndpoint $ "/chats/" <> toS (rawChatId cid) <> "/messages/" <> toS (rawMessageId mid)
  in ZO.prepareGet endpoint [] []

-- | Get a single message
getMessage :: (ZM.HasZoho m)
           => ChatId
           -> MessageId
           -> m (Either Error Message)
getMessage cid mid = ZM.runRequestAndParseResponse $ getMessageRequest cid mid

-- | Post a message to a channel by channel unique name - Request builder
postMessageToChannelRequest :: ChannelUniqueName -> PostMessageReq -> Request
postMessageToChannelRequest channelName msgReq =
  let endpoint = mkCliqEndpoint $ "/channelsbyname/" <> toS (rawChannelUniqueName channelName) <> "/message"
  in ZO.prepareJSONPost endpoint [] [] msgReq

-- | Post a message to a channel by channel unique name
-- Returns Nothing if sync_message was not set (empty response), Just Value if sync_message=true
postMessageToChannel :: (ZM.HasZoho m)
                     => ChannelUniqueName
                     -> PostMessageReq
                     -> m (Either Error (Maybe Value))
postMessageToChannel channelName msgReq = ZM.runRequestAndParseOptionalResponse Nothing Just $ postMessageToChannelRequest channelName msgReq

-- | Post a message to a channel as a bot - Request builder
postMessageToChannelAsBotRequest :: ChannelUniqueName -> BotUniqueName -> PostMessageReq -> Request
postMessageToChannelAsBotRequest channelName botName msgReq =
  let endpoint = mkCliqEndpoint $ "/channelsbyname/" <> toS (rawChannelUniqueName channelName) <> "/message"
      queryParams = [("bot_unique_name", Just $ toS $ rawBotUniqueName botName)]
  in ZO.prepareJSONPost endpoint queryParams [] msgReq

-- | Post a message to a channel as a bot
-- Returns Nothing if sync_message was not set (empty response), Just Value if sync_message=true
postMessageToChannelAsBot :: (ZM.HasZoho m)
                          => ChannelUniqueName
                          -> BotUniqueName
                          -> PostMessageReq
                          -> m (Either Error (Maybe Value))
postMessageToChannelAsBot channelName botName msgReq = ZM.runRequestAndParseOptionalResponse Nothing Just $ postMessageToChannelAsBotRequest channelName botName msgReq

-- | Post a message to a chat by chat ID - Request builder
postMessageToChatRequest :: ChatId -> PostMessageReq -> Request
postMessageToChatRequest cid msgReq =
  let endpoint = mkCliqEndpoint $ "/chats/" <> toS (rawChatId cid) <> "/message"
  in ZO.prepareJSONPost endpoint [] [] msgReq

-- | Post a message to a chat by chat ID
-- Returns Nothing if sync_message was not set (empty response), Just Value if sync_message=true
postMessageToChat :: (ZM.HasZoho m)
                  => ChatId
                  -> PostMessageReq
                  -> m (Either Error (Maybe Value))
postMessageToChat cid msgReq = ZM.runRequestAndParseOptionalResponse Nothing Just $ postMessageToChatRequest cid msgReq

-- | Post a message to a user by email ID - Request builder
postMessageToUserRequest :: Text -> PostMessageReq -> Request
postMessageToUserRequest email msgReq =
  let endpoint = mkCliqEndpoint $ "/buddies/" <> toS email <> "/message"
  in ZO.prepareJSONPost endpoint [] [] msgReq

-- | Post a message to a user by email ID
-- Returns Nothing if sync_message was not set (empty response), Just Value if sync_message=true
postMessageToUser :: (ZM.HasZoho m)
                  => Text  -- ^ Email ID
                  -> PostMessageReq
                  -> m (Either Error (Maybe Value))
postMessageToUser email msgReq = ZM.runRequestAndParseOptionalResponse Nothing Just $ postMessageToUserRequest email msgReq

-- | Edit a message - Request builder
editMessageRequest :: ChatId -> MessageId -> EditMessageReq -> Request
editMessageRequest cid mid editReq =
  let endpoint = mkCliqEndpoint $ "/chats/" <> toS (rawChatId cid) <> "/messages/" <> toS (rawMessageId mid)
  in ZO.prepareJSONPatch endpoint [] [] editReq

-- | Edit a message
editMessage :: (ZM.HasZoho m)
            => ChatId
            -> MessageId
            -> EditMessageReq
            -> m (Either Error Value)
editMessage cid mid editReq = ZM.runRequestAndParseResponse $ editMessageRequest cid mid editReq

-- | Delete a message - Request builder
deleteMessageRequest :: ChatId -> MessageId -> Request
deleteMessageRequest cid mid =
  let endpoint = mkCliqEndpoint $ "/chats/" <> toS (rawChatId cid) <> "/messages/" <> toS (rawMessageId mid)
  in ZO.prepareDelete endpoint [] [] Nothing

-- | Delete a message (returns 204 No Content on success)
deleteMessage :: (ZM.HasZoho m)
              => ChatId
              -> MessageId
              -> m (Either Error Value)
deleteMessage cid mid = ZM.runRequestAndParseResponse $ deleteMessageRequest cid mid

-- | Get reactions for a message - Request builder
getReactionsRequest :: ChatId -> MessageId -> Request
getReactionsRequest cid mid =
  let endpoint = mkCliqEndpoint $ "/chats/" <> toS (rawChatId cid) <> "/messages/" <> toS (rawMessageId mid) <> "/reactions"
  in ZO.prepareGet endpoint [] []

-- | Get reactions for a message
getReactions :: (ZM.HasZoho m)
             => ChatId
             -> MessageId
             -> m (Either Error (HashMap Text [Text]))
getReactions cid mid = do
  result :: Either Error (ResponseWrapper "data" (HashMap Text [Text])) <- ZM.runRequestAndParseResponse $ getReactionsRequest cid mid
  pure $ fmap unwrapResponse result
