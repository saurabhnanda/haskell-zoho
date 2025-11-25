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
  , UserId(..)
  , BotUniqueName(..)
  , MessageType(..)
  , MessageSender(..)
  , MessageContent(..)
  , Message(..)
  , PostMessageReq(..)
  , PostMessageResponse(..)
  , EditMessageReq(..)
  , GetMessagesOptions(..)

  -- * Rich Message Types
  , CliqMessage(..)
  , CliqStandardMessage(..)
  , CliqButton(..)
  , CliqButtonAction(..)
  , CliqForm(..)
  , CliqBanner(..)

  -- * Smart Constructors
  , textMessage
  , textWithButtons
  , toPostMessageReq

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

  -- * Utilities
  , escapeExclamation
  ) where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Aeson
import Data.Aeson.Types (Object)
import qualified Data.Aeson.Casing as Casing
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
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

-- | JSON options for newtypes - unwraps unary records to raw values
jsonOpts :: Options
jsonOpts = defaultOptions { unwrapUnaryRecords = True }

-- | Chat identifier
newtype ChatId = ChatId { rawChatId :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON ChatId where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON ChatId where
  toJSON = genericToJSON jsonOpts

-- | Message identifier
newtype MessageId = MessageId { rawMessageId :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON MessageId where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON MessageId where
  toJSON = genericToJSON jsonOpts

-- | User identifier
newtype UserId = UserId { rawUserId :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON UserId where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON UserId where
  toJSON = genericToJSON jsonOpts

-- | Bot unique name
newtype BotUniqueName = BotUniqueName { rawBotUniqueName :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON BotUniqueName where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON BotUniqueName where
  toJSON = genericToJSON jsonOpts

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
  , senderId :: !UserId
  } deriving (Eq, Show, Generic)

instance FromJSON MessageSender where
  parseJSON = genericParseJSON (zohoPrefixTyp Casing.snakeCase)

instance ToJSON MessageSender where
  toJSON = genericToJSON (zohoPrefixTyp Casing.snakeCase)

$(makeLensesWith abbreviatedFields ''MessageSender)

-- | Message content (simplified - can be extended for file attachments)
data MessageContent = MessageContent
  { contentText :: !(Maybe Text)
  , contentEdited :: !(Maybe Bool)        -- ^ True if message was edited
  , contentEditedTime :: !(Maybe POSIXTime)  -- ^ Timestamp when message was last edited
  } deriving (Eq, Show, Generic)

instance FromJSON MessageContent where
  parseJSON = genericParseJSON (zohoPrefixTyp Casing.snakeCase)

instance ToJSON MessageContent where
  toJSON = genericToJSON (zohoPrefixTyp Casing.snakeCase)

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
  parseJSON = genericParseJSON (zohoPrefixTyp Casing.snakeCase)

instance ToJSON Message where
  toJSON = genericToJSON (zohoPrefixTyp Casing.snakeCase)

$(makeLensesWith abbreviatedFields ''Message)

-- | Options for getting messages
data GetMessagesOptions = GetMessagesOptions
  { optionsFromtime :: !(Maybe Integer)  -- ^ Time in milliseconds
  , optionsTotime :: !(Maybe Integer)    -- ^ Time in milliseconds
  , optionsLimit :: !(Maybe Int)         -- ^ Max 100
  } deriving (Eq, Show, Generic)

$(makeLensesWith abbreviatedFields ''GetMessagesOptions)

-- | Button action types
data CliqButtonAction
  = CliqInvokeFunction !Text !(Maybe Text)  -- ^ function name, optional owner email
  | CliqOpenUrl !Text                        -- ^ URL to open
  deriving (Eq, Show, Generic)

instance ToJSON CliqButtonAction where
  toJSON (CliqInvokeFunction name mOwner) = object $
    [ "type" .= ("invoke.function" :: Text)
    , "data" .= object (["name" .= name] ++ maybe [] (\owner -> ["owner" .= owner]) mOwner)
    ]
  toJSON (CliqOpenUrl url) = object
    [ "type" .= ("open.url" :: Text)
    , "data" .= object ["web" .= url]
    ]

-- | Button with label, type, key, and action
data CliqButton = CliqButton
  { cbLabel :: !Text                   -- ^ Button label text (max 20 characters)
  , cbTyp :: !Text                     -- ^ "+" for primary, "-" for secondary (will be serialized as "type")
  , cbKey :: !Text                     -- ^ Passed to function as arguments.get("key")
  , cbAction :: !CliqButtonAction
  , cbHint :: !(Maybe Text)            -- ^ Optional tooltip text
  , cbArguments :: !(Maybe Value)      -- ^ Custom data passed to function handler
  } deriving (Eq, Show, Generic)

instance ToJSON CliqButton where
  toJSON = genericToJSON $ (zohoPrefixTyp Casing.snakeCase) { omitNothingFields = True }

$(makeLensesWith abbreviatedFields ''CliqButton)

-- | Form message (placeholder - will be expanded later)
data CliqForm = CliqForm
  { cfTitle :: !Text
  , cfHint :: !(Maybe Text)
  , cfName :: !Text
  , cfButtonLabel :: !Text
  , cfInputs :: !Value  -- TODO: add CliqFormInput type
  } deriving (Eq, Show, Generic)

instance ToJSON CliqForm where
  toJSON = genericToJSON $ zohoPrefixTyp Casing.snakeCase

$(makeLensesWith abbreviatedFields ''CliqForm)

-- | Banner message (placeholder - will be expanded later)
data CliqBanner = CliqBanner
  { cbanText :: !Text
  , cbanTyp :: !Text  -- ^ "info", "success", "warning", "error" (will be serialized as "type")
  } deriving (Eq, Show, Generic)

instance ToJSON CliqBanner where
  toJSON = genericToJSON $ zohoPrefixTyp Casing.snakeCase

$(makeLensesWith abbreviatedFields ''CliqBanner)

-- | Standard Cliq message where all fields can coexist
-- Any combination of text, card, slides, buttons, and suggestions is valid
data CliqStandardMessage = CliqStandardMessage
  { csmText :: !(Maybe Text)           -- ^ Message text (markdown supported)
  , csmCard :: !(Maybe Value)          -- ^ Card object (TODO: add CliqCard type)
  , csmSlides :: !(Maybe Value)        -- ^ Slides array (TODO: add CliqSlide type)
  , csmButtons :: !(Maybe [CliqButton]) -- ^ Action buttons (max 25)
  , csmSuggestions :: !(Maybe Value)   -- ^ Quick reply suggestions (TODO: add CliqSuggestions type)
  } deriving (Eq, Show, Generic)

instance ToJSON CliqStandardMessage where
  toJSON = genericToJSON $ (zohoPrefixTyp Casing.snakeCase) { omitNothingFields = True }

$(makeLensesWith abbreviatedFields ''CliqStandardMessage)

-- | Top-level Cliq message type with mutually exclusive variants
--
-- Standard messages allow any combination of text, card, slides, buttons, suggestions.
-- Special message types (form, transient, banner) are standalone and cannot be combined with standard elements.
data CliqMessage
  = CliqStandard !CliqStandardMessage  -- ^ Standard message with combinable elements
  | CliqFormMessage !CliqForm          -- ^ Form message (requires type="form")
  | CliqTransientMessage !Text         -- ^ Transient/temporary message (requires type="transient_message")
  | CliqBannerMessage !CliqBanner      -- ^ Banner platform notification
  deriving (Eq, Show, Generic)

instance ToJSON CliqMessage where
  toJSON (CliqStandard msg) = toJSON msg
  toJSON (CliqFormMessage form) = object
    [ "type" .= ("form" :: Text)
    , "form" .= form
    ]
  toJSON (CliqTransientMessage txt) = object
    [ "type" .= ("transient_message" :: Text)
    , "text" .= txt
    ]
  toJSON (CliqBannerMessage banner) = object
    [ "banner" .= banner
    ]

-- | Smart constructor: Create a text-only message
textMessage :: Text -> CliqMessage
textMessage txt = CliqStandard $ CliqStandardMessage
  { csmText = Just txt
  , csmCard = Nothing
  , csmSlides = Nothing
  , csmButtons = Nothing
  , csmSuggestions = Nothing
  }

-- | Smart constructor: Create a message with text and buttons
textWithButtons :: Text -> [CliqButton] -> CliqMessage
textWithButtons txt btns = CliqStandard $ CliqStandardMessage
  { csmText = Just txt
  , csmCard = Nothing
  , csmSlides = Nothing
  , csmButtons = Just btns
  , csmSuggestions = Nothing
  }

-- | Request body for posting a message via REST API
-- See: docs/ZOHO-CLIQ.md for full documentation
--
-- This type wraps CliqMessage and adds REST API specific fields (replyTo, syncMessage, markAsRead).
-- For building messages, use CliqMessage constructors or smart constructors like textMessage, textWithButtons.
--
-- Note: Not all message types may be supported by all endpoints. Standard messages work everywhere.
-- Forms, transient messages, and banners may only work in specific contexts.
data PostMessageReq = PostMessageReq
  { reqMessage :: !CliqMessage              -- ^ The message content (all types supported)
  , reqReplyTo :: !(Maybe MessageId)        -- ^ Optional message to reply to
  , reqSyncMessage :: !(Maybe Bool)         -- ^ Return message ID synchronously
  , reqMarkAsRead :: !(Maybe Bool)          -- ^ Mark message as read
  } deriving (Eq, Show, Generic)

instance ToJSON PostMessageReq where
  toJSON PostMessageReq{..} =
    let msgFields = case toJSON reqMessage of
          Object obj -> obj
          _ -> mempty
        extraFields = object $
          maybe [] (\x -> ["replyTo" .= x]) reqReplyTo ++
          maybe [] (\x -> ["syncMessage" .= x]) reqSyncMessage ++
          maybe [] (\x -> ["markAsRead" .= x]) reqMarkAsRead
    in Object (msgFields <> case extraFields of Object o -> o; _ -> mempty)

$(makeLensesWith abbreviatedFields ''PostMessageReq)

-- | Create a PostMessageReq from a CliqMessage
toPostMessageReq :: CliqMessage -> PostMessageReq
toPostMessageReq msg = PostMessageReq
  { reqMessage = msg
  , reqReplyTo = Nothing
  , reqSyncMessage = Nothing
  , reqMarkAsRead = Nothing
  }

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
  toJSON = genericToJSON (zohoPrefixTyp Casing.snakeCase)

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
-- Returns Nothing if sync_message was not set (empty response), Just PostMessageResponse if sync_message=true
postMessageToChat :: (ZM.HasZoho m)
                  => ChatId
                  -> PostMessageReq
                  -> m (Either Error (Maybe PostMessageResponse))
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

-- | Escape exclamation marks for Zoho Cliq message text
--
-- NOTE: This is ONLY needed when posting messages via Deluge scripts.
-- When using the REST API directly (postMessageToChannel, postMessageToChannelAsBot, etc.),
-- exclamation marks work fine and DO NOT need escaping.
--
-- This function is provided for compatibility with Deluge-based integrations where
-- exclamation marks need to be escaped as \! to avoid "input_json_invalid" errors.
escapeExclamation :: Text -> Text
escapeExclamation = T.replace "!" "\\!"
