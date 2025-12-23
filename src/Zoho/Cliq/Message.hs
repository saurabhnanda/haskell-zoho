{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Zoho.Cliq.Message
  ( -- * Types
    ChatId(..)
    , MessageId(..)
  , UserId(..)
  , MessageType(..)
  , MessageSender(..)
  , MessageContentPoly(..)
  , MessageContent
  , MessagePoly(..)
  , Message
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

  -- * Slide Types
  , CliqSlide(..)
  , CliqSlideTable(..)
  , CliqSlideTableStyles(..)
  , CliqSlideTableSticky(..)
  , CliqSlideFields(..)
  , CliqSlideList(..)
  , CliqSlideListStyles(..)
  , CliqListBulletStyle(..)
  , CliqSlideLabel(..)
  , CliqSlideImages(..)
  , CliqSlideChart(..)
  , CliqSlideChartStyles(..)
  , CliqChartPreview(..)
  , CliqSlideChartItem(..)
  , CliqSlideGraph(..)
  , CliqSlideGraphStyles(..)
  , CliqGraphPreview(..)
  , CliqSlideGraphAxis(..)
  , CliqSlideGraphDataPoint(..)
  , CliqSlideGraphValue(..)
  , CliqSlideText(..)

  -- * Smart Constructors
  , textMessage
  , textWithButtons
  , textWithSlides
  , slidesToValue
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
  , msToUTCTime
  , utcTimeToMs
  ) where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Object)
import qualified Data.Aeson.Casing as Casing
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.String.Conv (toS)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import GHC.Generics
import Network.HTTP.Client (Request)
import qualified URI.ByteString as U
import Zoho.Cliq.Channel
import Zoho.Types (Error, ResponseWrapper, zohoPrefix, zohoPrefixTyp, unwrapResponse)
import qualified Zoho.OAuth as ZO
import qualified Zoho.ZohoM as ZM

-- | Convert milliseconds (as Integer) to UTCTime
-- Zoho Cliq API returns timestamps in milliseconds since Unix epoch
msToUTCTime :: Integer -> UTCTime
msToUTCTime ms = posixSecondsToUTCTime (fromIntegral ms / 1000)

-- | Convert UTCTime to milliseconds (as Integer)
-- For sending timestamps back to Zoho Cliq API
utcTimeToMs :: UTCTime -> Integer
utcTimeToMs utc =
  let posix = realToFrac (utcTimeToPOSIXSeconds utc) :: Double
  in round (posix * 1000)

-- | JSON options for newtypes - unwraps unary records to raw values
jsonOpts :: Options
jsonOpts = defaultOptions { unwrapUnaryRecords = True }

-- | Add a "type" field to a JSON object (used for slide discriminators)
addTypeField :: Text -> Value -> Value
addTypeField t (Object o) = Object $ KM.insert "type" (String t) o
addTypeField _ v = v

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
  -- Zoho returns %20 in message IDs but REST API expects _ in URL paths
  -- Normalize here at the parsing boundary to avoid double-encoding issues
  parseJSON = withText "MessageId" $ \t ->
    pure $ MessageId $ T.replace "%20" "_" t

instance ToJSON MessageId where
  toJSON = genericToJSON jsonOpts

-- | User identifier
newtype UserId = UserId { rawUserId :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON UserId where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON UserId where
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
-- Polymorphic over time type to handle Zoho's milliseconds timestamps
data MessageContentPoly time = MessageContent
  { contentText :: !(Maybe Text)
  , contentEdited :: !(Maybe Bool)        -- ^ True if message was edited
  , contentEditedTime :: !(Maybe time)    -- ^ Timestamp when message was last edited
  } deriving (Eq, Show, Generic)

-- | MessageContent with properly converted UTCTime timestamps
type MessageContent = MessageContentPoly UTCTime

-- | Parse as Integer (milliseconds), convert to UTCTime
instance FromJSON (MessageContentPoly UTCTime) where
  parseJSON v = convertTimes <$> genericParseJSON (zohoPrefixTyp Casing.snakeCase) v
    where
      convertTimes :: MessageContentPoly Integer -> MessageContentPoly UTCTime
      convertTimes mc = mc { contentEditedTime = msToUTCTime <$> contentEditedTime mc }

instance FromJSON (MessageContentPoly Integer) where
  parseJSON = genericParseJSON (zohoPrefixTyp Casing.snakeCase)

instance ToJSON (MessageContentPoly UTCTime) where
  toJSON mc = genericToJSON (zohoPrefixTyp Casing.snakeCase) $
    mc { contentEditedTime = utcTimeToMs <$> contentEditedTime mc }

instance ToJSON (MessageContentPoly Integer) where
  toJSON = genericToJSON (zohoPrefixTyp Casing.snakeCase)

$(makeLensesWith abbreviatedFields ''MessageContentPoly)

-- | A message in a chat/channel
-- Polymorphic over time type to handle Zoho's milliseconds timestamps
data MessagePoly time = Message
  { messageSender :: !(Maybe MessageSender)
  , messageId :: !(Maybe MessageId)
  , messageTime :: !(Maybe time)          -- ^ Zoho returns milliseconds, converted to UTCTime
  , messageTyp :: !(Maybe MessageType)    -- ^ 'Typ' will be converted to 'type' by zohoPrefixTyp
  , messageContent :: !(Maybe (MessageContentPoly time))
  } deriving (Eq, Show, Generic)

-- | Message with properly converted UTCTime timestamps
type Message = MessagePoly UTCTime

-- | Parse as Integer (milliseconds), convert to UTCTime
instance FromJSON (MessagePoly UTCTime) where
  parseJSON v = convertTimes <$> genericParseJSON (zohoPrefixTyp Casing.snakeCase) v
    where
      convertTimes :: MessagePoly Integer -> MessagePoly UTCTime
      convertTimes msg = msg
        { messageTime = msToUTCTime <$> messageTime msg
        , messageContent = convertContentTimes <$> messageContent msg
        }
      convertContentTimes :: MessageContentPoly Integer -> MessageContentPoly UTCTime
      convertContentTimes mc = mc { contentEditedTime = msToUTCTime <$> contentEditedTime mc }

instance FromJSON (MessagePoly Integer) where
  parseJSON = genericParseJSON (zohoPrefixTyp Casing.snakeCase)

instance ToJSON (MessagePoly UTCTime) where
  toJSON msg = genericToJSON (zohoPrefixTyp Casing.snakeCase) $
    msg { messageTime = utcTimeToMs <$> messageTime msg
        , messageContent = convertContentTimes <$> messageContent msg
        }
    where
      convertContentTimes :: MessageContentPoly UTCTime -> MessageContentPoly Integer
      convertContentTimes mc = mc { contentEditedTime = utcTimeToMs <$> contentEditedTime mc }

instance ToJSON (MessagePoly Integer) where
  toJSON = genericToJSON (zohoPrefixTyp Casing.snakeCase)

$(makeLensesWith abbreviatedFields ''MessagePoly)

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

-- ============================================================================
-- Slide Types
-- ============================================================================

-- | Table slide - display data in rows and columns.
--
-- Per REST API: https://www.zoho.com/cliq/help/restapi/v2/#attaching_content
data CliqSlideTable = CliqSlideTable
  { tableTitle :: !(Maybe Text)               -- ^ Table title (optional)
  , tableHeaders :: ![Text]                   -- ^ Column headers. Max 10, 30 chars each.
  , tableRows :: ![[Text]]                    -- ^ Row data. Max 100 rows, 100 chars/cell.
  , tableStyles :: !(Maybe CliqSlideTableStyles) -- ^ Optional styling (width, sticky)
  } deriving (Eq, Show, Generic)

-- | Table styles. Key is @"styles"@ (plural), not @"style"@.
data CliqSlideTableStyles = CliqSlideTableStyles
  { stylesWidth :: !(Maybe [Int])            -- ^ Column width %. Length = headers, sum = 100.
  , stylesSticky :: !(Maybe CliqSlideTableSticky) -- ^ Freeze rows/columns
  } deriving (Eq, Show, Generic)

-- | Sticky (freeze) configuration for table rows/columns.
data CliqSlideTableSticky = CliqSlideTableSticky
  { stickyRows :: !(Maybe Int)    -- ^ Rows to freeze. Range: 0-2.
  , stickyColumns :: !(Maybe Int) -- ^ Columns to freeze. Range: 0-2.
  } deriving (Eq, Show, Generic)

instance ToJSON CliqSlideTableSticky where
  toJSON = genericToJSON $ zohoPrefix Casing.snakeCase

instance ToJSON CliqSlideTableStyles where
  toJSON = genericToJSON $ zohoPrefix Casing.snakeCase

instance ToJSON CliqSlideTable where
  toJSON CliqSlideTable{..} = object $ catMaybes
    [ ("title" .=) <$> tableTitle
    -- Cliq expects headers/rows inside a nested "data" object
    -- Rows are objects with header names as keys
    , Just $ "data" .= object
        [ "headers" .= tableHeaders
        , "rows" .= map (HM.fromList . zip tableHeaders) tableRows
        ]
    , ("styles" .=) <$> tableStyles
    ]

-- | Fields slide - display key-value pairs (similar to label but without title)
-- NOTE: "fields" type may not be officially supported - consider using "label" instead
-- Max 10 elements
data CliqSlideFields = CliqSlideFields
  { fieldsData :: ![(Text, Text)]               -- ^ Key-value pairs (max 10)
  } deriving (Eq, Show, Generic)

instance ToJSON CliqSlideFields where
  toJSON CliqSlideFields{..} = object
    [ "data" .= map (\(k, v) -> object [Key.fromText k .= v]) fieldsData
    ]

-- | List slide - display items as a bulleted/numbered list.
--
-- Per REST API: https://www.zoho.com/cliq/help/restapi/v2/#attaching_content
--
-- __WARNING:__ Styles do NOT render for REST API (Dec 2025). Kept for future compatibility.
data CliqSlideList = CliqSlideList
  { listTitle :: !Text                         -- ^ List title
  , listData :: ![Text]                        -- ^ List items
  , listStyles :: !(Maybe CliqSlideListStyles) -- ^ Bullet style (WARNING: doesn't render)
  } deriving (Eq, Show, Generic)

instance ToJSON CliqSlideList where
  toJSON = genericToJSON $ zohoPrefix Casing.snakeCase

-- | List styles - customize bullet appearance. WARNING: doesn't render for REST API.
data CliqSlideListStyles = CliqSlideListStyles
  { stylesType :: !CliqListBulletStyle  -- ^ Bullet/numbering style
  } deriving (Eq, Show, Generic)

instance ToJSON CliqSlideListStyles where
  toJSON = genericToJSON $ zohoPrefix Casing.snakeCase

-- | Bullet style for list slides.
--
-- __WARNING:__ As of Dec 2025, these styles do NOT render for REST API messages.
-- Kept for API spec compliance and future compatibility.
data CliqListBulletStyle
  = ListStyleCircle       -- ^ @circle@ - hollow circle
  | ListStyleDecimal      -- ^ @decimal@ - 1, 2, 3...
  | ListStyleDisc         -- ^ @disc@ - filled circle (default)
  | ListStyleLowerAlpha   -- ^ @lower-alpha@ - a, b, c...
  | ListStyleUpperAlpha   -- ^ @upper-alpha@ - A, B, C...
  | ListStyleSquare       -- ^ @square@ - filled square
  | ListStyleLowerRoman   -- ^ @lower-roman@ - i, ii, iii...
  | ListStyleUpperRoman   -- ^ @upper-roman@ - I, II, III...
  deriving (Eq, Show, Generic, Bounded, Enum)

instance ToJSON CliqListBulletStyle where
  toJSON = genericToJSON $ defaultOptions
    { constructorTagModifier = camelTo2 '-' . drop (length ("ListStyle" :: String))
    }

-- | Label slide - display key-value pairs with a title. Values support markdown links.
--
-- Per REST API: https://www.zoho.com/cliq/help/restapi/v2/#attaching_content
data CliqSlideLabel = CliqSlideLabel
  { labelTitle :: !Text          -- ^ Label title
  , labelData :: ![(Text, Text)] -- ^ Key-value pairs (values support markdown links)
  } deriving (Eq, Show, Generic)

instance ToJSON CliqSlideLabel where
  toJSON CliqSlideLabel{..} = object
    [ "title" .= labelTitle
    , "data" .= map (\(k, v) -> object [Key.fromText k .= v]) labelData
    ]

-- | Images slide - display images in a slider.
--
-- Per REST API: https://www.zoho.com/cliq/help/restapi/v2/#attaching_content
data CliqSlideImages = CliqSlideImages
  { imagesTitle :: !(Maybe Text) -- ^ Image slider title
  , imagesData :: ![Text]        -- ^ Image URLs
  } deriving (Eq, Show, Generic)

instance ToJSON CliqSlideImages where
  toJSON = genericToJSON $ zohoPrefix Casing.snakeCase

-- | Percentage chart slide - pie/doughnut charts.
--
-- Per REST API: https://www.zoho.com/cliq/help/restapi/v2/#attaching_content
data CliqSlideChart = CliqSlideChart
  { chartStyles :: !(Maybe CliqSlideChartStyles) -- ^ Visual style (pie, doughnut, semi_doughnut)
  , chartData :: ![CliqSlideChartItem]           -- ^ Chart segments. Max 5 items.
  } deriving (Eq, Show, Generic)

instance ToJSON CliqSlideChart where
  toJSON = genericToJSON $ zohoPrefix Casing.snakeCase

-- | Chart styles.
data CliqSlideChartStyles = CliqSlideChartStyles
  { stylesPreview :: !CliqChartPreview -- ^ Chart visualization style
  } deriving (Eq, Show, Generic)

instance ToJSON CliqSlideChartStyles where
  toJSON = genericToJSON $ zohoPrefix Casing.snakeCase

-- | Chart preview/visualization style.
data CliqChartPreview
  = ChartPreviewPie           -- ^ @pie@ - standard pie chart
  | ChartPreviewDoughnut      -- ^ @doughnut@ - ring chart with hole
  | ChartPreviewSemiDoughnut  -- ^ @semi_doughnut@ - half ring chart
  deriving (Eq, Show, Generic, Bounded, Enum)

instance ToJSON CliqChartPreview where
  toJSON = genericToJSON $ defaultOptions
    { constructorTagModifier = camelTo2 '_' . drop (length ("ChartPreview" :: String))
    }

-- | Chart data item (segment).
data CliqSlideChartItem = CliqSlideChartItem
  { chartLabel :: !Text   -- ^ Segment label. Max 20 characters.
  , chartValue :: !Double -- ^ Segment value (float).
  } deriving (Eq, Show, Generic)

instance ToJSON CliqSlideChartItem where
  toJSON = genericToJSON $ zohoPrefix Casing.snakeCase

-- | Graph slide - bar charts and trend lines.
--
-- Per REST API: https://www.zoho.com/cliq/help/restapi/v2/#attaching_content
--
-- Note: This is different from 'CliqSlideChart' (percentage_chart) which is for
-- pie/doughnut charts. Graph is for bar charts and trend lines.
data CliqSlideGraph = CliqSlideGraph
  { graphStyles :: !(Maybe CliqSlideGraphStyles) -- ^ Visual style and axis config
  , graphData :: ![CliqSlideGraphDataPoint]      -- ^ Data categories. Max 5.
  } deriving (Eq, Show, Generic)

instance ToJSON CliqSlideGraph where
  toJSON = genericToJSON $ zohoPrefix Casing.snakeCase

-- | Graph styles - preview type and axis labels.
data CliqSlideGraphStyles = CliqSlideGraphStyles
  { stylesPreview :: !CliqGraphPreview             -- ^ Graph visualization style
  , stylesXAxis :: !(Maybe CliqSlideGraphAxis)     -- ^ X-axis config (horizontal)
  , stylesYAxis :: !(Maybe CliqSlideGraphAxis)     -- ^ Y-axis config (vertical)
  } deriving (Eq, Show, Generic)

instance ToJSON CliqSlideGraphStyles where
  toJSON = genericToJSON $ zohoPrefix Casing.snakeCase

-- | Graph preview/visualization style.
data CliqGraphPreview
  = GraphPreviewVerticalBar        -- ^ @vertical_bar@ - standard vertical bar chart
  | GraphPreviewVerticalStackedBar -- ^ @vertical_stacked_bar@ - stacked vertical bar chart
  | GraphPreviewTrend              -- ^ @trend@ - line/trend chart
  deriving (Eq, Show, Generic, Bounded, Enum)

instance ToJSON CliqGraphPreview where
  toJSON = genericToJSON $ defaultOptions
    { constructorTagModifier = camelTo2 '_' . drop (length ("GraphPreview" :: String))
    }

-- | Axis configuration for graph.
data CliqSlideGraphAxis = CliqSlideGraphAxis
  { axisTitle :: !Text  -- ^ Axis title. Max 20 characters.
  } deriving (Eq, Show, Generic)

instance ToJSON CliqSlideGraphAxis where
  toJSON = genericToJSON $ zohoPrefix Casing.snakeCase

-- | Data point (category) for graph.
data CliqSlideGraphDataPoint = CliqSlideGraphDataPoint
  { dataCategory :: !Text             -- ^ Category name. Max 20 characters.
  , dataValues :: ![CliqSlideGraphValue]  -- ^ Data values. Max 20 items.
  } deriving (Eq, Show, Generic)

instance ToJSON CliqSlideGraphDataPoint where
  toJSON = genericToJSON $ zohoPrefix Casing.snakeCase

-- | Individual value within a graph data point.
data CliqSlideGraphValue = CliqSlideGraphValue
  { valueLabel :: !Text   -- ^ Label. Max 20 characters.
  , valueValue :: !Double -- ^ Numeric value (float).
  } deriving (Eq, Show, Generic)

instance ToJSON CliqSlideGraphValue where
  toJSON = genericToJSON $ zohoPrefix Casing.snakeCase

-- | Text slide - markdown text content
-- Max 500 chars
data CliqSlideText = CliqSlideText
  { textData :: !Text  -- ^ Text content (max 500 chars, markdown supported)
  } deriving (Eq, Show, Generic)

instance ToJSON CliqSlideText where
  toJSON CliqSlideText{..} = object
    [ "data" .= textData
    ]

-- | Union type for all slide types
data CliqSlide
  = SlideTable !CliqSlideTable
  | SlideFields !CliqSlideFields
  | SlideList !CliqSlideList
  | SlideLabel !CliqSlideLabel
  | SlideImages !CliqSlideImages
  | SlideChart !CliqSlideChart       -- ^ Pie/doughnut charts (percentage_chart)
  | SlideGraph !CliqSlideGraph       -- ^ Bar charts and trend lines (graph)
  | SlideText !CliqSlideText
  deriving (Eq, Show, Generic)

instance ToJSON CliqSlide where
  toJSON (SlideTable t) = addTypeField "table" $ toJSON t
  toJSON (SlideFields f) = addTypeField "fields" $ toJSON f
  toJSON (SlideList l) = addTypeField "list" $ toJSON l
  toJSON (SlideLabel l) = addTypeField "label" $ toJSON l
  toJSON (SlideImages i) = addTypeField "images" $ toJSON i
  toJSON (SlideChart c) = addTypeField "percentage_chart" $ toJSON c
  toJSON (SlideGraph g) = addTypeField "graph" $ toJSON g
  toJSON (SlideText t) = addTypeField "text" $ toJSON t

-- ============================================================================
-- End Slide Types
-- ============================================================================

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
-- WARNING: Cliq has a 5000 character limit for message text. Exceeding this will cause
-- "input_maxlength_reached" API errors. Truncate your text before calling this function.
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

-- | Smart constructor: Create a message with text and slides
textWithSlides :: Text -> [CliqSlide] -> CliqMessage
textWithSlides txt slides = CliqStandard $ CliqStandardMessage
  { csmText = Just txt
  , csmCard = Nothing
  , csmSlides = Just $ slidesToValue slides
  , csmButtons = Nothing
  , csmSuggestions = Nothing
  }

-- | Convert a list of slides to a JSON Value for use with csmSlides
slidesToValue :: [CliqSlide] -> Value
slidesToValue = toJSON

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
  , reqSyncMessage :: !(Maybe Bool)         -- ^ If True, API returns message_id synchronously (useful for edit-after-post flows)
  , reqMarkAsRead :: !(Maybe Bool)          -- ^ Mark message as read
  } deriving (Eq, Show, Generic)

instance ToJSON PostMessageReq where
  toJSON PostMessageReq{..} =
    let msgFields = case toJSON reqMessage of
          Object obj -> obj
          _ -> mempty
        extraFields = object $
          maybe [] (\x -> ["reply_to" .= x]) reqReplyTo ++
          maybe [] (\x -> ["sync_message" .= x]) reqSyncMessage ++
          maybe [] (\x -> ["mark_as_read" .= x]) reqMarkAsRead
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
data PostMessageResponse = PostMessageResponse
  { resMessageId :: !MessageId
  } deriving (Eq, Show, Generic)

instance FromJSON PostMessageResponse where
  parseJSON = genericParseJSON (zohoPrefixTyp Casing.snakeCase)

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
-- Returns Nothing if sync_message was not set (empty response), Just PostMessageResponse if sync_message=true
postMessageToChannel :: (ZM.HasZoho m)
                     => ChannelUniqueName
                     -> PostMessageReq
                     -> m (Either Error (Maybe PostMessageResponse))
postMessageToChannel channelName msgReq = ZM.runRequestAndParseOptionalResponse Nothing Just $ postMessageToChannelRequest channelName msgReq

-- | Post a message to a channel as a bot - Request builder
postMessageToChannelAsBotRequest :: ChannelUniqueName -> BotUniqueName -> PostMessageReq -> Request
postMessageToChannelAsBotRequest channelName botName msgReq =
  let endpoint = mkCliqEndpoint $ "/channelsbyname/" <> toS (rawChannelUniqueName channelName) <> "/message"
      queryParams = [("bot_unique_name", Just $ toS $ rawBotUniqueName botName)]
  in ZO.prepareJSONPost endpoint queryParams [] msgReq

-- | Post a message to a channel as a bot
-- Returns Nothing if sync_message was not set (empty response), Just PostMessageResponse if sync_message=true
postMessageToChannelAsBot :: (ZM.HasZoho m)
                          => ChannelUniqueName
                          -> BotUniqueName
                          -> PostMessageReq
                          -> m (Either Error (Maybe PostMessageResponse))
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
-- Returns Nothing if sync_message was not set (empty response), Just PostMessageResponse if sync_message=true
postMessageToUser :: (ZM.HasZoho m)
                  => Text  -- ^ Email ID
                  -> PostMessageReq
                  -> m (Either Error (Maybe PostMessageResponse))
postMessageToUser email msgReq = ZM.runRequestAndParseOptionalResponse Nothing Just $ postMessageToUserRequest email msgReq

-- | Edit a message - Request builder
-- Note: Zoho Cliq requires PUT (not PATCH) for editing messages
editMessageRequest :: ChatId -> MessageId -> EditMessageReq -> Request
editMessageRequest cid mid editReq =
  let endpoint = mkCliqEndpoint $ "/chats/" <> toS (rawChatId cid) <> "/messages/" <> toS (rawMessageId mid)
  in ZO.prepareJSONPut endpoint [] [] editReq

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
