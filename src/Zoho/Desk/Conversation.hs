{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
module Zoho.Desk.Conversation
  ( module Zoho.Desk.Conversation
  , module Common
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Zoho.Types
import GHC.Generics (Generic)
import qualified Data.Aeson.Casing as Casing
import Data.Aeson as Aeson
import Control.Lens (makeLensesWith, abbreviatedFields, (&), (?~))
import Zoho.OAuth as ZO
import Network.HTTP.Client as HC (Request)
import Zoho.Desk.Common as Common
import Zoho.ZohoM as ZM
import Data.String.Conv (toS)
import Control.Monad (join)
import Text.Read (readMaybe)

-- | Type of conversation entry
data ConversationType = ConvThread | ConvComment
  deriving (Eq, Show, Generic)

instance ToJSON ConversationType where
  toJSON = genericToJSON $ constructorDrop 4 Casing.camelCase

instance FromJSON ConversationType where
  parseJSON = genericParseJSON $ constructorDrop 4 Casing.camelCase

-- | Author information for threads and comments
data ConversationAuthor = ConversationAuthor
  { authorFirstName :: !(Maybe Text)
  , authorLastName :: !(Maybe Text)
  , authorName :: !(Maybe Text)
  , authorEmail :: !(Maybe Text)
  , authorTyp :: !(Maybe Text)  -- "END_USER" | "AGENT"
  , authorPhotoURL :: !(Maybe Text)
  , authorRoleName :: !(Maybe Text)  -- Only for agents
  } deriving (Eq, Show, Generic)

instance FromJSON ConversationAuthor where
  parseJSON = genericParseJSON $ zohoPrefixTyp Casing.camelCase

instance ToJSON ConversationAuthor where
  toJSON = genericToJSON $ zohoPrefixTyp Casing.camelCase


-- | Unified conversation entry (all fields at top level as per API)
data ConversationEntryPoly attachCnt = ConversationEntryPoly
  { convId :: !(Maybe Text)
  , convTyp :: !ConversationType
  , convSummary :: !(Maybe Text)      -- Main content
  , convContent :: !(Maybe Text)      -- HTML content (comments use this)
  , convAuthor :: !(Maybe ConversationAuthor)
  , convCreatedTime :: !(Maybe UTCTime)
  , convCanReply :: !(Maybe Bool)
  -- Thread-specific fields (present when type = "thread")
  , convIsDescriptionThread :: !(Maybe Bool)
  , convVisibility :: !(Maybe Text)  -- "public" | "private"
  , convChannel :: !(Maybe Text)     -- "EMAIL" | "FORUMS" | "CHAT" etc.
  , convDirection :: !(Maybe Text)   -- "in" | "out"
  , convStatus :: !(Maybe Text)      -- "SUCCESS" | "FAILED" | "DRAFT"
  , convFromEmailAddress :: !(Maybe Text)
  , convTo :: !(Maybe Text)
  , convCc :: !(Maybe Text)
  , convBcc :: !(Maybe Text)
  , convReplyTo :: !(Maybe Text)
  , convIsForward :: !(Maybe Bool)
  , convHasAttach :: !(Maybe Bool)
  , convAttachmentCount :: !(Maybe attachCnt)
  , convContentType :: !(Maybe Text) -- "text/html"
  , convResponderId :: !(Maybe Text)
  , convRespondedIn :: !(Maybe Text) -- Duration like "00:05:14"
  -- Comment-specific fields (present when type = "comment")
  , convCommenterId :: !(Maybe Text)
  , convCommentedTime :: !(Maybe UTCTime)
  , convModifiedTime :: !(Maybe UTCTime)
  , convIsPublic :: !(Maybe Bool)
  } deriving (Eq, Show, Generic)

type ConversationEntry = ConversationEntryPoly Int

instance FromJSON (ConversationEntryPoly Int) where
  parseJSON v = fmap doConversion (genericParseJSON (zohoPrefixTyp Casing.camelCase) v)
    where
      doConversion :: ConversationEntryPoly String -> ConversationEntryPoly Int
      doConversion x = x { convAttachmentCount = join $ fmap readMaybe (convAttachmentCount x) }

instance ToJSON (ConversationEntryPoly Int) where
  toJSON = genericToJSON $ zohoPrefixTyp Casing.camelCase

$(makeLensesWith abbreviatedFields ''ConversationAuthor)
$(makeLensesWith abbreviatedFields ''ConversationEntryPoly)

-- | List options for conversations API
data ConversationListOptions = ConversationListOptions
  { optFrom :: !(Maybe Int)
  , optLimit :: !(Maybe Int)
  , optInclude :: !(Maybe Text)  -- "mentions,source" or other combinations
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

$(makeLensesWith abbreviatedFields ''ConversationListOptions)

emptyConversationListOptions :: ConversationListOptions
emptyConversationListOptions = emptyZohoStructure

-- | Build request for listing conversations
listConversationsRequest :: OrgId
                         -> ConversationListOptions  
                         -> TicketId
                         -> Request
listConversationsRequest oid ConversationListOptions{..} tid =
  ZO.prepareGet (Common.mkApiEndpoint $ "/tickets/" <> toS tid <> "/conversations") params [Common.orgIdHeader oid]
  where
    params =
      applyOptionalQueryParam "include" optInclude $
      applyOptionalQueryParam "limit" (show <$> optLimit) $
      applyOptionalQueryParam "from" (show <$> optFrom)
      []

-- | List all conversations for a ticket (threads + comments)
listConversations :: forall m . (HasZoho m)
                  => OrgId
                  -> ConversationListOptions
                  -> TicketId  
                  -> m (Either Error [ConversationEntry])
listConversations oid listOpts tid = do
  x :: Either Error (ResponseWrapper "data" [ConversationEntry]) <-
    ZM.runRequestAndParseOptionalResponse (ResponseWrapper []) Prelude.id $
    listConversationsRequest oid listOpts tid
  pure $ fmap unwrapResponse x