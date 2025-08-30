{-# LANGUAGE DeriveAnyClass #-}
module Zoho.Desk.Thread
  ( module Zoho.Desk.Thread
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
import Data.Char (toUpper)
import UnliftIO (MonadIO, liftIO, catch, SomeException, handle)


data Visibility = VisiblePublic
                | VisiblePrivate
                | VisibleOther !Text
                deriving (Eq, Show, Generic)

instance ToJSON Visibility where
  toJSON v = toJSON $ case v of
    VisiblePublic -> "public"
    VisiblePrivate -> "private"
    VisibleOther x -> x

instance FromJSON Visibility where
  parseJSON = withText "Expecting Text to parse into Visibility" $ \t -> do
    pure $ case t of
      "public" -> VisiblePublic
      "private" -> VisiblePrivate
      x -> VisibleOther x

data Status = Success
            | Failed
            | Pending
            | Draft
            deriving (Eq, Show, Enum, Ord, Generic)

instance ToJSON Status where
  toJSON = genericToJSON $ defaultOptions
    { constructorTagModifier = map toUpper
    }

instance FromJSON Status where
  parseJSON = genericParseJSON $ defaultOptions
    { constructorTagModifier = map toUpper
    }

data Direction = In | Out deriving (Eq, Show, Enum, Ord, Generic)

instance ToJSON Direction where
  toJSON x = case x of
    In -> "in"
    Out -> "out"

instance FromJSON Direction where
  parseJSON = withText "Expecting Text to parse into Direction" $ \t ->
    case t of
      "in" -> pure In
      "out" -> pure Out
      "inbound" -> pure In
      "outbound" -> pure Out
      x -> fail $ "Unexpected " <> show x

-- TODO: Use proper email parser for threadCc, threadBcc, and threadReplyTo --
-- else this will break on something like the following due to presence of
-- comma:
--
--   "Manager, Support" <managersupport@vacationlabs.com>
--

data ThreadPoly stringInt = Thread
  { threadId :: !(Maybe Text)
  , threadIsDescriptionThread :: !(Maybe Bool)
  , threadVisibility :: !(Maybe Visibility)
  , threadAuthor :: !(Maybe OmitField)
  , threadChannel :: !(Maybe Text)
  , threadHasAttach :: !(Maybe Bool)
  , threadChannelRelatedInfo :: !(Maybe OmitField)
  , threadCreatedTime :: !(Maybe UTCTime)
  , threadAttachmentCount :: !(Maybe stringInt)
  , threadFromEmailAddress :: !(Maybe Text)
  , threadActions :: !(Maybe OmitField)
  , threadStatus :: !(Maybe Status)
  , threadSummary :: !(Maybe Text)
  , threadAttachments :: !(Maybe OmitField) -- TODO
  , threadTo :: !(Maybe Text)
  , threadCc :: !(Maybe Text)
  , threadBcc :: !(Maybe Text)
  , threadIsContentTruncated :: !(Maybe Bool)
  , threadFullContentURL :: !(Maybe Text)
  , threadContentType :: !(Maybe ContentType)
  , threadContent :: !(Maybe Text)
  , threadIsForward :: !(Maybe Bool)
  , threadReplyTo :: !(Maybe Text)
  , threadPlainText :: !(Maybe Text)
  , threadDirection :: !(Maybe Direction)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

type Thread = ThreadPoly Int

emptyThread :: Thread
emptyThread = emptyZohoStructure

$(makeLensesWith abbreviatedFields ''ThreadPoly)


instance FromJSON (ThreadPoly Int) where
  parseJSON v = fmap doConversion (genericParseJSON (zohoPrefix Casing.camelCase) v)
    where
      doConversion :: ThreadPoly String -> ThreadPoly Int
      doConversion x@Thread{..} =
        x { threadAttachmentCount = join $ fmap readMaybe threadAttachmentCount
          -- Keep email fields as raw comma-separated strings - no parsing needed
          -- threadCc, threadBcc, threadReplyTo remain as Text
          }

instance ToJSON (ThreadPoly Int) where
  toJSON = (genericToJSON $ zohoPrefix Casing.camelCase) . doConversion
    where
      doConversion :: ThreadPoly Int -> ThreadPoly String
      doConversion x@Thread{..} =
        x { threadAttachmentCount = show <$> threadAttachmentCount
          -- Email fields already Text, no conversion needed
          }



-- * List threads

data ListOptions = ListOptions
  { optFrom :: !(Maybe Int)
  , optLimit :: !(Maybe Int)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

$(makeLensesWith abbreviatedFields ''ListOptions)

emptyListOptions :: ListOptions
emptyListOptions = emptyZohoStructure

listRequest :: OrgId
            -> ListOptions
            -> TicketId
            -> Request
listRequest oid ListOptions{..} tid =
  ZO.prepareGet (Common.mkApiEndpoint $ "/tickets/" <> toS tid <> "/threads") params [Common.orgIdHeader oid]
  where
    params =
      applyOptionalQueryParam "limit" (show <$> optLimit) $
      applyOptionalQueryParam "from" (show <$> optFrom)
      []



list :: forall m . (HasZoho m)
     => OrgId
     -> ListOptions
     -> TicketId
     -> m (Either Error [Thread])
list oid listOpts tid = do
  x :: Either Error (ResponseWrapper "data" [Thread]) <-
    ZM.runRequestAndParseOptionalResponse (ResponseWrapper []) Prelude.id $
    listRequest oid listOpts tid
  pure $ fmap unwrapResponse x


-- * Get single thread

getRequest :: OrgId
           -> TicketId
           -> ThreadId
           -> Request
getRequest oid tid thid =
  ZO.prepareGet (Common.mkApiEndpoint $ "/tickets/" <> toS tid <> "/threads/" <> toS thid) params [Common.orgIdHeader oid]
  where
    params = 
      applyOptionalQueryParam "include" (Just "plainText" :: Maybe Text) []


get :: forall m . (HasZoho m)
    => OrgId
    -> TicketId
    -> ThreadId
    -> m (Either Error Thread)
get oid tid thid =
  ZM.runRequestAndParseResponse $
  getRequest oid tid thid


createRequest :: OrgId
              -> TicketId
              -> Thread
              -> Request
createRequest oid tid a =
  ZO.prepareJSONPost (Common.mkApiEndpoint $ "/tickets/" <> toS tid <> "/sendReply") [] [Common.orgIdHeader oid] a

create :: (HasZoho m)
       => OrgId
       -> TicketId
       -> Thread
       -> m (Either Error Thread)
create oid tid a =
  ZM.runRequestAndParseResponse $
  createRequest oid tid a


--
-- TODO: Use the Mailbox type instead
-- https://hackage.haskell.org/package/email-header-0.4.1/docs/Network-Email-Header-Parser.html#v:mailbox
--
type FromEmailAddress = Text
type ToEmailAddress = Text
type Content = Text
sendEmailReply :: (HasZoho m)
               => OrgId
               -> TicketId
               -> FromEmailAddress
               -> ToEmailAddress
               -> [ToEmailAddress]  -- Still accept list for API convenience
               -> Content
               -> Thread
               -> m (Either Error Thread)
sendEmailReply oid tid from_ to_ cc_ content_ a =
  create oid tid $ a
  & channel ?~ "EMAIL"
  & to ?~ to_
  & cc ?~ T.intercalate "," cc_  -- Convert list to comma-separated string
  & fromEmailAddress ?~ from_
  & content ?~ content_
