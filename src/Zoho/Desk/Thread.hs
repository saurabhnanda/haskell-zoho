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
import Data.Aeson.TH as Aeson
import Control.Lens (makeLensesWith, abbreviatedFields, (&), (?~))
import Zoho.Desk.Utils (threadStatusJsonOptions, threadJsonOptions)
import Zoho.OAuth as ZO
import Network.HTTP.Client as HC (Request)
import Zoho.Desk.Common as Common
import Zoho.ZohoM as ZM
import Data.String.Conv (toS)
import Control.Monad (join)
import Text.Read (readMaybe)

data Visibility = VisiblePublic
                | VisiblePrivate
                | VisibleOther !Text
                deriving (Eq, Show)

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
            deriving (Eq, Show, Enum, Ord)

$(deriveJSON threadStatusJsonOptions ''Status)

data Direction = In | Out deriving (Eq, Show, Enum, Ord)

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

data ContentType = PlainText
                 | Html
                 | ContentTypeOther !Text
                 deriving (Eq, Show, Ord)

instance ToJSON ContentType where
  toJSON x = toJSON $ case x of
    PlainText -> "plainText"
    Html -> "html"
    ContentTypeOther s -> s

instance FromJSON ContentType where
  parseJSON = withText "Expecting Text to parse into ContentType" $ \t ->
    pure $ case t of
    "plainText" -> PlainText
    "html" -> Html
    x -> ContentTypeOther x


-- TODO: Use proper email parser for threadCc, threadBcc, and threadReplyTo --
-- else this will break on something like the following due to presence of
-- comma:
--
--   "Manager, Support" <managersupport@vacationlabs.com>
--

data ThreadPoly stringInt textList = Thread
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
  , threadCc :: !(Maybe textList)
  , threadBcc :: !(Maybe textList)
  , threadIsContentTruncated :: !(Maybe Bool)
  , threadFullContentURL :: !(Maybe Text)
  , threadContentType :: !(Maybe ContentType)
  , threadContent :: !(Maybe Text)
  , threadIsForward :: !(Maybe Bool)
  , threadReplyTo :: !(Maybe textList)
  , threadPlainText :: !(Maybe Text)
  , threadDirection :: !(Maybe Direction)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

type Thread = ThreadPoly Int [Text]

emptyThread :: Thread
emptyThread = emptyZohoStructure

$(makeLensesWith abbreviatedFields ''ThreadPoly)

instance (FromJSON stringInt, FromJSON textList) => FromJSON (ThreadPoly stringInt textList) where
  parseJSON = genericParseJSON threadJsonOptions

instance {-# OVERLAPS #-} FromJSON (ThreadPoly Int [Text]) where
  parseJSON v = fmap doConversion (genericParseJSON threadJsonOptions v)
    where
      doConversion :: ThreadPoly String Text -> ThreadPoly Int [Text]
      doConversion x@Thread{..} =
        x { threadAttachmentCount = join $ fmap readMaybe threadAttachmentCount
          , threadCc = fmap ((fmap T.strip) . (T.splitOn ",")) threadCc
          , threadBcc = fmap ((fmap T.strip) . (T.splitOn ",")) threadBcc
          , threadReplyTo = fmap ((fmap T.strip) . (T.splitOn ",")) threadReplyTo
          }

instance (ToJSON stringInt, ToJSON textList) => ToJSON (ThreadPoly stringInt textList) where
  toJSON = genericToJSON threadJsonOptions

instance {-# OVERLAPS #-} ToJSON (ThreadPoly Int [Text]) where
  toJSON = (genericToJSON threadJsonOptions) . doConversion
    where
      doConversion :: ThreadPoly Int [Text] -> ThreadPoly String Text
      doConversion x@Thread{..} =
        x { threadAttachmentCount = show <$> threadAttachmentCount
          , threadCc = fmap (T.intercalate ",") threadCc
          , threadBcc = fmap (T.intercalate ",") threadBcc
          , threadReplyTo = fmap (T.intercalate ",") threadReplyTo
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
               -> [ToEmailAddress]
               -> Content
               -> Thread
               -> m (Either Error Thread)
sendEmailReply oid tid from_ to_ cc_ content_ a =
  create oid tid $ a
  & channel ?~ "EMAIL"
  & to ?~ to_
  & cc ?~ cc_
  & fromEmailAddress ?~ from_
  & content ?~ content_
