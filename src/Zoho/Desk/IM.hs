module Zoho.Desk.IM where

import qualified Zoho.OAuth as ZO
import qualified Zoho.ZohoM as ZM
import Zoho.ZohoM (HasZoho)
import qualified Zoho.Desk.Common as Common
import Zoho.Types (OrgId, Error(..), zohoPrefix)
import Data.Aeson as Aeson
import qualified Data.Text as T 
import Data.Text (Text)
import GHC.Generics
import Data.Aeson.Casing as Casing
import Network.HTTP.Client (Request)
import Data.String.Conv (toS)

newtype ChannelId = ChannelId { rawChannelId :: Text } deriving (Eq, Show, Generic)

imJsonOptions :: Aeson.Options
imJsonOptions = zohoPrefix Casing.camelCase

data ZohoDeskInitiateIMSessionReq = ZohoDeskInitiateIMSessionReq
  { reqReceiverId          :: !Text         -- Phone number in E.164 format
  , reqReceiverType        :: !Text         -- Always "PHONENUMBER" for WhatsApp
  , reqCannedMessageId     :: !Text         -- Template message ID
  , reqLanguage            :: !Text         -- Language code (e.g. "en")
  , reqMessage             :: !Text         -- The message content
  , reqHeaderMessage       :: !(Maybe Text) -- optional header content
  } deriving (Show, Generic)

instance ToJSON ZohoDeskInitiateIMSessionReq where
    toJSON = genericToJSON imJsonOptions

initiateIMSessionRequest :: OrgId
                        -> ChannelId
                         -> ZohoDeskInitiateIMSessionReq
                         -> Request
initiateIMSessionRequest oid ChannelId{..} req =
  ZO.prepareJSONPost (Common.mkApiEndpoint $ "/im/channels/" <> toS rawChannelId <> "/initiateSession") [] [Common.orgIdHeader oid] req 

initiateIMSession :: (HasZoho m)
                  => OrgId
                  -> ChannelId
                  -> ZohoDeskInitiateIMSessionReq
                  -> m (Either Error Aeson.Value)
initiateIMSession oid chId req =
  ZM.runRequestAndParseResponse $ initiateIMSessionRequest oid chId req
