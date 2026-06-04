{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- | Zoho Meeting session (meeting) detail. Endpoint lives on the @/api/v2/@ base (NOT
-- the @/meeting/api/v2/@ base that recordings use). Field shapes verified against the
-- live @sessions/{meetingKey}.json@ response (camelCase keys; @duration@ is a String
-- here, unlike the recordings endpoint where it is a Number).
module Zoho.Meeting.Session where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.String.Conv (toS)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (Request)
import Zoho.Meeting.Common (MeetingKey (..), MeetingOrgId (..), mkApiEndpoint)
import qualified Zoho.OAuth as ZO
import Zoho.Types (Error, ResponseWrapper (..))
import Zoho.ZohoM as ZM (HasZoho, runRequestAndParseResponse)

data Session = Session
  { sessMeetingKey                :: !(Maybe MeetingKey)
  , sessTopic                     :: !(Maybe Text)
  , sessAgenda                    :: !(Maybe Text)
  , sessPresenterEmail            :: !(Maybe Text)
  , sessPresenterName             :: !(Maybe Text)
  , sessPresenter                 :: !(Maybe Text)   -- ^ presenter zuid (as String)
  , sessStartTime                 :: !(Maybe Text)
  , sessEndTime                   :: !(Maybe Text)
  , sessDuration                  :: !(Maybe Text)   -- ^ ms, but serialized as String here
  , sessTimezone                  :: !(Maybe Text)
  , sessType                      :: !(Maybe Text)
  , sessStatus                    :: !(Maybe Int)
  , sessEventId                   :: !(Maybe Text)
  , sessIsPastSession             :: !(Maybe Bool)
  , sessTranscriptionLanguageCode :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance ToJSON Session where
  toJSON = genericToJSON (aesonPrefix camelCase){omitNothingFields = True}

instance FromJSON Session where
  parseJSON = genericParseJSON (aesonPrefix camelCase)

getRequest :: MeetingOrgId -> MeetingKey -> Request
getRequest zsoid (MeetingKey k) =
  ZO.prepareGet (mkApiEndpoint zsoid ("/sessions/" <> toS k <> ".json")) [] []

-- | Fetch a single meeting's detail. Wrapper key is @session@ (singular).
get :: forall m. (HasZoho m) => MeetingOrgId -> MeetingKey -> m (Either Error Session)
get zsoid mk =
  runRequestAndParseResponse (getRequest zsoid mk) >>= \case
    Left e -> pure $ Left e
    Right (ResponseWrapper s :: ResponseWrapper "session" Session) -> pure $ Right s
