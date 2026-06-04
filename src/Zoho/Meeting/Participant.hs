{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Zoho Meeting participant report. Endpoint:
-- @/api/v2/{zsoid}/participant/{meetingKey}.json?index&count@ (index is REQUIRED).
-- Field shapes verified against the live response (camelCase keys; the snake_case
-- @encrypt_email@ / @ip_address@ extras are intentionally not modelled).
module Zoho.Meeting.Participant where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Maybe (fromMaybe)
import Data.String.Conv (toS)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (Request)
import Zoho.Meeting.Common (MeetingKey (..), MeetingOrgId (..), mkApiEndpoint)
import qualified Zoho.OAuth as ZO
import Zoho.Types (EmptyZohoStructure (..), Error)
import Zoho.ZohoM as ZM (HasZoho, runRequestAndParseResponse)

data Participant = Participant
  { partEmail             :: !(Maybe Text)
  , partRole              :: !(Maybe Text)    -- ^ e.g. @presenter@ / @attendee@
  , partJoinTime          :: !(Maybe Integer) -- ^ epoch ms
  , partLeaveTime         :: !(Maybe Integer) -- ^ epoch ms
  , partDuration          :: !(Maybe Integer) -- ^ ms
  , partInAndOutTime      :: !(Maybe Text)
  , partMemberId          :: !(Maybe Text)
  , partParticipantAvatar :: !(Maybe Text)
  , partSource            :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance ToJSON Participant where
  toJSON = genericToJSON (aesonPrefix camelCase){omitNothingFields = True}

instance FromJSON Participant where
  parseJSON = genericParseJSON (aesonPrefix camelCase)

data ParticipantReport = ParticipantReport
  { prParticipantsCount :: !(Maybe Int)
  , prParticipants      :: ![Participant]
  } deriving (Eq, Show, Generic)

instance FromJSON ParticipantReport where
  parseJSON = withObject "ParticipantReport" $ \o -> do
    prParticipantsCount <- o .:? "participantsCount"
    prParticipants      <- o .:? "participants" .!= []
    pure ParticipantReport{..}

data ListOpts = ListOpts
  { optsIndex :: !(Maybe Int)  -- ^ starting offset (required by the API; defaults to 1)
  , optsCount :: !(Maybe Int)  -- ^ page size (defaults to 50)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

defaultListOpts :: ListOpts
defaultListOpts = emptyZohoStructure

-- | @index@ is required by the API (a missing one 400s), so always send it; default
-- index=1, count=50.
listRequest :: MeetingOrgId -> MeetingKey -> ListOpts -> Request
listRequest orgId (MeetingKey k) ListOpts{..} =
  let params = ZO.applyOptionalQueryParam "index" (Just (show (fromMaybe 1 optsIndex)))
             $ ZO.applyOptionalQueryParam "count" (Just (show (fromMaybe 50 optsCount))) []
  in ZO.prepareGet (mkApiEndpoint orgId ("/participant/" <> toS k <> ".json")) params []

list :: forall m. (HasZoho m) => MeetingOrgId -> MeetingKey -> ListOpts -> m (Either Error ParticipantReport)
list orgId mk opts = runRequestAndParseResponse (listRequest orgId mk opts)
