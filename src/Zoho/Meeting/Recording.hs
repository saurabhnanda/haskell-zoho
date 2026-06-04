{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Zoho Meeting recordings: types + endpoints.
--
-- Field shapes are taken from the live @recordings.json@ response (camelCase keys),
-- so codecs use @aesonPrefix camelCase@ rather than the @zohoPrefix snakeCase@ that
-- Books/Desk use. Only the commonly-needed fields are modelled; the snake/dup
-- oddballs in the raw payload (@short_meeting_key@, @FileSize@) are skipped (the
-- lowercase @fileSize@ covers it).
--
-- The list response @{recordings, meta:{moreRecords,count}}@ is parsed by the lib's
-- 'PaginatedResponse' (its parser handles the @meta@/@moreRecords@ envelope).
module Zoho.Meeting.Recording where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.String.Conv (toS)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (Request)
import Zoho.Meeting.Common (MeetingOrgId (..), MeetingKey (..), mkApiEndpoint)
import qualified Zoho.OAuth as ZO
import Zoho.Types (Error, PaginatedResponse (..), ResponseWrapper (..), EmptyZohoStructure (..), parseTextOrNumber)
import Zoho.ZohoM as ZM (HasZoho, runRequestAndParseResponse)

newtype ERecordingId = ERecordingId { rawERecordingId :: Text }
  deriving (Eq, Show, Generic, Ord)
  deriving (ToJSON) via Text
  deriving (FromJSON) via Text

newtype RecordingId = RecordingId { rawRecordingId :: Text }
  deriving (Eq, Show, Generic, Ord)
  deriving (ToJSON) via Text

-- Zoho is inconsistent: the list endpoint returns @recordingId@ as a String, the
-- get-specific endpoint returns it as a Number. Accept both (Common.parseTextOrNumber).
instance FromJSON RecordingId where
  parseJSON v = RecordingId <$> parseTextOrNumber v

data Recording = Recording
  { recErecordingId             :: !(Maybe ERecordingId)
  , recRecordingId              :: !(Maybe RecordingId)
  , recMeetingKey               :: !(Maybe MeetingKey)
  , recTopic                    :: !(Maybe Text)
  , recCreatorName              :: !(Maybe Text)
  , recDatenTime                :: !(Maybe Text)     -- ^ Zoho key is @datenTime@ (sic)
  , recSDate                    :: !(Maybe Text)
  , recSTime                    :: !(Maybe Text)
  , recDuration                 :: !(Maybe Int)      -- ^ milliseconds
  , recDurationInMins           :: !(Maybe Int)
  , recStartTimeinMs            :: !(Maybe Integer)
  , recFileSize                 :: !(Maybe Text)     -- ^ e.g. @"593 MB"@
  , recResourceName             :: !(Maybe Text)     -- ^ the .mp4 filename
  , recStatus                   :: !(Maybe Text)     -- ^ e.g. @UPLOADED@
  , recIsMeeting                :: !(Maybe Bool)
  , recDownloadUrl              :: !(Maybe Text)     -- ^ pre-signed; authorized GET -> mp4 bytes
  , recPlayUrl                  :: !(Maybe Text)
  , recShareUrl                 :: !(Maybe Text)
  , recIsTranscriptGenerated    :: !(Maybe Bool)
  , recTranscriptionDownloadUrl :: !(Maybe Text)
  , recIsSummaryGenerated       :: !(Maybe Bool)
  , recSummaryDownloadUrl       :: !(Maybe Text)
  , recOpenAIStatus             :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance ToJSON Recording where
  toJSON = genericToJSON (aesonPrefix camelCase){omitNothingFields = True}

instance FromJSON Recording where
  parseJSON = genericParseJSON (aesonPrefix camelCase)

-- | Pagination options for 'list'. Verified against the live API: @index@ is the
-- starting offset, @count@ the page size (default 20). The response @meta.moreRecords@
-- / @meta.count@ drive paging (surfaced via 'PaginatedResponse').
data ListOpts = ListOpts
  { optsIndex :: !(Maybe Int)  -- ^ starting offset
  , optsCount :: !(Maybe Int)  -- ^ page size (default 20 server-side)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

defaultListOpts :: ListOpts
defaultListOpts = emptyZohoStructure

listRequest :: MeetingOrgId -> ListOpts -> Request
listRequest zsoid ListOpts{..} =
  let params = ZO.applyOptionalQueryParam "index" (show <$> optsIndex)
             $ ZO.applyOptionalQueryParam "count" (show <$> optsCount) []
  in ZO.prepareGet (mkApiEndpoint zsoid "/recordings.json") params []

-- | List recordings for the org. Mirrors @Zoho.Books.Invoice.list@.
list :: forall m. (HasZoho m) => MeetingOrgId -> ListOpts -> m (Either Error (PaginatedResponse "recordings" [Recording]))
list zsoid opts = runRequestAndParseResponse (listRequest zsoid opts)

getByMeetingKeyRequest :: MeetingOrgId -> MeetingKey -> Request
getByMeetingKeyRequest zsoid (MeetingKey k) =
  ZO.prepareGet (mkApiEndpoint zsoid ("/recordings/" <> toS k <> ".json")) [] []

-- | Recordings for a single meeting key. The endpoint returns the same
-- @{recordings:[...], count}@ envelope as 'list' (a meeting can have multiple
-- recordings, and there is NO @meta@ here -- @count@ is top-level), so we read just
-- the @recordings@ array via 'ResponseWrapper' and return the list.
getByMeetingKey :: forall m. (HasZoho m) => MeetingOrgId -> MeetingKey -> m (Either Error [Recording])
getByMeetingKey zsoid mk =
  runRequestAndParseResponse (getByMeetingKeyRequest zsoid mk) >>= \case
    Left e -> pure $ Left e
    Right (ResponseWrapper recs :: ResponseWrapper "recordings" [Recording]) -> pure $ Right recs
