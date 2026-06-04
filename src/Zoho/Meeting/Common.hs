{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Shared endpoint helper + identifiers for the Zoho Meeting REST API.
--
-- All Meeting endpoints (recordings, sessions, participants) live on the single base
-- @https://meeting.zoho.com/api/v2/{zsoid}@.
module Zoho.Meeting.Common where

import Data.Aeson
import qualified Data.ByteString as BS
import Data.Text (Text)
import GHC.Generics
import qualified Zoho.OAuth as ZO
import Zoho.Types (parseTextOrNumber)
import URI.ByteString as U

-- | Zoho Meeting organization id (path segment, e.g. @709505673@). Zoho's API calls
-- this @zsoid@. A distinct newtype on purpose: each Zoho product has its OWN org-id
-- value (Books/Desk/Subscriptions/Meeting all differ), so this must not be
-- interchangeable with the generic 'Zoho.Types.OrgId' that Books/Desk use.
newtype MeetingOrgId = MeetingOrgId { rawMeetingOrgId :: BS.ByteString }
  deriving (Eq, Show, Generic)

-- | A meeting key. Zoho serializes ids inconsistently (String in some responses,
-- Number in others), so FromJSON accepts both via 'parseTextOrNumber'.
newtype MeetingKey = MeetingKey { rawMeetingKey :: Text }
  deriving (Eq, Show, Generic, Ord)
  deriving (ToJSON) via Text

instance FromJSON MeetingKey where
  parseJSON v = MeetingKey <$> parseTextOrNumber v

-- | @https://meeting.zoho.com/api/v2/{zsoid}{path}@ -- the single base for all Meeting
-- endpoints (recordings, sessions, participants).
mkApiEndpoint :: MeetingOrgId -> BS.ByteString -> URI
mkApiEndpoint MeetingOrgId{rawMeetingOrgId} p =
  ZO.mkEndpoint (Host "meeting.zoho.com") ("/api/v2/" <> rawMeetingOrgId <> p)
