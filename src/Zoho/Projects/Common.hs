{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Shared endpoint helper + identifiers for the Zoho Projects v3 REST API.
--
-- All Projects endpoints live under the portal-scoped base
-- @https://projectsapi.zoho.com/api/v3/portal/{portalId}@.
module Zoho.Projects.Common where

import Data.Aeson
import qualified Data.ByteString as BS
import Data.Text (Text)
import GHC.Generics
import qualified Zoho.OAuth as ZO
import Zoho.Types (parseTextOrNumber, EmptyZohoStructure (..))
import URI.ByteString as U

-- | Zoho Projects portal id (path segment, e.g. @60016806987@). "Portal" is exclusive
-- to Zoho Projects, so the unqualified name is unambiguous (unlike 'Zoho.Types.OrgId',
-- which Books/Desk share and 'MeetingOrgId' must disambiguate from).
newtype PortalId = PortalId { rawPortalId :: BS.ByteString }
  deriving (Eq, Show, Generic)

-- | A project id. Zoho serializes ids inconsistently (String @id_string@ in some
-- responses, Number @id@ in others), so FromJSON accepts both via 'parseTextOrNumber'.
newtype ProjectId = ProjectId { rawProjectId :: Text }
  deriving (Eq, Show, Generic, Ord)
  deriving (ToJSON) via Text

instance FromJSON ProjectId where
  parseJSON v = ProjectId <$> parseTextOrNumber v

newtype TasklistId = TasklistId { rawTasklistId :: Text }
  deriving (Eq, Show, Generic, Ord)
  deriving (ToJSON) via Text

instance FromJSON TasklistId where
  parseJSON v = TasklistId <$> parseTextOrNumber v

newtype TaskId = TaskId { rawTaskId :: Text }
  deriving (Eq, Show, Generic, Ord)
  deriving (ToJSON) via Text

instance FromJSON TaskId where
  parseJSON v = TaskId <$> parseTextOrNumber v

-- | @https://projectsapi.zoho.com/api/v3/portal/{portalId}{path}@ -- the single base for
-- all Projects endpoints (projects, tasklists, tasks).
mkApiEndpoint :: PortalId -> BS.ByteString -> URI
mkApiEndpoint PortalId{rawPortalId} p =
  ZO.mkEndpoint (Host "projectsapi.zoho.com") ("/api/v3/portal/" <> rawPortalId <> p)

-- | The older @/restapi/@ (v1/v2) base, needed for endpoints not yet on v3 -- notably the
-- task-layout / custom-field metadata endpoint.
mkRestApiEndpoint :: PortalId -> BS.ByteString -> URI
mkRestApiEndpoint PortalId{rawPortalId} p =
  ZO.mkEndpoint (Host "projectsapi.zoho.com") ("/restapi/portal/" <> rawPortalId <> p)

-- | Pagination shared by every Projects @list@ endpoint (the v3 API uses uniform
-- @page@ / @per_page@ query params). An all-'Nothing' value relies on server defaults.
data ListOptions = ListOptions
  { loptsPage    :: !(Maybe Int)
  , loptsPerPage :: !(Maybe Int)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

emptyListOptions :: ListOptions
emptyListOptions = emptyZohoStructure

-- | Render 'ListOptions' into the @page@ / @per_page@ query params for a GET request.
listQueryParams :: ListOptions -> [(BS.ByteString, Maybe BS.ByteString)]
listQueryParams ListOptions{..} =
  ZO.applyOptionalQueryParam "page" (show <$> loptsPage) $
  ZO.applyOptionalQueryParam "per_page" (show <$> loptsPerPage) []
