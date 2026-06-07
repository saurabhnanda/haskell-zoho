{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Zoho Projects task layout / custom-field metadata.
--
-- The custom-field metadata (mapping a field's @display_name@ to its API @column_name@,
-- e.g. @UDF_CHAR1@) lives on the older @/restapi/@ base, not v3. 'rawGet' returns the
-- raw response so the nested shape can be inspected before a typed model is committed.
module Zoho.Projects.Tasklayout where

import Data.Aeson (Value)
import Data.String.Conv (toS)
import Network.HTTP.Client (Request)
import Zoho.Projects.Common (PortalId, ProjectId (..), mkRestApiEndpoint)
import qualified Zoho.OAuth as ZO
import Zoho.Types (Error)
import Zoho.ZohoM as ZM (HasZoho, runRequestAndParseResponse)

rawGetRequest :: PortalId -> ProjectId -> Request
rawGetRequest portalId (ProjectId pid) =
  ZO.prepareGet (mkRestApiEndpoint portalId ("/projects/" <> toS pid <> "/tasklayouts")) [] []

-- | Fetch the task-layout metadata as a raw JSON 'Value'. Used to discover custom-field
-- @column_name@s (and, later, to resolve them dynamically by @display_name@).
rawGet :: forall m. (HasZoho m) => PortalId -> ProjectId -> m (Either Error Value)
rawGet portalId projectId = runRequestAndParseResponse (rawGetRequest portalId projectId)
