{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Zoho Projects: list projects in a portal.
--
-- Used to resolve/verify the target project id that meeting action-item tasks land in.
-- Only the commonly-needed fields are modelled.
module Zoho.Projects.Project where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (Request)
import Zoho.Projects.Common (PortalId, ProjectId, ListOptions, mkApiEndpoint, listQueryParams)
import qualified Zoho.OAuth as ZO
import Zoho.Types (Error)
import Zoho.ZohoM as ZM (HasZoho, runRequestAndParseResponse)

data Project = Project
  { projId   :: !(Maybe ProjectId)
  , projName :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance FromJSON Project where
  parseJSON = genericParseJSON (aesonPrefix snakeCase)

instance ToJSON Project where
  toJSON = genericToJSON (aesonPrefix snakeCase){omitNothingFields = True}

listRequest :: PortalId -> ListOptions -> Request
listRequest portalId opts =
  ZO.prepareGet (mkApiEndpoint portalId "/projects") (listQueryParams opts) []

-- | List projects in the portal. Unlike the tasklist/task endpoints, v3 @GET /projects@
-- returns a bare JSON array (no @{projects:[...]}@ wrapper).
list :: forall m. (HasZoho m) => PortalId -> ListOptions -> m (Either Error [Project])
list portalId opts = runRequestAndParseResponse (listRequest portalId opts)
