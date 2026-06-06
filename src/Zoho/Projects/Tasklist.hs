{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Zoho Projects: list task lists within a project.
--
-- Used to resolve the target tasklist id that new tasks are created under (v3 task
-- creation requires a tasklist when the project has no default).
module Zoho.Projects.Tasklist where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.String.Conv (toS)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (Request)
import Zoho.Projects.Common (PortalId, ProjectId (..), TasklistId, ListOptions, mkApiEndpoint, listQueryParams)
import qualified Zoho.OAuth as ZO
import Zoho.Types (Error, ResponseWrapper (..))
import Zoho.ZohoM as ZM (HasZoho, runRequestAndParseResponse)

data Tasklist = Tasklist
  { tlId   :: !(Maybe TasklistId)
  , tlName :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance FromJSON Tasklist where
  parseJSON = genericParseJSON (aesonPrefix snakeCase)

instance ToJSON Tasklist where
  toJSON = genericToJSON (aesonPrefix snakeCase){omitNothingFields = True}

listRequest :: PortalId -> ProjectId -> ListOptions -> Request
listRequest portalId (ProjectId pid) opts =
  ZO.prepareGet (mkApiEndpoint portalId ("/projects/" <> toS pid <> "/tasklists")) (listQueryParams opts) []

-- | List task lists in a project. The response is @{tasklists:[...]}@.
list :: forall m. (HasZoho m) => PortalId -> ProjectId -> ListOptions -> m (Either Error [Tasklist])
list portalId projectId opts =
  runRequestAndParseResponse (listRequest portalId projectId opts) >>= \case
    Left e -> pure $ Left e
    Right (ResponseWrapper ts :: ResponseWrapper "tasklists" [Tasklist]) -> pure $ Right ts
