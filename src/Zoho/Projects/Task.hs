{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- | Zoho Projects v3 tasks: a single 'Task' type (all fields optional, via
-- 'EmptyZohoStructure') reused for read, create, and update -- the same convention as
-- 'Zoho.Desk.Task' and the Books modules. Build a create/update body from 'emptyTask'
-- and set only the fields you want.
--
-- Owners are assigned by zuid via the nested @owners_and_work.owners[{zuid}]@ shape and
-- the target list via @tasklist.id@; 'OwnerRef' / 'TasklistRef' express those shapes by
-- wrapping the canonical 'Zuid' / 'TasklistId' id types.
module Zoho.Projects.Task where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.String.Conv (toS)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (Request)
import Zoho.Projects.Common (PortalId, ProjectId (..), TasklistId (..), TaskId (..), mkApiEndpoint)
import qualified Zoho.OAuth as ZO
import Zoho.Types (Error, Zuid (..), EmptyZohoStructure (..))
import Zoho.ZohoM as ZM (HasZoho, runRequestAndParseResponse)

-- | A single owner reference inside @owners_and_work.owners@. Serializes to @{"zuid": ...}@.
newtype OwnerRef = OwnerRef { ownerZuid :: Zuid }
  deriving (Eq, Show, Generic)

instance ToJSON OwnerRef where
  toJSON = genericToJSON (aesonPrefix snakeCase)

instance FromJSON OwnerRef where
  parseJSON = genericParseJSON (aesonPrefix snakeCase)

-- | The @owners_and_work@ object. Serializes to @{"owners": [{"zuid": ...}]}@. On read
-- the key may be absent (then 'oawOwners' is empty).
newtype OwnersAndWork = OwnersAndWork { oawOwners :: [OwnerRef] }
  deriving (Eq, Show, Generic)

instance ToJSON OwnersAndWork where
  toJSON = genericToJSON (aesonPrefix snakeCase)

instance FromJSON OwnersAndWork where
  parseJSON = withObject "OwnersAndWork" $ \o -> OwnersAndWork <$> (o .:? "owners" .!= [])

-- | Build the @owners_and_work@ payload from a list of assignee zuids.
ownersFromZuids :: [Zuid] -> OwnersAndWork
ownersFromZuids = OwnersAndWork . map OwnerRef

-- | The @tasklist@ reference in the task body. Serializes to @{"id": ...}@; reuses the
-- canonical 'TasklistId' and only adds the object-wrapping the v3 API requires (a flat
-- @tasklist_id@ is rejected).
newtype TasklistRef = TasklistRef { tlrId :: TasklistId }
  deriving (Eq, Show, Generic)

instance ToJSON TasklistRef where
  toJSON = genericToJSON (aesonPrefix snakeCase)

instance FromJSON TasklistRef where
  parseJSON = genericParseJSON (aesonPrefix snakeCase)

-- | One Projects task. All fields optional so the same type serves read responses and
-- create/update bodies. Only the commonly-needed fields are modelled; inspect a live
-- response to add more.
data Task = Task
  { taskId            :: !(Maybe TaskId)
  , taskName          :: !(Maybe Text)
  , taskDescription   :: !(Maybe Text)
  , taskTasklist      :: !(Maybe TasklistRef)
  , taskOwnersAndWork :: !(Maybe OwnersAndWork)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

instance FromJSON Task where
  parseJSON = genericParseJSON (aesonPrefix snakeCase)

instance ToJSON Task where
  toJSON = genericToJSON (aesonPrefix snakeCase){omitNothingFields = True}

-- | An all-'Nothing' task. Set only the fields you want for a create/update body.
emptyTask :: Task
emptyTask = emptyZohoStructure

createRequest :: PortalId -> ProjectId -> Task -> Request
createRequest portalId (ProjectId pid) task =
  ZO.prepareJSONPost (mkApiEndpoint portalId ("/projects/" <> toS pid <> "/tasks")) [] [] task

-- | Create a task in a project. v3 returns the created task as a bare object (no
-- @{tasks:[...]}@ wrapper).
create :: forall m. (HasZoho m) => PortalId -> ProjectId -> Task -> m (Either Error Task)
create portalId projectId task = runRequestAndParseResponse (createRequest portalId projectId task)

updateRequest :: PortalId -> ProjectId -> TaskId -> Task -> Request
updateRequest portalId (ProjectId pid) (TaskId tid) task =
  ZO.prepareJSONPatch (mkApiEndpoint portalId ("/projects/" <> toS pid <> "/tasks/" <> toS tid)) [] [] task

-- | Update a task (PATCH) -- e.g. rename, change status, or reassign owners by passing
-- @emptyTask{ taskOwnersAndWork = Just (ownersFromZuids zuids) }@. Returns the updated
-- task as a bare object.
update :: forall m. (HasZoho m) => PortalId -> ProjectId -> TaskId -> Task -> m (Either Error Task)
update portalId projectId taskId task = runRequestAndParseResponse (updateRequest portalId projectId taskId task)

getRequest :: PortalId -> ProjectId -> TaskId -> Request
getRequest portalId (ProjectId pid) (TaskId tid) =
  ZO.prepareGet (mkApiEndpoint portalId ("/projects/" <> toS pid <> "/tasks/" <> toS tid)) [] []

-- | Fetch a single task by id (returns the task as a bare object; a missing id is an
-- HTTP error).
get :: forall m. (HasZoho m) => PortalId -> ProjectId -> TaskId -> m (Either Error Task)
get portalId projectId taskId = runRequestAndParseResponse (getRequest portalId projectId taskId)
