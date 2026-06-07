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
import qualified Data.Aeson.KeyMap as KM
import Data.String.Conv (toS)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (Request)
import Zoho.Projects.Common (PortalId, ProjectId (..), TasklistId (..), TaskId (..), mkApiEndpoint)
import qualified Zoho.OAuth as ZO
import Zoho.Types (Error, Zuid (..), EmptyZohoStructure (..), unsafeMergeObjects)
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

-- | One Projects task, parameterized over the custom-fields type @cf@ -- the same
-- @cf@ type-param convention as 'Zoho.Desk.Ticket' / the Books modules (the lib stays
-- project-agnostic). v3 places custom fields as FLAT top-level keys on the task object
-- (keyed by their layout @api_name@), NOT nested under a @custom_fields@ key -- so 'cf'
-- is merged into the task object by the hand-written 'ToJSON' below, the same
-- merge-two-objects approach as @Zoho.CRM.Records@. All fields optional so the same type
-- serves read responses and create/update bodies.
data TaskPoly cf = Task
  { taskId            :: !(Maybe TaskId)
  , taskName          :: !(Maybe Text)
  , taskDescription   :: !(Maybe Text)
  , taskPriority      :: !(Maybe Text)        -- ^ none | low | medium | high
  , taskStartDate     :: !(Maybe Text)        -- ^ @YYYY-MM-DD@
  , taskEndDate       :: !(Maybe Text)        -- ^ @YYYY-MM-DD@ -- v3's "due date"
  , taskTasklist      :: !(Maybe TasklistRef)
  , taskOwnersAndWork :: !(Maybe OwnersAndWork)
  , taskCustomFields  :: !(Maybe cf)          -- ^ merged flat into the task object on write (api_name keys)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

-- | Default task type for reads / callers that don't set custom fields.
type Task = TaskPoly Value

-- v3 keys custom fields by api_name directly ON the task object, NOT nested under a
-- "custom_fields" key, so 'cf' is merged flat with the standard fields (the same approach
-- as @Zoho.CRM.Contacts@, reusing its 'unsafeMergeObjects' helper).
--
-- ToJSON: generic-encode the standard fields (the @taskCustomFields@ field lands as a
-- nested @custom_fields@ object), drop that nested key, then merge the cf object flat.
-- FromJSON: hand-assemble (mirrors @Zoho.CRM.Contacts.contactParser@) -- the SAME incoming
-- value is handed to the cf parser so it can read its flat keys off the top-level object.
instance ToJSON cf => ToJSON (TaskPoly cf) where
  toJSON t =
    let stripped = case genericToJSON (aesonPrefix snakeCase){omitNothingFields = True} t of
          Object g -> Object (KM.delete "custom_fields" g)
          v        -> v
    in unsafeMergeObjects stripped (maybe Null toJSON (taskCustomFields t))

instance forall cf. FromJSON cf => FromJSON (TaskPoly cf) where
  parseJSON v = do
    (base :: TaskPoly cf) <- genericParseJSON (aesonPrefix snakeCase) v  -- standard fields
    tcf <- parseJSON v                                                   -- cf reads its flat keys from the same object
    pure base { taskCustomFields = tcf }

-- | An all-'Nothing' task. Set only the fields you want for a create/update body.
emptyTask :: TaskPoly cf
emptyTask = emptyZohoStructure

createRequest :: (ToJSON cf) => PortalId -> ProjectId -> TaskPoly cf -> Request
createRequest portalId (ProjectId pid) task =
  ZO.prepareJSONPost (mkApiEndpoint portalId ("/projects/" <> toS pid <> "/tasks")) [] [] task

-- | Create a task in a project. v3 returns the created task as a bare object (no
-- @{tasks:[...]}@ wrapper).
create :: forall m cf. (HasZoho m, ToJSON cf, FromJSON cf) => PortalId -> ProjectId -> TaskPoly cf -> m (Either Error (TaskPoly cf))
create portalId projectId task = runRequestAndParseResponse (createRequest portalId projectId task)

updateRequest :: (ToJSON cf) => PortalId -> ProjectId -> TaskId -> TaskPoly cf -> Request
updateRequest portalId (ProjectId pid) (TaskId tid) task =
  ZO.prepareJSONPatch (mkApiEndpoint portalId ("/projects/" <> toS pid <> "/tasks/" <> toS tid)) [] [] task

-- | Update a task (PATCH) -- e.g. rename, change status, or reassign owners by passing
-- @emptyTask{ taskOwnersAndWork = Just (ownersFromZuids zuids) }@. Returns the updated
-- task as a bare object.
update :: forall m cf. (HasZoho m, ToJSON cf, FromJSON cf) => PortalId -> ProjectId -> TaskId -> TaskPoly cf -> m (Either Error (TaskPoly cf))
update portalId projectId taskId task = runRequestAndParseResponse (updateRequest portalId projectId taskId task)

getRequest :: PortalId -> ProjectId -> TaskId -> Request
getRequest portalId (ProjectId pid) (TaskId tid) =
  ZO.prepareGet (mkApiEndpoint portalId ("/projects/" <> toS pid <> "/tasks/" <> toS tid)) [] []

-- | Fetch a single task by id (returns the task as a bare object; a missing id is an
-- HTTP error).
get :: forall m cf. (HasZoho m, FromJSON cf) => PortalId -> ProjectId -> TaskId -> m (Either Error (TaskPoly cf))
get portalId projectId taskId = runRequestAndParseResponse (getRequest portalId projectId taskId)
