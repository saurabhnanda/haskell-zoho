{-# LANGUAGE DeriveAnyClass #-}
module Zoho.Desk.Task
  ( module Zoho.Desk.Task
  , module Common
  )
where

import Control.Lens
import Control.Monad (join)
import Data.Aeson as Aeson
import Data.Aeson.Casing as Casing
import Data.String.Conv (toS)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import Network.HTTP.Client as HC (Request)
import Text.Read (readMaybe)
import Zoho.Desk.Common as Common
import Zoho.Desk.Contact (Contact)
import Zoho.Desk.Ticket (Ticket, TicketPoly)
import Zoho.Desk.Utils (taskJsonOptions)
import Zoho.OAuth as ZO
import Zoho.Types (ApiName, EmptyZohoStructure(..), Error, OrgId(..), ResponseWrapper(..), zohoPrefix)
import Zoho.ZohoM as ZM

-- * Task Types

data Task cf = Task
  { taskId :: !(Maybe TaskId)
  , taskSubject :: !(Maybe Text)
  , taskDepartmentId :: !(Maybe Text)
  , taskTicketId :: !(Maybe TicketId)
  , taskDueDate :: !(Maybe UTCTime)
  , taskOwnerId :: !(Maybe AgentId)
  , taskCategory :: !(Maybe Text)
  , taskStatus :: !(Maybe Text)
  , taskPriority :: !(Maybe Text)
  , taskDescription :: !(Maybe Text)
  , taskContactId :: !(Maybe ContactId)
  , taskTeamId :: !(Maybe TeamId)
  , taskCreatorId :: !(Maybe AgentId)
  , taskCreatedTime :: !(Maybe UTCTime)
  , taskModifiedTime :: !(Maybe UTCTime)
  , taskCompletedTime :: !(Maybe UTCTime)
  , taskStartTime :: !(Maybe UTCTime)
  , taskWebUrl :: !(Maybe Text)
  , taskIsSpam :: !(Maybe Bool)
  , taskIsCommented :: !(Maybe Bool)
  , taskActivityType :: !(Maybe Text)
  , taskDirection :: !(Maybe Text)
  , taskCustomFields :: !(Maybe cf)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

emptyTask :: Task cf
emptyTask = emptyZohoStructure

$(makeLensesWith abbreviatedFields ''Task)

instance (FromJSON cf) => FromJSON (Task cf) where
  parseJSON = genericParseJSON taskJsonOptions

instance (ToJSON cf) => ToJSON (Task cf) where
  toJSON = genericToJSON taskJsonOptions

-- * List Tasks

data ListTasksOptions = ListTasksOptions
  { loptsFrom :: !(Maybe Int)
  , loptsLimit :: !(Maybe Int)
  , loptsInclude :: !(Maybe [ApiName])
  , loptsDepartmentId :: !(Maybe Text)
  , loptsDepartmentIds :: !(Maybe [Text])
  , loptsViewId :: !(Maybe Text)
  , loptsAssignee :: !(Maybe Text)
  , loptsDueDate :: !(Maybe Text)
  , loptsIsCompleted :: !(Maybe Bool)
  , loptsSortBy :: !(Maybe ApiName)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

$(makeLensesWith abbreviatedFields ''ListTasksOptions)

emptyListTasksOptions :: ListTasksOptions
emptyListTasksOptions = emptyZohoStructure

listRequest :: ListTasksOptions -> OrgId -> Request
listRequest ListTasksOptions{..} oid =
  ZO.prepareGet (Common.mkApiEndpoint "/tasks") params [Common.orgIdHeader oid]
  where
    params =
      applyOptionalQueryParam "sortBy" loptsSortBy $
      applyOptionalQueryParam "isCompleted" (fmap (\b -> if b then "true" else "false" :: Text) loptsIsCompleted) $
      applyOptionalQueryParam "dueDate" loptsDueDate $
      applyOptionalQueryParam "assignee" loptsAssignee $
      applyOptionalQueryParam "viewId" loptsViewId $
      applyOptionalCsvQueryParam "departmentIds" loptsDepartmentIds $
      applyOptionalQueryParam "departmentId" loptsDepartmentId $
      applyOptionalCsvQueryParam "include" loptsInclude $
      applyOptionalQueryParam "limit" (show <$> loptsLimit) $
      applyOptionalQueryParam "from" (show <$> loptsFrom)
      []

list :: forall m cf . (HasZoho m, FromJSON cf)
     => ListTasksOptions
     -> OrgId
     -> m (Either Error [Task cf])
list listOpts oid = do
  x :: Either Error (ResponseWrapper "data" [Task cf]) <-
    ZM.runRequestAndParseOptionalResponse (ResponseWrapper []) Prelude.id $
    listRequest listOpts oid
  pure $ fmap unwrapResponse x

-- * List Tasks by Ticket

data ListTasksByTicketOptions = ListTasksByTicketOptions
  { ltboFrom :: !(Maybe Int)
  , ltboLimit :: !(Maybe Int)
  , ltboIsCompleted :: !(Maybe Bool)
  , ltboIsSpam :: !(Maybe Bool)
  , ltboSortBy :: !(Maybe ApiName)
  , ltboInclude :: !(Maybe [ApiName])
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

$(makeLensesWith abbreviatedFields ''ListTasksByTicketOptions)

emptyListTasksByTicketOptions :: ListTasksByTicketOptions
emptyListTasksByTicketOptions = emptyZohoStructure

listByTicketRequest :: TicketId -> ListTasksByTicketOptions -> OrgId -> Request
listByTicketRequest ticketId ListTasksByTicketOptions{..} oid =
  ZO.prepareGet (Common.mkApiEndpoint $ "/tickets/" <> toS ticketId <> "/tasks") params [Common.orgIdHeader oid]
  where
    params =
      applyOptionalCsvQueryParam "include" ltboInclude $
      applyOptionalQueryParam "sortBy" ltboSortBy $
      applyOptionalQueryParam "isSpam" (fmap (\b -> if b then "true" else "false" :: Text) ltboIsSpam) $
      applyOptionalQueryParam "isCompleted" (fmap (\b -> if b then "true" else "false" :: Text) ltboIsCompleted) $
      applyOptionalQueryParam "limit" (show <$> ltboLimit) $
      applyOptionalQueryParam "from" (show <$> ltboFrom)
      []

listByTicket :: forall m cf . (HasZoho m, FromJSON cf)
             => TicketId
             -> ListTasksByTicketOptions
             -> OrgId
             -> m (Either Error [Task cf])
listByTicket ticketId listOpts oid = do
  x :: Either Error (ResponseWrapper "data" [Task cf]) <-
    ZM.runRequestAndParseOptionalResponse (ResponseWrapper []) Prelude.id $
    listByTicketRequest ticketId listOpts oid
  pure $ fmap unwrapResponse x

-- * Create Task

createRequest :: (ToJSON cf) => OrgId -> Task cf -> Request
createRequest oid task =
  ZO.prepareJSONPost (Common.mkApiEndpoint "/tasks") [] [Common.orgIdHeader oid] task

create :: (HasZoho m, ToJSON cf, FromJSON cf)
       => OrgId
       -> Task cf
       -> m (Either Error (Task cf))
create oid task =
  ZM.runRequestAndParseResponse $
  createRequest oid task

-- * Update Task

updateRequest :: (ToJSON cf) => OrgId -> TaskId -> Task cf -> Request
updateRequest oid taskId task =
  ZO.prepareJSONPatch (Common.mkApiEndpoint $ "/tasks/" <> toS taskId) [] [Common.orgIdHeader oid] task{taskId=Just taskId}

update :: (HasZoho m, ToJSON cf, FromJSON cf)
       => OrgId
       -> TaskId
       -> Task cf
       -> m (Either Error (Task cf))
update oid taskId task =
  ZM.runRequestAndParseResponse $
  updateRequest oid taskId task

-- * Update Many Tasks

data UpdateManyTasksRequest = UpdateManyTasksRequest
  { umtIds :: ![TaskId]
  , umtFieldName :: !Text
  , umtFieldValue :: !(Maybe Text)
  , umtIsCustomField :: !(Maybe Bool)
  } deriving (Eq, Show, Generic)

instance ToJSON UpdateManyTasksRequest where
  toJSON = genericToJSON (zohoPrefix Casing.camelCase)

data UpdateManyTasksError = UpdateManyTasksError
  { umteErrorMessage :: !Text
  , umteErrorCode :: !Text
  , umteHttpCode :: !Text
  } deriving (Eq, Show, Generic)

instance FromJSON UpdateManyTasksError where
  parseJSON = genericParseJSON (zohoPrefix Casing.camelCase)

data UpdateManyTasksResult = UpdateManyTasksResult
  { umtrSuccess :: !Bool
  , umtrId :: !TaskId
  , umtrErrors :: !(Maybe UpdateManyTasksError)
  } deriving (Eq, Show, Generic)

instance FromJSON UpdateManyTasksResult where
  parseJSON = genericParseJSON (zohoPrefix Casing.camelCase)

data UpdateManyTasksResponse = UpdateManyTasksResponse
  { umtrespResults :: ![UpdateManyTasksResult]
  } deriving (Eq, Show, Generic)

instance FromJSON UpdateManyTasksResponse where
  parseJSON = genericParseJSON (zohoPrefix Casing.camelCase)

updateManyRequest :: OrgId -> UpdateManyTasksRequest -> Request
updateManyRequest oid req =
  ZO.prepareJSONPost (Common.mkApiEndpoint "/tasks/updateMany") [] [Common.orgIdHeader oid] req

updateMany :: (HasZoho m) => OrgId -> UpdateManyTasksRequest -> m (Either Error UpdateManyTasksResponse)
updateMany oid req =
  ZM.runRequestAndParseResponse $
  updateManyRequest oid req

-- * Delete Tasks (Move to Trash)

data MoveToTrashRequest = MoveToTrashRequest
  { mttrEntityIds :: ![TaskId]
  } deriving (Eq, Show, Generic)

instance ToJSON MoveToTrashRequest where
  toJSON = genericToJSON (zohoPrefix Casing.camelCase)

moveToTrashRequest :: OrgId -> [TaskId] -> Request
moveToTrashRequest oid taskIds =
  ZO.prepareJSONPost (Common.mkApiEndpoint "/tasks/moveToTrash") [] [Common.orgIdHeader oid] $
    MoveToTrashRequest taskIds

moveToTrash :: (HasZoho m) => OrgId -> [TaskId] -> m (Either Error ())
moveToTrash oid taskIds =
  ZM.runRequestAndParseOptionalResponse () Prelude.id $
  moveToTrashRequest oid taskIds
