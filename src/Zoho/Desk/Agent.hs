{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
module Zoho.Desk.Agent
  ( module Zoho.Desk.Agent
  , module Common
  )
where

import Control.Lens (makeLensesWith, abbreviatedFields)
import Data.Aeson as Aeson
import Data.Aeson.TH as Aeson
import Data.Aeson.Casing as Casing
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import Zoho.Types (EmptyZohoStructure(..), Error, zohoPrefix, OrgId)
import Zoho.OAuth as ZO hiding (mkApiEndpoint)
import Zoho.Desk.Common as Common
import Network.HTTP.Client as HC (Request)
import Zoho.ZohoM as ZM
import qualified Data.Text as T
import Prelude
import Data.String.Conv (toS)
import Data.Maybe (catMaybes)

-- | Agent status filter options
data AgentStatus = ActiveAgent | DisabledAgent | DeletedAgent | ImportedAgent
  deriving (Eq, Show)

agentStatusToText :: AgentStatus -> Text
agentStatusToText ActiveAgent = "ACTIVE"
agentStatusToText DisabledAgent = "DISABLED" 
agentStatusToText DeletedAgent = "DELETED"
agentStatusToText ImportedAgent = "IMPORTED"


-- | Sort order options
data SortOrder = Asc | Desc
  deriving (Eq, Show)

sortOrderToText :: SortOrder -> Text
sortOrderToText Asc = "asc"
sortOrderToText Desc = "desc"

-- | List agents options for query parameters
data ListOptions = ListOptions
  { optFrom :: !(Maybe Int)           -- Index to start from (>=0)
  , optLimit :: !(Maybe Int)          -- Number of agents (0-200)
  , optStatus :: !(Maybe AgentStatus) -- Filter by activation status
  , optSearchStr :: !(Maybe Text)     -- Search string (max 100 chars)
  , optFieldName :: !(Maybe Text) -- Field for searching/sorting (firstName, lastName, name, emailId)
  , optSortOrder :: !(Maybe SortOrder) -- Sort order
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

emptyListOptions :: ListOptions
emptyListOptions = emptyZohoStructure

-- | Convert ListOptions to query parameters
listOptionsToParams :: ListOptions -> [(Text, Text)]
listOptionsToParams ListOptions{..} = catMaybes
  [ fmap (("from",) . toS . show) optFrom
  , fmap (("limit",) . toS . show) optLimit  
  , fmap (("status",) . agentStatusToText) optStatus
  , fmap ("searchStr",) optSearchStr
  , fmap ("fieldName",) optFieldName
  , fmap (("sortOrder",) . sortOrderToText) optSortOrder
  ]

-- | Agent profile data type
data AgentProfile = AgentProfile
  { aprofileId :: !(Maybe ProfileId)
  , aprofileName :: !(Maybe Text)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

$(makeLensesWith abbreviatedFields ''AgentProfile)

instance FromJSON AgentProfile where
  parseJSON = genericParseJSON (zohoPrefix Casing.camelCase)

instance ToJSON AgentProfile where
  toJSON = genericToJSON (zohoPrefix Casing.camelCase)

-- | Agent role data type
data AgentRole = AgentRole
  { aroleId :: !(Maybe RoleId)
  , aroleName :: !(Maybe Text)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

$(makeLensesWith abbreviatedFields ''AgentRole)

instance FromJSON AgentRole where
  parseJSON = genericParseJSON (zohoPrefix Casing.camelCase)

instance ToJSON AgentRole where
  toJSON = genericToJSON (zohoPrefix Casing.camelCase)

-- | Agent department data type
data AgentDepartment = AgentDepartment
  { departmentId :: !(Maybe Text)
  , departmentName :: !(Maybe Text)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

$(makeLensesWith abbreviatedFields ''AgentDepartment)

instance FromJSON AgentDepartment where
  parseJSON = genericParseJSON (zohoPrefix Casing.camelCase)

instance ToJSON AgentDepartment where
  toJSON = genericToJSON (zohoPrefix Casing.camelCase)

-- | Agent data type based on the Zoho Desk API response  
data Agent = Agent
  { agentId :: !(Maybe AgentId)
  , agentFirstName :: !(Maybe Text)
  , agentLastName :: !(Maybe Text) 
  , agentName :: !(Maybe Text)
  , agentEmailId :: !(Maybe Text)
  , agentPhone :: !(Maybe Text)
  , agentMobile :: !(Maybe Text)
  , agentExtn :: !(Maybe Text)
  , agentZuid :: !(Maybe AgentZuid)
  , agentRoleId :: !(Maybe RoleId)
  , agentProfileId :: !(Maybe ProfileId)
  , agentTimeZone :: !(Maybe Text)
  , agentLangCode :: !(Maybe Text)
  , agentCountryCode :: !(Maybe Text)
  , agentPhotoURL :: !(Maybe Text)
  , agentChannelExpert :: !(Maybe [Text])
  , agentAssociatedDepartmentIds :: !(Maybe [Text])
  , agentAssociatedChatDepartmentIds :: !(Maybe [Text])
  , agentRolePermissionType :: !(Maybe Text)
  , agentStatus :: !(Maybe Text)
  , agentIsConfirmed :: !(Maybe Bool)
  , agentIsZiaAgent :: !(Maybe Bool)
  , agentAboutInfo :: !(Maybe Text)
  -- Extended fields for detailed agent info
  , agentProfile :: !(Maybe AgentProfile)
  , agentRole :: !(Maybe AgentRole)
  , agentAssociatedDepartments :: !(Maybe [AgentDepartment])
  , agentAssociatedChatDepartments :: !(Maybe [AgentDepartment])
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

emptyAgent :: Agent
emptyAgent = emptyZohoStructure

$(makeLensesWith abbreviatedFields ''Agent)

instance FromJSON Agent where
  parseJSON = genericParseJSON (zohoPrefix Casing.camelCase)

instance ToJSON Agent where
  toJSON = genericToJSON (zohoPrefix Casing.camelCase)

listRequest :: ListOptions -> OrgId -> Request  
listRequest options orgId = 
  ZO.prepareGet (Common.mkApiEndpoint "/agents") params [Common.orgIdHeader orgId]
  where
    params = map (\(k, v) -> (toS k, Just (toS v))) $ listOptionsToParams options

-- | List all agents in the organization
-- GET /api/v1/agents
list :: (HasZoho m) => ListOptions -> OrgId -> m (Either Error [Agent])
list options orgId = do
  x :: Either Error (ResponseWrapper "data" [Agent]) <-
    ZM.runRequestAndParseOptionalResponse (ResponseWrapper []) Prelude.id $
    listRequest options orgId
  pure $ fmap unwrapResponse x

getRequest :: OrgId -> AgentId -> [Text] -> Request
getRequest orgId agentId includes = 
  ZO.prepareGet (Common.mkApiEndpoint ("/agents/" <> toS agentId)) params [Common.orgIdHeader orgId]
  where
    params = case includes of
      [] -> []
      xs -> [("include", Just $ toS $ T.intercalate "," xs)]

-- | Get a specific agent by ID
-- GET /api/v1/agents/{agent_id}
get :: (HasZoho m) => OrgId -> AgentId -> [Text] -> m (Either Error Agent)
get orgId agentId includes = 
  ZM.runRequestAndParseResponse $
  getRequest orgId agentId includes