{-# LANGUAGE DeriveAnyClass #-}
module Zoho.Desk.Contact
  ( module Zoho.Desk.Contact
  , module Common
  )
where

import Control.Lens
import Data.Aeson as Aeson
import Data.Aeson.TH as Aeson
import Data.Aeson.Casing as Casing
import Data.Text (Text)
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import GHC.Generics
import Zoho.Desk.Utils (contactJsonOptions)
import Zoho.Types (EmptyZohoStructure(..), Error, zohoPrefix, UnsafeEither(..))
import Zoho.Types (OrgId(..), ApiName, ResponseWrapper(..))
import Zoho.OAuth as ZO hiding (mkApiEndpoint)
import Zoho.Desk.Common as Common
import Zoho.Desk.Common
import Network.HTTP.Client as HC (Request, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Zoho.ZohoM as ZM
import qualified Data.Text as T
import Prelude
import Data.String.Conv (toS)
import Text.Read (readMaybe)
import Control.Monad (join)

data CustomerHappiness = CustomerHappiness
  { happyBadPercentage :: !(Maybe Float)
  , happyGoodPercentage :: !(Maybe Float)
  , happyOkPercentage :: !(Maybe Float)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

$(makeLensesWith abbreviatedFields ''CustomerHappiness)

instance FromJSON CustomerHappiness where
  parseJSON = withObject "Need an Object to parse into Zoho.Desk.Contact.CustomerHappiness" $ \o -> do
    happyBadPercentage <- floatParser $ o .:? "badPercentage"
    happyGoodPercentage <- floatParser $ o .:? "goodPercentage"
    happyOkPercentage <- floatParser $ o .:? "okPercentage"
    pure CustomerHappiness{..}
    where
      floatParser :: (Monad m) => m (Maybe String) -> m (Maybe Float)
      floatParser x =
        fmap join $
        (fmap . fmap) readMaybe x

instance ToJSON CustomerHappiness where
  toJSON = genericToJSON (zohoPrefix Casing.camelCase)

data Contact cf = Contact
  { contactId :: !(Maybe Text)
  , contactCustomFields :: !(Maybe cf)
  , contactLastName :: !(Maybe Text)
  , contactFirstName :: !(Maybe Text)
  , contactFacebook :: !(Maybe Text)
  , contactTwitter :: !(Maybe Text)
  , contactSecondaryEmail :: !(Maybe Text)
  , contactEmail :: !(Maybe Text)
  , contactPhone :: !(Maybe Text)
  , contactMobile :: !(Maybe Text)
  , contactCity :: !(Maybe Text)
  , contactCountry :: !(Maybe Text)
  , contactState :: !(Maybe Text)
  , contactStreet :: !(Maybe Text)
  , contactZip :: !(Maybe Text)
  , contactDescription :: !(Maybe Text)
  , contactTitle :: !(Maybe Text)
  , contactTyp :: !(Maybe Text) -- TODO
  , contactOwnerId :: !(Maybe AgentId)
  , contactOwner :: !(Maybe Aeson.Value) -- TODO
  , contactAccountId :: !(Maybe Text)
  , contatZohoCRMContact :: !(Maybe Aeson.Value) -- TODO
  , contactCustomerHappiness :: !(Maybe CustomerHappiness)
  , contactIsDeleted :: !(Maybe Bool)
  , contactIsTrashed :: !(Maybe Bool)
  , contactIsSpam :: !(Maybe Bool)
  , contactPhotoUrl :: !(Maybe Text)
  , contactWebUrl :: !(Maybe Text)
  , contactCreatedTime :: !(Maybe UTCTime)
  , contactModifiedTime :: !(Maybe UTCTime)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

$(makeLensesWith abbreviatedFields ''Contact)

emptyContact :: Contact cf
emptyContact = emptyZohoStructure

$(deriveJSON contactJsonOptions ''Contact)

-- TODO: include
data ListOptions = ListOptions
  { optFrom :: !(Maybe Int)
  , optLimit :: !(Maybe Int)
  , optViewId :: !(Maybe Text)
  , optSortBy :: !(Maybe ApiName)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

emptyListOptions :: ListOptions
emptyListOptions = emptyZohoStructure

listRequest :: ListOptions
            -> OrgId
            -> Request
listRequest ListOptions{..} oid =
  ZO.prepareGet (Common.mkApiEndpoint "/contacts") params [Common.orgIdHeader oid]
  where
    params =
      applyOptionalQueryParam "sortBy" optSortBy $
      applyOptionalQueryParam "viewId" optViewId $
      applyOptionalQueryParam "limit" (show <$> optLimit) $
      applyOptionalQueryParam "from" (show <$> optFrom)
      []

list :: forall m cf . (HasZoho m, FromJSON cf)
     => ListOptions
     -> OrgId
     -> m (Either Error [Contact cf])
list listOpts oid = do
  x :: Either Error (ResponseWrapper "data" [Contact cf]) <-
    ZM.runRequestAndParseOptionalResponse (ResponseWrapper []) Prelude.id $
    listRequest listOpts oid
  pure $ fmap unwrapResponse x

createRequest :: (ToJSON cf)
              => OrgId
              -> Contact cf
              -> Request
createRequest oid a =
  ZO.prepareJSONPost (Common.mkApiEndpoint "/contacts") [] [Common.orgIdHeader oid] a

create :: (HasZoho m, ToJSON cf, FromJSON cf)
       => OrgId
       -> Contact cf
       -> m (Either Error (Contact cf))
create oid a =
  ZM.runRequestAndParseResponse $
  createRequest oid a

updateRequest :: (ToJSON cf)
              => OrgId
              -> Text
              -> Contact cf
              -> Request
updateRequest oid cid a =
  ZO.prepareJSONPatch (Common.mkApiEndpoint $ "/contacts/" <> toS cid) [] [Common.orgIdHeader oid] a{contactId=Just cid}


update :: (HasZoho m, ToJSON cf, FromJSON cf)
       => OrgId
       -> Text
       -> Contact cf
       -> m (Either Error (Contact cf))
update oid cid a =
  ZM.runRequestAndParseResponse $
  updateRequest oid cid a

data SortBy = SortRelevance
            | SortModifiedTime
            | SortCreatedTime
            | SortLastName
            | SortFirstName
            | SortOther ApiName
            deriving (Eq, Show)

data SearchOptions = SearchOptions
  { soptsFrom :: !(Maybe Int)
  , soptsLimit :: !(Maybe Int)
  , soptsId :: !(Maybe Text)
  , soptsFullName :: !(Maybe Text)
  , soptsFirstName :: !(Maybe Text)
  , soptsLastName :: !(Maybe Text)
  , soptsEmail :: !(Maybe Text)
  , soptsPhone :: !(Maybe Text)
  , soptsMobile :: !(Maybe Text)
  , soptsAccountName :: !(Maybe Text)
  , soptsAll :: !(Maybe Text)
  , soptsCustomFields :: ![(ApiName, Text)]
  , soptsCreatedTimeRange :: !(Maybe (UTCTime, UTCTime))
  , soptsModifiedTimeRange :: !(Maybe (UTCTime, UTCTime))
  , soptsSortBy :: !(Maybe (SortBy, SortDirection))
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

$(makeLensesWith abbreviatedFields ''SearchOptions)


emptySearchOptions :: SearchOptions
emptySearchOptions = emptyZohoStructure


searchRequest :: SearchOptions
              -> OrgId
              -> Request
searchRequest opts@SearchOptions{..} oid =
  ZO.prepareGet (Common.mkApiEndpoint "/contacts/search") params [Common.orgIdHeader oid]
  where
    applySortBy k v p = case v of
      Nothing -> p
      Just (sortField, sortDir) ->
        let x = case sortField of
                  SortRelevance -> "relevance"
                  SortModifiedTime -> "modifiedTime"
                  SortCreatedTime -> "createdTime"
                  SortLastName -> "lastName"
                  SortFirstName -> "firstName"
                  SortOther z -> z
            y = case sortDir of
                  SortAsc -> x
                  SortDesc -> "-" <> x
        in (k, Just $ toS y):p

    params =
      applySortBy "sortBy" soptsSortBy $
      applyOptionalQueryParam "fullName" soptsFullName $
      applyOptionalQueryParam "firstName" soptsFirstName $
      applyOptionalQueryParam "lastName" soptsLastName $
      applyOptionalQueryParam "email" soptsEmail $
      applyOptionalQueryParam "phone" soptsPhone $
      applyOptionalQueryParam "mobile" soptsMobile $
      applyOptionalQueryParam "accountName" soptsAccountName $
      applyOptionalQueryParam "_all" soptsAll $
      applyCustomFieldSearchParams opts $
      applyCommonSearchParams opts []

search :: (HasZoho m, FromJSON cf)
       => SearchOptions
       -> OrgId
       -> m (Either Error (SearchResults (Contact cf)))
search sopts oid =
  ZM.runRequestAndParseOptionalResponse (SearchResults [] 0) Prelude.id $
  searchRequest sopts oid

data ContactStatistics = ContactStatistics
  { statsOpenTickets :: !(Maybe Int)
  , statsClosedTickets :: !(Maybe Int)
  , statsOnHoldTickets :: !(Maybe Int)
  , statsOverdueTickets :: !(Maybe Int)
  , statsSpamTickets :: !(Maybe Int)
  , statsTotalTickets :: !(Maybe Int)
  , statsAverageHappinessScore :: !(Maybe Float)
  , statsAverageFirstResponseTime :: !(Maybe Text)
  , statsAverageResponseTime :: !(Maybe Text)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

$(makeLensesWith abbreviatedFields ''ContactStatistics)

instance FromJSON ContactStatistics where
  parseJSON = withObject "ContactStatistics" $ \o -> do
    statsOpenTickets <- convertCount <$> (o .:? "openTickets")
    statsClosedTickets <- convertCount <$> (o .:? "closedTickets")
    statsOnHoldTickets <- convertCount <$> (o .:? "onHoldTickets")
    statsOverdueTickets <- convertCount <$> (o .:? "overdueTickets")
    statsSpamTickets <- convertCount <$> (o .:? "spamTickets")
    statsTotalTickets <- convertCount <$> (o .:? "totalTickets")
    statsAverageHappinessScore <- convertScore <$> (o .:? "averageHappinessScore")
    statsAverageFirstResponseTime <- o .:? "averageFirstResponseTime"
    statsAverageResponseTime <- o .:? "averageResponseTime"
    pure ContactStatistics{..}
    where
      convertCount :: Maybe (UnsafeEither Int Text) -> Maybe Int
      convertCount Nothing = Nothing
      convertCount (Just (UnsafeLeft i)) = Just i
      convertCount (Just (UnsafeRight t)) = readMaybe (toS t)

      convertScore :: Maybe (UnsafeEither Float Text) -> Maybe Float
      convertScore Nothing = Nothing
      convertScore (Just (UnsafeLeft f)) = Just f
      convertScore (Just (UnsafeRight t)) = readMaybe (toS t)

instance ToJSON ContactStatistics where
  toJSON = genericToJSON (zohoPrefix Casing.camelCase)

getStatisticsRequest :: OrgId
                     -> Text  -- Contact ID
                     -> Request
getStatisticsRequest oid contactId =
  ZO.prepareGet (Common.mkApiEndpoint $ "/contacts/" <> toS contactId <> "/statistics") [] [Common.orgIdHeader oid]

getStatistics :: (HasZoho m)
              => OrgId
              -> Text  -- Contact ID
              -> m (Either Error ContactStatistics)
getStatistics oid contactId =
  ZM.runRequestAndParseResponse $
  getStatisticsRequest oid contactId

data MoveToTrashRequest = MoveToTrashRequest
  { mttContactIds :: ![ContactId]
  } deriving (Eq, Show, Generic)

instance ToJSON MoveToTrashRequest where
  toJSON = genericToJSON (zohoPrefix Casing.camelCase)

moveToTrashRequest :: OrgId -> [ContactId] -> Request
moveToTrashRequest oid contactIds =
  ZO.prepareJSONPost (Common.mkApiEndpoint "/contacts/moveToTrash") [] [Common.orgIdHeader oid] $
    MoveToTrashRequest contactIds

moveToTrash :: (HasZoho m) => OrgId -> [ContactId] -> m (Either Error ())
moveToTrash oid contactIds =
  ZM.runRequestAndParseOptionalResponse () Prelude.id $
  moveToTrashRequest oid contactIds

-- Update many contacts API
data UpdateManyRequest = UpdateManyRequest
  { umrIds :: ![ContactId]
  , umrFieldName :: !Text
  , umrFieldValue :: !(Maybe Text)
  , umrIsCustomField :: !(Maybe Bool)
  } deriving (Eq, Show, Generic)

instance ToJSON UpdateManyRequest where
  toJSON = genericToJSON (zohoPrefix Casing.camelCase)

data UpdateManyError = UpdateManyError
  { umeErrorCode :: !Text
  , umeHttpCode :: !Text
  } deriving (Eq, Show, Generic)

instance FromJSON UpdateManyError where
  parseJSON = genericParseJSON (zohoPrefix Casing.camelCase)

data UpdateManyResult = UpdateManyResult
  { umrSuccess :: !Bool
  , umrId :: !ContactId
  , umrErrors :: !(Maybe UpdateManyError)
  } deriving (Eq, Show, Generic)

instance FromJSON UpdateManyResult where
  parseJSON = genericParseJSON (zohoPrefix Casing.camelCase)

data UpdateManyResponse = UpdateManyResponse
  { umrespResults :: ![UpdateManyResult]
  } deriving (Eq, Show, Generic)

instance FromJSON UpdateManyResponse where
  parseJSON = genericParseJSON (zohoPrefix Casing.camelCase)

updateManyRequest :: OrgId -> UpdateManyRequest -> Request
updateManyRequest oid req =
  ZO.prepareJSONPost (Common.mkApiEndpoint "/contacts/updateMany") [] [Common.orgIdHeader oid] req

updateMany :: (HasZoho m) => OrgId -> UpdateManyRequest -> m (Either Error UpdateManyResponse)
updateMany oid req =
  ZM.runRequestAndParseResponse $
  updateManyRequest oid req

-- | Duplicate detection: list field values that have duplicates
-- GET /api/v1/groupContactDuplicateValues?fieldName=email
data DuplicateFieldName = DupFirstName
                        | DupLastName
                        | DupEmail
                        | DupSecondaryEmail
                        | DupPhone
                        | DupFullName
                        | DupBothEmail
                        deriving (Eq, Show)

duplicateFieldNameToText :: DuplicateFieldName -> Text
duplicateFieldNameToText = \case
  DupFirstName -> "firstName"
  DupLastName -> "lastName"
  DupEmail -> "email"
  DupSecondaryEmail -> "secondaryEmail"
  DupPhone -> "phone"
  DupFullName -> "fullName"
  DupBothEmail -> "bothEmail"

data DuplicateListOptions = DuplicateListOptions
  { dloFieldName :: !DuplicateFieldName
  , dloFrom :: !(Maybe Int)
  , dloLimit :: !(Maybe Int)
  , dloSortBy :: !(Maybe Text)  -- "count" or "fieldValue"
  , dloSwFieldValue :: !(Maybe Text)
  } deriving (Eq, Show)

data DuplicateValueEntry = DuplicateValueEntry
  { dveCount :: !Int
  , dveFieldValue :: !Text
  } deriving (Eq, Show, Generic)

instance FromJSON DuplicateValueEntry where
  parseJSON = genericParseJSON (zohoPrefix Casing.camelCase)

listDuplicateValuesRequest :: DuplicateListOptions -> OrgId -> Request
listDuplicateValuesRequest DuplicateListOptions{..} oid =
  ZO.prepareGet (Common.mkApiEndpoint "/groupContactDuplicateValues") params [Common.orgIdHeader oid]
  where
    params =
      applyOptionalQueryParam "swFieldValue" dloSwFieldValue $
      applyOptionalQueryParam "sortBy" dloSortBy $
      applyOptionalQueryParam "limit" (show <$> dloLimit) $
      applyOptionalQueryParam "from" (show <$> dloFrom)
      [("fieldName", Just $ toS $ duplicateFieldNameToText dloFieldName)]

listDuplicateValues :: (HasZoho m)
                    => DuplicateListOptions
                    -> OrgId
                    -> m (Either Error [DuplicateValueEntry])
listDuplicateValues opts oid = do
  x :: Either Error (ResponseWrapper "data" [DuplicateValueEntry]) <-
    ZM.runRequestAndParseOptionalResponse (ResponseWrapper []) Prelude.id $
    listDuplicateValuesRequest opts oid
  pure $ fmap unwrapResponse x

-- | Duplicate detection: list full contact details for a duplicate group
-- GET /api/v1/groupContacts?fieldName=email&fieldValues=jade@example.com
data DuplicateGroupOptions = DuplicateGroupOptions
  { dgoFieldName :: !DuplicateFieldName
  , dgoFieldValues :: !Text  -- The actual duplicate value (e.g., email address)
  , dgoFrom :: !(Maybe Int)
  , dgoLimit :: !(Maybe Int)
  , dgoSortBy :: !(Maybe Text)
  } deriving (Eq, Show)

data DuplicateGroup cf = DuplicateGroup
  { dgFieldValue :: !Text
  , dgContacts :: ![Contact cf]
  } deriving (Eq, Show, Generic)

instance (FromJSON cf) => FromJSON (DuplicateGroup cf) where
  parseJSON = genericParseJSON (zohoPrefix Casing.camelCase)

listDuplicateGroupRequest :: DuplicateGroupOptions -> OrgId -> Request
listDuplicateGroupRequest DuplicateGroupOptions{..} oid =
  ZO.prepareGet (Common.mkApiEndpoint "/groupContacts") params [Common.orgIdHeader oid]
  where
    params =
      applyOptionalQueryParam "sortBy" dgoSortBy $
      applyOptionalQueryParam "limit" (show <$> dgoLimit) $
      applyOptionalQueryParam "from" (show <$> dgoFrom)
      [ ("fieldName", Just $ toS $ duplicateFieldNameToText dgoFieldName)
      , ("fieldValues", Just $ toS dgoFieldValues)
      ]

listDuplicateGroup :: forall m cf . (HasZoho m, FromJSON cf)
                   => DuplicateGroupOptions
                   -> OrgId
                   -> m (Either Error [DuplicateGroup cf])
listDuplicateGroup opts oid = do
  x :: Either Error (ResponseWrapper "data" [DuplicateGroup cf]) <-
    ZM.runRequestAndParseOptionalResponse (ResponseWrapper []) Prelude.id $
    listDuplicateGroupRequest opts oid
  pure $ fmap unwrapResponse x

-- | Merge contacts
-- POST /api/v1/contacts/{contact_id}/merge
-- The contact_id in the URL becomes the surviving contact.
-- The ids in the body are the contacts to merge into it.
-- The source object maps field names to the contact ID whose value should be kept.
data MergeRequest = MergeRequest
  { mrIds :: ![ContactId]
  , mrSource :: !(Maybe Aeson.Value)  -- JSON object mapping field names to contact IDs
  } deriving (Eq, Show, Generic)

instance ToJSON MergeRequest where
  toJSON = genericToJSON (zohoPrefix Casing.camelCase)

mergeRequest :: OrgId
             -> ContactId  -- Primary (surviving) contact
             -> MergeRequest
             -> Request
mergeRequest oid primaryId req =
  ZO.prepareJSONPost (Common.mkApiEndpoint $ "/contacts/" <> toS primaryId <> "/merge") [] [Common.orgIdHeader oid] req

merge :: forall m cf . (HasZoho m, FromJSON cf)
      => OrgId
      -> ContactId  -- Primary (surviving) contact
      -> MergeRequest
      -> m (Either Error (Contact cf))
merge oid primaryId req =
  ZM.runRequestAndParseResponse $
  mergeRequest oid primaryId req
