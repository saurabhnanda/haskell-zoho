{-# LANGUAGE DeriveAnyClass #-}
module Zoho.Desk.Ticket
  ( module Zoho.Desk.Ticket
  , module Common
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Zoho.Types
import GHC.Generics
import qualified Data.Aeson.Casing as Casing
import Data.Aeson as Aeson
import Data.Aeson.TH as Aeson
import Control.Lens (makeLensesWith, abbreviatedFields)
import Zoho.Desk.Utils (ticketJsonOptions)
import Zoho.OAuth as ZO
import Network.HTTP.Client as HC (Request)
import Zoho.Desk.Common as Common
import Zoho.ZohoM as ZM
import Data.String.Conv (toS)
import Control.Monad (join)
import Text.Read (readMaybe)

data TicketPoly stringInt cf = Ticket
  { ticketId :: !(Maybe TicketId)
  , ticketTicketNumber :: !(Maybe Text)
  , ticketSubject :: !(Maybe Text)
  , ticketDepartmentId :: !(Maybe Text)
  , ticketContactId :: !(Maybe Text)
  , ticketProductId :: !(Maybe Text)
  , ticketUploads :: !(Maybe OmitField) -- TODO
  , ticketEmail :: !(Maybe Text)
  , ticketPhone :: !(Maybe Text)
  , ticketDescription :: !(Maybe Text)
  , ticketStatus :: !(Maybe Text)
  , ticketAssigneeId :: !(Maybe AssigneeId)
  , ticketCategory :: !(Maybe Text)
  , ticketSubCategory :: !(Maybe Text)
  , ticketResolution :: !(Maybe Text)
  , ticketPriority :: !(Maybe Text)
  , ticketChannel :: !(Maybe Text)
  , ticketClassification :: !(Maybe Text)
  , ticketCustomFields :: !(Maybe cf)
  , ticketWebUrl :: !(Maybe Text)
  , ticketTeamId :: !(Maybe TeamId)
  , ticketThreadCount :: !(Maybe stringInt)
  , ticketCommentCount :: !(Maybe stringInt)
  , ticketTaskCount :: !(Maybe stringInt)
  , ticketAttachmentCount :: !(Maybe stringInt)
  , ticketSharedCount :: !(Maybe stringInt)
  , ticketApprovalCount :: !(Maybe stringInt)
  , ticketIsSpam :: !(Maybe Bool)
  , ticketIsDeleted :: !(Maybe Bool)
  , ticketIsTrashed :: !(Maybe Bool)
  , ticketCreatedTime :: !(Maybe UTCTime)
  , ticketModifiedTime :: !(Maybe UTCTime)
  , ticketClosedTime :: !(Maybe UTCTime)
  , ticketDueDate :: !(Maybe UTCTime)
  , ticketCustomerResponseTime :: !(Maybe UTCTime)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

type Ticket = TicketPoly Int

emptyTicket :: Ticket cf
emptyTicket = emptyZohoStructure

$(makeLensesWith abbreviatedFields ''TicketPoly)

instance (FromJSON cf, FromJSON stringInt) => FromJSON (TicketPoly stringInt cf) where
  parseJSON = genericParseJSON ticketJsonOptions

instance {-# OVERLAPS #-} (FromJSON cf) => FromJSON (TicketPoly Int cf) where
  parseJSON v = fmap convertStringToInt (genericParseJSON ticketJsonOptions v)
    where
      convertStringToInt :: TicketPoly String cf -> TicketPoly Int cf
      convertStringToInt tkt@Ticket{..} =
        tkt { ticketThreadCount = join $ fmap readMaybe ticketThreadCount
            , ticketCommentCount = join $ fmap readMaybe ticketCommentCount
            , ticketTaskCount = join $ fmap readMaybe ticketTaskCount
            , ticketAttachmentCount = join $ fmap readMaybe ticketAttachmentCount
            , ticketSharedCount = join $ fmap readMaybe ticketSharedCount
            , ticketApprovalCount = join $ fmap readMaybe ticketApprovalCount
            }

instance (ToJSON cf, ToJSON stringInt) => ToJSON (TicketPoly stringInt cf) where
  toJSON = genericToJSON ticketJsonOptions

instance {-# OVERLAPS #-} (ToJSON cf) => ToJSON (TicketPoly Int cf) where
  toJSON = (genericToJSON ticketJsonOptions) . convertIntToText
    where
      convertIntToText :: TicketPoly Int cf -> TicketPoly String cf
      convertIntToText x@Ticket{..} =
        x { ticketThreadCount = show <$> ticketThreadCount
          , ticketCommentCount = show <$> ticketCommentCount
          , ticketTaskCount = show <$> ticketTaskCount
          , ticketAttachmentCount = show <$> ticketAttachmentCount
          , ticketSharedCount = show <$> ticketSharedCount
          , ticketApprovalCount = show <$> ticketApprovalCount
          }

-- * List tickets

data ListOptions = ListOptions
  { optFrom :: !(Maybe Int)
  , optLimit :: !(Maybe Int)
  , optViewId :: !(Maybe Text)
  , optSortBy :: !(Maybe ApiName)
  , optDepartmentIds :: !(Maybe [Text])
  , optAssignee :: !(Maybe Text) -- TODO: custom data type?
  , optChannels :: !(Maybe [Text])
  , optStatuses :: !(Maybe [Text])
  , optReceivedInDays :: !(Maybe Int)
  , optInclude :: !(Maybe [ApiName])
  , optPriorities :: !(Maybe [Text])
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

$(makeLensesWith abbreviatedFields ''ListOptions)

emptyListOptions :: ListOptions
emptyListOptions = emptyZohoStructure

listRequest :: ListOptions
            -> OrgId
            -> Request
listRequest ListOptions{..} oid =
  ZO.prepareGet (Common.mkApiEndpoint "/tickets") params [Common.orgIdHeader oid]
  where
    params =
      applyOptionalCsvQueryParam "priority" optPriorities $
      applyOptionalCsvQueryParam "include" optInclude $
      applyOptionalQueryParam "receivedInDays" (show <$> optReceivedInDays) $
      applyOptionalCsvQueryParam "status" optStatuses $
      applyOptionalCsvQueryParam "channel" optChannels $
      applyOptionalQueryParam "assignee" optAssignee $
      applyOptionalCsvQueryParam "departmentIds" optDepartmentIds $
      applyOptionalQueryParam "sortBy" optSortBy $
      applyOptionalQueryParam "viewId" optViewId $
      applyOptionalQueryParam "limit" (show <$> optLimit) $
      applyOptionalQueryParam "from" (show <$> optFrom)
      []


-- Sort by a specific attribute: responseDueDate or customerResponseTime or createdTime or ticketNumber. The default sorting order is ascending. A - prefix denotes descending order of sorting.


list :: forall m cf . (HasZoho m, FromJSON cf)
     => ListOptions
     -> OrgId
     -> m (Either Error [Ticket cf])
list listOpts oid = do
  x :: Either Error (ResponseWrapper "data" [Ticket cf]) <-
    ZM.runRequestAndParseOptionalResponse (ResponseWrapper []) Prelude.id $
    listRequest listOpts oid
  pure $ fmap unwrapResponse x


createRequest :: (ToJSON cf)
              => OrgId
              -> Ticket cf
              -> Request
createRequest oid a =
  ZO.prepareJSONPost (Common.mkApiEndpoint "/tickets") [] [Common.orgIdHeader oid] a

create :: (HasZoho m, ToJSON cf, FromJSON cf)
       => OrgId
       -> Ticket cf
       -> m (Either Error (Ticket cf))
create oid a =
  ZM.runRequestAndParseResponse $
  createRequest oid a


-- relevance,modifiedTime,createdTime,customerResponseTime
data SortBy = SortRelevance
            | SortModifiedTime
            | SortCreatedTime
            | SortCustomerResponseTime
            | SortOther ApiName
            deriving (Eq, Show)

data SearchOptions = SearchOptions
  { soptsFrom :: !(Maybe Int)
  , soptsLimit :: !(Maybe Int)
  , soptsDepartmentId :: !(Maybe Text)
  , soptsId :: !(Maybe Text)
  , soptsTicketNumber :: !(Maybe Text)
  , soptsSubject :: !(Maybe Text)
  , soptsDescription :: !(Maybe Text)
  , soptsStatus :: !(Maybe Text)
  , soptsPriority :: !(Maybe Text)
  , soptsEmail :: !(Maybe Text)
  , soptsPhone :: !(Maybe Text)
  , soptsChannel :: !(Maybe Text)
  , soptsCategory :: !(Maybe Text)
  , soptsAssigneeId :: !(Maybe AssigneeId)
  , soptsContactId :: !(Maybe ContactId)
  , soptsAccountId :: !(Maybe AccountId)
  , soptsProductId :: !(Maybe ProductId)
  , soptsContactName :: !(Maybe Text)
  , soptsAccountName :: !(Maybe Text)
  , soptsProductName :: !(Maybe Text)
  , soptsTag :: !(Maybe Text)
  , soptsAll :: !(Maybe Bool)
  , soptsCustomFields :: ![(ApiName, Text)]
  , soptsCustomerResponseTimeRange :: !(Maybe (UTCTime, UTCTime))
  , soptsDueDateRange :: !(Maybe (UTCTime, UTCTime))
  , soptsCreatedTimeRange :: !(Maybe (UTCTime, UTCTime))
  , soptsModifiedTimeRange :: !(Maybe (UTCTime, UTCTime))
  , soptsSortBy :: !(Maybe (SortBy, Common.SortDirection))
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

$(makeLensesWith abbreviatedFields ''SearchOptions)

emptySearchOptions :: SearchOptions
emptySearchOptions = emptyZohoStructure

searchRequest :: OrgId
              -> SearchOptions
              -> Request
searchRequest oid opts@SearchOptions{..} =
  ZO.prepareGet (Common.mkApiEndpoint "/tickets/search") params [Common.orgIdHeader oid]
  where
    applySortBy k v p = case v of
      Nothing -> p
      Just (sortField, sortDir) ->
        let x = case sortField of
                  SortRelevance -> "relevance"
                  SortModifiedTime -> "modifiedTime"
                  SortCreatedTime -> "createdTime"
                  SortCustomerResponseTime -> "customerResponseTime"
                  SortOther z -> z
            y = case sortDir of
                  Common.SortAsc -> x
                  Common.SortDesc -> "-" <> x
        in (k, Just $ toS y):p

    params =
      applySortBy "sortBy" soptsSortBy $
      -- TODO: applyOptionalQueryParam "_all" soptsTag $
      applyTimeRangeParam "dueDateRange" soptsDueDateRange $
      applyTimeRangeParam "customerResponseTimeRange" soptsCustomerResponseTimeRange $
      applyOptionalQueryParam "tag" soptsTag $
      applyOptionalQueryParam "productName" soptsProductName $
      applyOptionalQueryParam "accountName" soptsAccountName $
      applyOptionalQueryParam "contactName" soptsContactName $
      applyOptionalQueryParam "productId" soptsProductId $
      applyOptionalQueryParam "accountId" soptsAccountId $
      applyOptionalQueryParam "contactId" soptsContactId $
      applyOptionalQueryParam "assigneeId" soptsAssigneeId $
      applyOptionalQueryParam "category" soptsCategory $
      applyOptionalQueryParam "channel" soptsChannel $
      applyOptionalQueryParam "phone" soptsPhone $
      applyOptionalQueryParam "email" soptsEmail $
      applyOptionalQueryParam "priority" soptsPriority $
      applyOptionalQueryParam "status" soptsStatus $
      applyOptionalQueryParam "description" soptsDescription $
      applyOptionalQueryParam "subject" soptsSubject $
      applyOptionalQueryParam "ticketNumber" soptsTicketNumber $
      applyOptionalQueryParam "departmentId" soptsDepartmentId $
      applyCustomFieldSearchParams opts $
      applyCommonSearchParams opts []


search :: (HasZoho m, FromJSON cf)
       => OrgId
       -> SearchOptions
       -> m (Either Error (SearchResults (Ticket cf)))
search oid sopts =
  ZM.runRequestAndParseOptionalResponse (SearchResults [] 0) Prelude.id $
  searchRequest oid sopts
