{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
module Zoho.CRM.Records where

import Zoho.OAuth as ZO
import Network.OAuth.OAuth2 as O
-- import Network.Wreq as W hiding (Proxy(..))
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
-- import Network.Wreq as W
import Data.Aeson as Aeson
import Data.String.Conv
import Network.HTTP.Client (Manager)
import URI.ByteString as U
import Zoho.Types
import qualified Data.Text as T
import Data.Text (Text)
import Control.Lens
import Zoho.CRM.Common
import Data.Time
import Data.Maybe (listToMaybe)
import Zoho.ZohoM as ZM
import Network.HTTP.Client as HC
import Network.HTTP.Types as HT
import Data.Aeson.TH
import Data.Aeson.Casing as Casing
import qualified Data.List.NonEmpty as NE
import GHC.Base (sconcat)
import Data.Text.Conversions (ToText(..))
import GHC.Generics


type RecordId = Text

apiEndpoint :: BS.ByteString -> URI
apiEndpoint modApiName  = ZO.mkApiEndpoint $ "/crm/v2/" <> modApiName

apiEndpointStr :: BS.ByteString -> String
apiEndpointStr modApiName = toS $ serializeURIRef' $ apiEndpoint modApiName


data SortOrder = Asc | Desc deriving (Eq, Show, Ord, Enum)
instance ToText SortOrder where
  toText t = case t of
    Asc -> "asc"
    Desc -> "desc"

data TriState = TriTrue | TriFalse | TriBoth deriving (Eq, Show, Ord, Enum)
instance ToText TriState where
  toText t = case t of
    TriTrue -> "true"
    TriFalse -> "false"
    TriBoth -> "both"

instance (StringConv Text s) => StringConv TriState s where
  strConv l x = strConv l $ toText x

data Trigger = TrgWorkflow
             | TrgApproval
             | TrgBlueprint
             deriving (Eq, Show, Ord, Enum)

instance ToJSON Trigger where
  toJSON t = case t of
    TrgWorkflow -> "workflow"
    TrgApproval -> "approval"
    TrgBlueprint -> "blueprint"

data TriggerSetting = TSOmit
                    | TSNone
                    | TSSpecific [Trigger]
                    deriving (Eq, Show, Ord)

data ListOptions = ListOptions
  { optFields :: Maybe [ApiName]
  , optSortOrder :: Maybe SortOrder
  , optSortBy :: Maybe ApiName
  , optConverted :: Maybe TriState
  , optApproved :: Maybe TriState
  , optPage :: Maybe Int
  , optPerPage :: Maybe Int
  , optCustomViewId :: Maybe Text
  , optTerritoryId :: Maybe Text
  , optIncludeChildTerritories :: Maybe Bool
  , optModifiedAfter :: Maybe UTCTime
  } deriving (Eq, Show, Generic, EmptyZohoStructure)


emptyListOptions :: ListOptions
emptyListOptions = emptyZohoStructure

list :: (FromJSON a, HasZoho m)
     => BS.ByteString
     -> ListOptions
     -> m (Either Error (PaginatedResponse "data" [a]))
list modApiName listopts =
  ZM.runRequestAndParseOptionalResponse noResults Prelude.id $
  listRequest modApiName listopts
  where
    noResults = PaginatedResponse
      { pageActualData = []
      , pageRecordsPerPage = 0
      , pageCount = 0
      , pageCurrentPage = 0
      , pageMoreRecords = False
      }


listRequest :: BS.ByteString
            -> ListOptions
            -> Request
listRequest modApiName ListOptions{..} =
  ZO.prepareGet (apiEndpoint modApiName) qparams headers
  where
    iso8601 = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"

    headers =
      applyOptionalHeader hIfModifiedSince (iso8601 <$> optModifiedAfter)
      []

    qparams =
      applyOptionalQueryParam "include_child" ((T.toLower . toS . show) <$> optIncludeChildTerritories) $
      applyOptionalQueryParam "territory_id" optTerritoryId $
      applyOptionalQueryParam "cvid" optCustomViewId $
      applyOptionalQueryParam "per_page" (show <$> optPerPage) $
      applyOptionalQueryParam "page" (show <$> optPage) $
      applyOptionalQueryParam "approved" optApproved $
      applyOptionalQueryParam "converted" optConverted $
      applyOptionalQueryParam "sort_by" optSortBy $
      applyOptionalQueryParam "sort_order" (toText <$> optSortOrder) $
      applyOptionalCsvQueryParam "fields" optFields $
      []

getSpecificRequest :: BS.ByteString
                   -> Text
                   -> Request
getSpecificRequest modApiName recordId =
  let url = uriAppendPathFragment ("/" <> toS recordId) (apiEndpoint modApiName)
  in ZO.prepareGet url  [] []

getSpecific :: (HasZoho m, FromJSON a)
            => BS.ByteString
            -> Text
            -> m (Either Error (Maybe a))
getSpecific modApiName recordId =
  runRequestAndParseOptionalResponse Nothing transformFn $
  getSpecificRequest modApiName recordId
  where
    transformFn :: ResponseWrapper "data" [a] -> Maybe a
    transformFn xs = listToMaybe $ unwrapResponse xs

insertRequest :: (ToJSON a)
              => BS.ByteString
              -> [a]
              -> TriggerSetting
              -> Request
insertRequest modApiName records tsetting =
  insertUpdateHelper "POST" modApiName Nothing records tsetting Nothing

insert :: (ToJSON a, HasZoho m)
       => BS.ByteString
       -> [a]
       -> TriggerSetting
       -> m (Either Error [InsertResult])
insert modApiName records tsetting =
  (ZM.runRequestAndParseResponse $ insertRequest modApiName records tsetting) >>= \case
  Left e ->
    pure $ Left e
  Right (r :: ResponseWrapper "data" [InsertResult]) ->
    pure $ Right $ unwrapResponse r


updateRequest :: (ToJSON a)
              => BS.ByteString
              -> [a]
              -> TriggerSetting
              -> Request
updateRequest modApiName records tsetting =
  insertUpdateHelper "PUT" modApiName Nothing records tsetting Nothing


update :: (ToJSON a, HasZoho m)
       => BS.ByteString
       -> [a]
       -> TriggerSetting
       -> m (Either Error [UpdateResult])
update modApiName records tsetting =
  (ZM.runRequestAndParseResponse $ updateRequest modApiName records tsetting) >>= \case
  Left e ->
    pure $ Left e
  Right (r :: ResponseWrapper "data" [InsertResult]) ->
    pure $ Right $ unwrapResponse r

type DuplicateCheckFields = [ApiName]

upsertRequest :: (ToJSON a)
              => BS.ByteString
              -> [a]
              -> TriggerSetting
              -> DuplicateCheckFields
              -> Request
upsertRequest modApiName records tsetting dupCheckFields =
  insertUpdateHelper "POST" modApiName (Just "upsert") records tsetting (Just dupCheckFields)


upsert :: (ToJSON a, HasZoho m)
       => BS.ByteString
       -> [a]
       -> TriggerSetting
       -> DuplicateCheckFields
       -> m (Either Error [UpsertResult])
upsert modApiName records tsetting dupCheckFields =
  (ZM.runRequestAndParseResponse $ upsertRequest modApiName records tsetting dupCheckFields) >>= \case
  Left e ->
    pure $ Left e
  Right (r :: ResponseWrapper "data" [UpsertResult]) ->
    pure $ Right $ unwrapResponse r



type TriggerWorkflow = Bool
deleteRequest :: BS.ByteString
              -> NE.NonEmpty Text
              -> TriggerWorkflow
              -> Request
deleteRequest modApiName recordIds wfTrigger =
  prepareDelete (apiEndpoint modApiName) params [] Nothing
  where
    params = [ ("ids", Just $ toS $ sconcat $ NE.intersperse "," recordIds)
             , ("wf_trigger", Just $ if wfTrigger then "true" else "false")
             ]

delete :: (HasZoho m)
       => BS.ByteString
       -> NE.NonEmpty Text
       -> TriggerWorkflow
       -> m (Either Error [DeleteResult])
delete modApiName recordIds wfTrigger =
  (ZM.runRequestAndParseResponse $ deleteRequest modApiName recordIds wfTrigger) >>= \case
  Left e ->
    pure $ Left e
  Right (r :: ResponseWrapper "data" [DeleteResult]) ->
    pure $ Right $ unwrapResponse r


data SearchTerm = OpEquals !ApiName !Text
                | OpStartsWith !ApiName !Text
                | OpAnd !SearchTerm !SearchTerm
                | OpOr !SearchTerm !SearchTerm
                deriving (Eq, Show)

data SearchQuery = SearchEmail !Text
                 | SearchPhone !Text
                 | SearchWord !Text
                 | SearchCriteria !SearchTerm
                 deriving (Eq, Show, Generic)

data SearchOpts = SearchOpts
  { soptsConverted :: Maybe TriState
  , soptsApproved :: Maybe TriState
  , soptsPage :: Maybe Int
  , soptsPerPage :: Maybe Int
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

emptySearchOpts :: SearchOpts
emptySearchOpts = emptyZohoStructure

searchRequest :: BS.ByteString
              -> SearchQuery
              -> SearchOpts
              -> Request
searchRequest modApiName q SearchOpts{..} =
  let url = uriAppendPathFragment "/search" (apiEndpoint modApiName)
      searchparam = case q of
        SearchEmail e -> [("email", Just $ toS e)]
        SearchPhone p -> [("phone", Just $ toS p)]
        SearchWord w -> [("word", Just $ toS w)]
        SearchCriteria critera -> [("criteria", Just $ toS $ "(" <> applySearchCriteria critera <> ")")]
      optparams = applyOptionalQueryParam "converted" soptsConverted $
                  applyOptionalQueryParam "approved" soptsApproved $
                  applyOptionalQueryParam "page" (show <$> soptsPage) $
                  applyOptionalQueryParam "per_page" (show <$> soptsPerPage) []
  in prepareGet url (searchparam <> optparams) []
  where
    applySearchCriteria c = case c of
      OpEquals n v -> "(" <> n <> ":equals:" <> sanitise v <> ")"
      OpStartsWith n v -> "(" <> n <> ":starts_with:" <> sanitise v <> ")"
      OpAnd c1 c2 -> "(" <> applySearchCriteria c1 <> " and " <> applySearchCriteria c2 <> ")"
      OpOr c1 c2 -> "(" <> applySearchCriteria c1 <> " or " <> applySearchCriteria c2 <> ")"
    sanitise v =
      T.replace "," "\\," $
      T.replace ")" "\\)" $
      T.replace "(" "\\(" v

search :: (FromJSON a, HasZoho m)
       => BS.ByteString
       -> SearchQuery
       -> SearchOpts
       -> m (Either Error (PaginatedResponse "data" [a]))
search modApiName q opts =
  ZM.runRequestAndParseOptionalResponse emptyPaginatedResponse Prelude.id $
  searchRequest modApiName q opts
  where
    emptyPaginatedResponse = PaginatedResponse
      { pageActualData = []
      , pageRecordsPerPage = 0
      , pageCount = 0
      , pageCurrentPage = 0
      , pageMoreRecords = False
      }


relatedList :: (FromJSON a, HasZoho m)
            => BS.ByteString
            -> RecordId
            -> ApiName
            -> Maybe ZonedTime
            -> m (Either Error (PaginatedResponse "data" [a]))
relatedList modApiName rid relModName modifiedAfter_ =
  ZM.runRequestAndParseOptionalResponse noResults Prelude.id $
  relatedListRequest modApiName rid relModName modifiedAfter_
  where
    noResults = PaginatedResponse
      { pageActualData = []
      , pageRecordsPerPage = 0
      , pageCount = 0
      , pageCurrentPage = 0
      , pageMoreRecords = False
      }


relatedListRequest :: BS.ByteString
                   -> RecordId
                   -> ApiName
                   -> Maybe ZonedTime
                   -> Request
relatedListRequest modApiName rid relModName modifiedAfter_ =
  let url = uriAppendPathFragment
            ("/" <> toS rid <> "/" <> toS relModName)
            (apiEndpoint modApiName)
  in ZO.prepareGet url [] headers
  where
    iso8601 = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"

    headers =
      applyOptionalHeader hIfModifiedSince (iso8601 <$> modifiedAfter_)
      []



-- * Internal helper function
--
insertUpdateHelper :: forall a . (ToJSON a)
                   => String
                   -> BS.ByteString
                   -> Maybe BS.ByteString
                   -> [a]
                   -> TriggerSetting
                   -> Maybe DuplicateCheckFields
                   -> Request
insertUpdateHelper method modApiName mPathFragment records tsetting mDupCheckFields =
  let url = uriAppendPathFragment (maybe "" ("/" <>) mPathFragment) (apiEndpoint modApiName)
  in ZO.prepareWithPayload method url [] [] (Aeson.encode finalPayload)
  where
    finalPayload :: Aeson.Value
    finalPayload =
      let o1 = Aeson.toJSON wrappedRecords
          noTrigger = [] :: [Trigger]
          o2 = case tsetting of
            TSOmit -> Aeson.object []
            TSNone -> Aeson.object [ "trigger" Aeson..= noTrigger ]
            TSSpecific x -> Aeson.object [ "trigger" Aeson..= x ]
          o3 = case mDupCheckFields of
            Nothing -> Aeson.object []
            Just dupCheckFields -> Aeson.object [ "duplicate_check_fields" Aeson..= dupCheckFields ]
      in o1 `unsafeMergeObjects` o2 `unsafeMergeObjects` o3

    wrappedRecords :: ResponseWrapper "data" [a]
    wrappedRecords = ResponseWrapper records


$(makeLensesWith abbreviatedFields ''SearchOpts)
$(makeLensesWith abbreviatedFields ''ListOptions)

