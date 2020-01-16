{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE IncoherentInstances #-}
module Zoho.Desk.Account where

import Control.Lens
import Data.Aeson as Aeson
import Data.Aeson.TH as Aeson
import Data.Aeson.Casing as Casing
import Data.Text (Text)
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import GHC.Generics
import Zoho.Desk.Utils (accountJsonOptions)
import Zoho.Types (EmptyZohoStructure(..), Error)
import Zoho.Types (OrgId(..), ApiName, ResponseWrapper(..))
import Zoho.OAuth as ZO hiding (mkApiEndpoint)
import Zoho.Desk.Common (mkApiEndpoint, orgIdHeader, SearchResults(..), SortDirection(..))
import Network.HTTP.Client as HC (Request)
import Zoho.ZohoM as ZM
import qualified Data.Text as T
import Prelude
import Data.String.Conv (toS)

-- TODO: include=owner

data Account cf = Account
  { accId :: !(Maybe Text)
  , accAccountName :: !(Maybe Text)
  , accEmail :: !(Maybe Text)
  , accPhone :: !(Maybe Text)
  , accWebsite :: !(Maybe Text)
  , accFax :: !(Maybe Text)
  , accStreet :: !(Maybe Text)
  , accCity :: !(Maybe Text)
  , accState :: !(Maybe Text)
  , accCountry :: !(Maybe Text)
  , accIndustry :: !(Maybe Text)
  , accCode :: !(Maybe Text)
  , accDescription :: !(Maybe Text)
  , accAnnualRevenue :: !(Maybe Text) -- Strangely the UI validates this to be a number, but the JSON sends it as a string.
  , accCreatedTime :: !(Maybe UTCTime)
  , accModifiedTime :: !(Maybe UTCTime)
  , accZohoCRMAccount :: Maybe ()
  , accWebUrl :: !(Maybe Text)
  , accAssociatedSLAIds :: Maybe ()
  , accFollowing :: !(Maybe Bool)
  , accDeleted :: !(Maybe Bool)
  , accTrashed :: !(Maybe Bool)
  , accCustomFields :: !(Maybe cf)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

emptyAccount :: Account cf
emptyAccount = emptyZohoStructure

-- instance {-# OVERLAPS #-} FromJSON (Account ()) where
--   parseJSON v = do
--     a :: Account Aeson.Value <- parseJSON v
--     pure a{accCustomFields=Just ()}

$(deriveJSON accountJsonOptions ''Account)


-- TODO: includes
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
  ZO.prepareGet (mkApiEndpoint "/accounts") params [orgIdHeader oid]
  where
    params =
      applyOptionalQueryParam "sorBy" optSortBy $
      applyOptionalQueryParam "viewId" optViewId $
      applyOptionalQueryParam "limit" (show <$> optLimit) $
      applyOptionalQueryParam "from" (show <$> optFrom)
      []


list :: forall m cf . (HasZoho m, FromJSON cf)
     => ListOptions
     -> OrgId
     -> m (Either Error [Account cf])
list listOpts oid = do
  x :: Either Error (ResponseWrapper "data" [Account cf]) <-
    ZM.runRequestAndParseOptionalResponse (ResponseWrapper []) Prelude.id $
    listRequest listOpts oid
  pure $ fmap unwrapResponse x


data SortBy = SortRelevance
            | SortModifiedTime
            | SortCreatedTime
            | SortAccountName
            | SortOther ApiName
            deriving (Eq, Show)

data SearchOptions = SearchOptions
  { soptsFrom :: !(Maybe Int)
  , soptsLimit :: !(Maybe Int)
  , soptsId :: !(Maybe Text)
  , soptsAccountName :: !(Maybe Text)
  , soptsAll :: !(Maybe Bool)
  -- , soptsCustomFields :: ![(ApiName, Text)]
  , soptsCustomField1 :: !(Maybe (ApiName, Text))
  , soptsCustomField2 :: !(Maybe (ApiName, Text))
  , soptsCustomField3 :: !(Maybe (ApiName, Text))
  , soptsCustomField4 :: !(Maybe (ApiName, Text))
  , soptsCustomField5 :: !(Maybe (ApiName, Text))
  , soptsCustomField6 :: !(Maybe (ApiName, Text))
  , soptsCustomField7 :: !(Maybe (ApiName, Text))
  , soptsCustomField8 :: !(Maybe (ApiName, Text))
  , soptsCustomField9 :: !(Maybe (ApiName, Text))
  , soptsCustomField10 :: !(Maybe (ApiName, Text))
  , soptsCreatedTimeRange :: !(Maybe (UTCTime, UTCTime))
  , soptsModifiedTimeRange :: !(Maybe (UTCTime, UTCTime))
  , soptsSortBy :: !(Maybe (SortBy, SortDirection))
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

emptySearchOptions :: SearchOptions
emptySearchOptions = emptyZohoStructure

searchRequest :: SearchOptions
              -> OrgId
              -> Request
searchRequest SearchOptions{..} oid =
  ZO.prepareGet (mkApiEndpoint "/accounts/search") params [orgIdHeader oid]
  where
    applyCfParam k x p = case x of
      Nothing -> p
      Just (n, v) -> (k, Just $ toS $ n <> ":" <> v):p

    iso8601 = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"

    applyTimeRangeParam k Nothing p = p
    applyTimeRangeParam k (Just (t1, t2)) p =
      (k, Just $ toS $ (iso8601 t1) <> ":" <> (iso8601 t2)):p

    applySortBy k v p = case v of
      Nothing -> p
      Just (sortField, sortDir) ->
        let x = case sortField of
                  SortRelevance -> "relevance"
                  SortModifiedTime -> "modifiedTime"
                  SortCreatedTime -> "createdTime"
                  SortAccountName -> "accountName"
                  SortOther z -> z
            y = case sortDir of
                  SortAsc -> x
                  SortDesc -> "-" <> x
        in (k, Just $ toS y):p

    params =
      applySortBy "sortBy" soptsSortBy $
      applyTimeRangeParam "modifiedTimeRange" soptsModifiedTimeRange $
      applyTimeRangeParam "createdTimeRange" soptsCreatedTimeRange $
      applyCfParam "customField1" soptsCustomField1 $
      applyCfParam "customField2" soptsCustomField2 $
      applyCfParam "customField3" soptsCustomField3 $
      applyCfParam "customField4" soptsCustomField4 $
      applyCfParam "customField5" soptsCustomField5 $
      applyCfParam "customField6" soptsCustomField6 $
      applyCfParam "customField7" soptsCustomField7 $
      applyCfParam "customField8" soptsCustomField8 $
      applyCfParam "customField9" soptsCustomField9 $
      applyCfParam "customField10" soptsCustomField10 $
      applyOptionalQueryParam "_all" ((T.toLower . toS . show) <$> soptsAll) $
      applyOptionalQueryParam "accountName" soptsAccountName $
      applyOptionalQueryParam "id" soptsId $
      applyOptionalQueryParam "limit" (show <$> soptsLimit) $
      applyOptionalQueryParam "from" (show <$> soptsFrom)
      []

search :: (HasZoho m, FromJSON cf)
       => SearchOptions
       -> OrgId
       -> m (Either Error (SearchResults (Account cf)))
search sopts oid =
  ZM.runRequestAndParseOptionalResponse (SearchResults [] 0) Prelude.id $
  searchRequest sopts oid

createRequest :: (ToJSON cf)
              => OrgId
              -> Account cf
              -> Request
createRequest oid a =
  ZO.prepareJSONPost (mkApiEndpoint "/accounts") [] [orgIdHeader oid] a

create :: (HasZoho m, ToJSON cf, FromJSON cf)
       => OrgId
       -> Account cf
       -> m (Either Error (Account cf))
create oid a =
  ZM.runRequestAndParseResponse $
  createRequest oid a

updateRequest :: (ToJSON cf)
              => OrgId
              -> Text
              -> Account cf
              -> Request
updateRequest oid aid a =
  ZO.prepareJSONPost (mkApiEndpoint $ "/accounts/" <> toS aid) [] [orgIdHeader oid] a{accId=Just aid}


update :: (HasZoho m, ToJSON cf, FromJSON cf)
       => OrgId
       -> Account cf
       -> m (Either Error (Account cf))
update oid a =
  ZM.runRequestAndParseResponse $
  createRequest oid a
