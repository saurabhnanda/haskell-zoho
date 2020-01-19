{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE IncoherentInstances #-}
module Zoho.Desk.Account
  ( module Zoho.Desk.Account
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
import Zoho.Desk.Utils (accountJsonOptions)
import Zoho.Types (EmptyZohoStructure(..), Error)
import Zoho.Types (OrgId(..), ApiName, ResponseWrapper(..))
import Zoho.OAuth as ZO hiding (mkApiEndpoint)
import Zoho.Desk.Common as Common
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

$(makeLensesWith abbreviatedFields ''Account)

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
  ZO.prepareGet (Common.mkApiEndpoint "/accounts") params [Common.orgIdHeader oid]
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
  ZO.prepareGet (Common.mkApiEndpoint "/accounts/search") params [Common.orgIdHeader oid]
  where
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
      applyOptionalQueryParam "accountName" soptsAccountName $
      applyCustomFieldSearchParams opts $
      applyCommonSearchParams opts []

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
  ZO.prepareJSONPost (Common.mkApiEndpoint "/accounts") [] [Common.orgIdHeader oid] a

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
  ZO.prepareJSONPatch (Common.mkApiEndpoint $ "/accounts/" <> toS aid) [] [Common.orgIdHeader oid] a{accId=Just aid}


update :: (HasZoho m, ToJSON cf, FromJSON cf)
       => OrgId
       -> Text
       -> Account cf
       -> m (Either Error (Account cf))
update oid aid a =
  ZM.runRequestAndParseResponse $
  updateRequest oid aid a
