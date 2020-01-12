{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
module Zoho.Desk.Account where

import Control.Lens
import Data.Aeson as Aeson
import Data.Aeson.TH as Aeson
import Data.Aeson.Casing as Casing
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import Zoho.Desk.Utils (accountJsonOptions)
import Zoho.Types (EmptyZohoStructure(..), Error)
import Zoho.Types (OrgId(..), ApiName, ResponseWrapper(..))
import Zoho.OAuth as ZO hiding (mkApiEndpoint)
import Zoho.Desk.Common (mkApiEndpoint, orgIdHeader)
import Network.HTTP.Client as HC (Request)
import Zoho.ZohoM as ZM
import Prelude

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

$(deriveJSON accountJsonOptions ''Account)

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
