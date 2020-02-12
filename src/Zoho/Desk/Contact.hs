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
import Zoho.Types (EmptyZohoStructure(..), Error, zohoPrefix)
import Zoho.Types (OrgId(..), ApiName, ResponseWrapper(..))
import Zoho.OAuth as ZO hiding (mkApiEndpoint)
import Zoho.Desk.Common as Common
import Zoho.Desk.Common
import Network.HTTP.Client as HC (Request)
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
  , contactOwnerId :: !(Maybe Text) -- TODO
  , contactOwner :: Maybe Aeson.Value -- TODO
  , contactAccountId :: !(Maybe Text)
  , contatZohoCRMContact :: Aeson.Value -- TODO
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
      applyCustomFieldSearchParams opts $
      applyCommonSearchParams opts []

search :: (HasZoho m, FromJSON cf)
       => SearchOptions
       -> OrgId
       -> m (Either Error (SearchResults (Contact cf)))
search sopts oid =
  ZM.runRequestAndParseOptionalResponse (SearchResults [] 0) Prelude.id $
  searchRequest sopts oid
