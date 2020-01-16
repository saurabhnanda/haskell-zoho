{-# LANGUAGE DeriveAnyClass #-}
module Zoho.Desk.Contact where

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
import Zoho.Desk.Common (mkApiEndpoint, orgIdHeader, SearchResults(..))
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
  , contactType :: !(Maybe Text) -- TODO
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
  ZO.prepareGet (mkApiEndpoint "/contacts") params [orgIdHeader oid]
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
     -> m (Either Error [Contact cf])
list listOpts oid = do
  x :: Either Error (ResponseWrapper "data" [Contact cf]) <-
    ZM.runRequestAndParseOptionalResponse (ResponseWrapper []) Prelude.id $
    listRequest listOpts oid
  pure $ fmap unwrapResponse x
