{-# LANGUAGE DeriveAnyClass #-}

module Zoho.Books.Contact where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson as Aeson
import Data.Aeson.Casing as Casing
import Control.Lens
import GHC.Generics
import Zoho.Types
import Zoho.Books.Common
import qualified Zoho.OAuth as ZO
import Zoho.ZohoM as ZM
import qualified Zoho.Books.Common as Common
import Network.HTTP.Client as HC (Request)
import Data.String.Conv

type ContactId = Text

data Address = Address 
  { addAttention :: !(Maybe Text)
  , addAddress :: !(Maybe Text)
  , addStreet2 :: !(Maybe Text)
  , addStateCode :: !(Maybe Text)
  , addCity :: !(Maybe Text)
  , addState :: !(Maybe Text)
  , addZip :: !(Maybe Text)
  , addCountry :: !(Maybe Text)
  , addFax :: !(Maybe Text)
  , addPhone :: !(Maybe Text)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

instance ToJSON Address where
  toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

instance FromJSON Address where
  parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

data CustomField = CustomField
  { cfIndex :: !(Maybe Int)
  , cfValue :: !(Maybe Text)
  , cfLabel :: !(Maybe Text)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

instance ToJSON CustomField where
  toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

instance FromJSON CustomField where
  parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

data Contact = Contact 
  { conContactId :: !(Maybe ContactId)
  , conContactName :: !(Maybe Text)
  , conCompanyName :: !(Maybe Text)
  , conContactType :: !(Maybe Text)
  , conCustomerSubType :: !(Maybe Text)
  , conIsTaxable :: !(Maybe Bool)
  , conTaxName :: !(Maybe Text)
  , conPlaceOfContact :: !(Maybe Text)
  , conGstNo :: !(Maybe Text)
  , conIsTdsRegistered :: !(Maybe Text)
  , conGstTreatment :: !(Maybe Text)
  , conCurrencyCode :: !(Maybe Text)
  , conStatus :: !(Maybe Text)
  , conCustomFields :: !(Maybe [CustomField])
  , conBillingAddress :: !(Maybe Address)
  , conShippingAddress :: !(Maybe Address)
  , conNotes :: !(Maybe Text)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

instance ToJSON Contact where
  toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

instance FromJSON Contact where
  parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

$(makeLensesWith abbreviatedFields ''Address)
$(makeLensesWith abbreviatedFields ''CustomField)
$(makeLensesWith abbreviatedFields ''Contact)

fetchRequest :: OrgId -> ContactId -> Request
fetchRequest orgId conId = 
  ZO.prepareGet (Common.mkApiEndpoint $ "/contacts/" <> toS conId) (Common.orgIdParam orgId) []

fetch :: (HasZoho m) => OrgId -> ContactId -> m (Either Error Contact)
fetch orgId conId = do
  (ZM.runRequestAndParseResponse $ fetchRequest orgId conId) >>= \case
    Left e -> pure $ Left e
    Right (r :: ResponseWrapper "contact" Contact) -> pure $ Right $ unwrapResponse r


data ListOpts = ListOpts
  { optPage :: !(Maybe Int)
  , optPerPage :: !(Maybe Int)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

listRequest :: OrgId -> ListOpts -> Request
listRequest orgId ListOpts{..} = 
  let params = ZO.applyOptionalQueryParam "per_page" (fmap show optPerPage) $
               ZO.applyOptionalQueryParam "page" (fmap show optPage) $
               Common.orgIdParam orgId
  in ZO.prepareGet (Common.mkApiEndpoint "/contacts") params []

list :: (HasZoho m) => OrgId -> ListOpts -> m (Either Error (PaginatedResponse "contacts" [Contact]))
list orgId opts = 
  ZM.runRequestAndParseResponse $ listRequest orgId opts