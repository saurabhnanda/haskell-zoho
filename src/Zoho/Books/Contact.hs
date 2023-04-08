{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingVia #-}

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
import qualified Data.HashMap.Lazy as HML
import qualified Data.ByteString as BS


newtype ContactId = ContactId {rawContactId :: Text} 
  deriving (Eq, Show, Generic, Ord) 
  deriving ToJSON via Text 
  deriving FromJSON via Text

newtype VendorId = VendorId {rawVendorId :: Text} 
  deriving (Eq, Show, Generic, Ord) 
  deriving ToJSON via Text 
  deriving FromJSON via Text

newtype CustomerId = CustomerId {rawCustomerId :: Text}
  deriving (Eq, Show, Generic, Ord)
  deriving (ToJSON) via Text
  deriving (FromJSON) via Text

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

data Contact objId cf = Contact 
  { conContactId :: !(Maybe objId)
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
  , conOtherFields :: !(Maybe cf)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

instance (ToJSON cf, ToJSON objId) => ToJSON (Contact objId cf) where
  toJSON :: ToJSON cf => Contact objId cf -> Value
  toJSON acc =
    let x = genericToJSON (zohoPrefix Casing.snakeCase) acc {conOtherFields = (Nothing :: Maybe OmitField)}
        cf = toJSON (conOtherFields acc)
        y = unsafeMergeObjects x cf
     in case y of
          Aeson.Object o -> Aeson.Object $ HML.delete "other_fields" o
          _ -> y

instance (FromJSON cf, FromJSON objId) => FromJSON (Contact objId cf) where
  parseJSON v = do
    acc :: Contact objId OmitField <- genericParseJSON (zohoPrefix Casing.snakeCase) v
    cf <- parseJSON v
    pure acc {conOtherFields = cf}

$(makeLensesWith abbreviatedFields ''Address)
$(makeLensesWith abbreviatedFields ''CustomField)
$(makeLensesWith abbreviatedFields ''Contact)

fetchRequest :: BS.ByteString -> OrgId -> Text -> Request
fetchRequest objName orgId id_ = 
  ZO.prepareGet (Common.mkApiEndpoint $ "/" <> objName <> "/" <> toS id_) (Common.orgIdParam orgId) []

fetch :: forall m cf objId. (HasZoho m, FromJSON cf, FromJSON objId) => BS.ByteString -> OrgId -> Text -> m (Either Error (Contact objId cf))
fetch objName orgId id_ = do
  (ZM.runRequestAndParseResponse $ fetchRequest objName orgId id_) >>= \case
    Left e -> pure $ Left e
    Right (r :: ResponseWrapper "contact" (Contact objId cf)) -> pure $ Right $ unwrapResponse r


data ListOpts = ListOpts
  { optPage :: !(Maybe Int)
  , optPerPage :: !(Maybe Int)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

listRequest :: BS.ByteString -> OrgId ->  ListOpts -> Request
listRequest objName orgId ListOpts{..} = 
  let params = ZO.applyOptionalQueryParam "per_page" (fmap show optPerPage) $
               ZO.applyOptionalQueryParam "page" (fmap show optPage) $
               Common.orgIdParam orgId
  in ZO.prepareGet (Common.mkApiEndpoint $ "/" <> objName) params []

list :: (HasZoho m, FromJSON cf, FromJSON objId) => BS.ByteString -> OrgId -> ListOpts -> m (Either Error (PaginatedResponse "contacts" [Contact objId cf]))
list objName orgId opts = 
  ZM.runRequestAndParseResponse $ listRequest objName orgId opts

type Vendor = Contact VendorId
type Customer = Contact CustomerId

listCustomers :: (HasZoho m, FromJSON cf) => OrgId -> ListOpts -> m (Either Error (PaginatedResponse "contacts" [Customer cf]))
listCustomers = list "customers"

fetchCustomer :: forall m cf . (HasZoho m, FromJSON cf) => OrgId -> CustomerId -> m (Either Error (Customer cf))
fetchCustomer orgId custId = fetch "customers" orgId (rawCustomerId custId)

listVendors :: (HasZoho m, FromJSON cf) => OrgId -> ListOpts -> m (Either Error (PaginatedResponse "contacts" [Vendor cf]))
listVendors = list "vendors"

fetchVendor :: forall m cf. (HasZoho m, FromJSON cf) => OrgId -> VendorId -> m (Either Error (Vendor cf))
fetchVendor orgId vendorId = fetch "vendors" orgId (rawVendorId vendorId)
