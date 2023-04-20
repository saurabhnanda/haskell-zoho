{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Zoho.Books.Bill where

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
import Zoho.Books.Account (AccountId(..))
import Zoho.Books.Contact (VendorId (..), CustomerId (..))
import Data.Time
import Zoho.CRM.Common (ZohoResult(..))
import Network.HTTP.Types as HT
import qualified Data.ByteString as BS

newtype BillId = BillId {rawBillId :: Text} 
  deriving (Eq, Show, Generic, Ord)
  deriving ToJSON via Text
  deriving FromJSON via Text

data BillLineItem = BillLineItem 
  { bliLineItemId :: !(Maybe Text)
  , bliItemId :: !(Maybe Text)
  , bliName :: !(Maybe Text)
  , bliAccountId :: !(Maybe AccountId)
  , bliDescription :: !(Maybe Text)
  , bliRate :: !(Maybe Double)
  , bliHsnOrSac :: !(Maybe Text)
  , bliQuantity :: !(Maybe Double)
  , bliCustomerId :: !(Maybe CustomerId)
  , bliTaxId :: !(Maybe TaxId)
  , bliGstTreatmentCode :: !(Maybe Text)
  , bliIsBillable :: !(Maybe Bool)
  } deriving (Eq, Show, Generic)
$(makeLensesWith abbreviatedFields ''BillLineItem)

instance ToJSON BillLineItem where
  toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

instance FromJSON BillLineItem where
  parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

data Bill cf = Bill
  { billBillId :: !(Maybe BillId)
  , billVendorId :: !(Maybe VendorId)
  , billCurrencyId :: !(Maybe CurrencyId)
  , billBillNumber :: !(Maybe Text)
  , billSourceOfSupply :: !(Maybe Text) -- TODO: Change to GstStateCode
  , billDestinationOfSupply :: !(Maybe Text) -- TODO: Change to GstStateCode
  , billGstTreatment :: !(Maybe Text) -- TODO
  , billGstNo :: !(Maybe Text) 
  , billReferenceNumber :: !(Maybe Text)
  , billDate :: !(Maybe Day)
  , billDueDate :: !(Maybe Day)
  , billPaymentTerms :: !(Maybe Int)
  , billPaymentTermsLabel :: !(Maybe Text)
  , billIsInclusiveTax :: !(Maybe Bool)
  , billLineItems :: !(Maybe [BillLineItem])
  , billNotes :: !(Maybe Text)
  , billCustomFields :: !(Maybe [CustomField])
  , billOtherFields :: !(Maybe cf)
  , billTdsPercent :: !(Maybe Text)
  , billTdsAmount :: !(Maybe Double)
  , billIsTdsAmountInPercent :: !(Maybe Bool)
  , billTdsTaxId :: !(Maybe TdsId)
  , billAdjustmentDescription :: !(Maybe Text)
  , billAdjustment :: !(Maybe Double)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)
$(makeLensesWith abbreviatedFields ''Bill)

instance (ToJSON cf) => ToJSON (Bill cf) where
  toJSON :: ToJSON cf => Bill cf -> Value
  toJSON acc =
    let x  = genericToJSON (zohoPrefix Casing.snakeCase) acc{billOtherFields=(Nothing :: Maybe OmitField)}
        cf = toJSON (billOtherFields acc)
        y = unsafeMergeObjects x cf
    in case y of
      Aeson.Object o -> Aeson.Object $ HML.delete "other_fields" o
      _ -> y

instance (FromJSON cf) => FromJSON (Bill cf) where
  parseJSON v = do
    acc :: Bill OmitField <- genericParseJSON (zohoPrefix Casing.snakeCase) v
    cf <- parseJSON v
    pure acc{billOtherFields=cf}

-- instance ToJSON Expense where
--   toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

-- instance FromJSON Expense where
--   parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

createRequest :: (ToJSON cf) => OrgId -> Bill cf -> Request
createRequest orgId obj =
  let params = Common.orgIdParam orgId
  in ZO.prepareJSONPost (Common.mkApiEndpoint "/bills") params [] obj

create :: forall m cf . (HasZoho m, ToJSON cf, FromJSON cf) => OrgId -> Bill cf -> m (Either Error (Bill cf))
create orgId obj = do
  (ZM.runRequestAndParseResponse $ createRequest orgId obj) >>= \case
    Left e -> pure $ Left e
    Right (r :: ResponseWrapper "bill" (Bill cf)) -> pure $ Right $ unwrapResponse r


deleteRequest :: OrgId -> BillId -> Request
deleteRequest orgId BillId{rawBillId} = 
  let params = Common.orgIdParam orgId
  in ZO.prepareDelete (Common.mkApiEndpoint $ "/bills/" <> toS rawBillId) params [] Nothing

delete :: (HasZoho m) => OrgId -> BillId -> m (Either Error DeleteResult)
delete orgId objId = 
  ZM.runRequestAndParseResponse $  deleteRequest orgId objId

data ListOpts = ListOpts
  { optReferenceNumber :: !(Maybe ListOp)
  , optBillNumber :: !(Maybe ListOp)
  , optDateBefore :: !(Maybe Day)
  , optDateAfter :: !(Maybe Day)
  , optDateStart :: !(Maybe Day)
  , optDateEnd :: !(Maybe Day)
  , optDescription :: !(Maybe ListOp)
  , optVendorName :: !(Maybe ListOp)
  , optVendorId :: !(Maybe VendorId)
  , optSearchText :: !(Maybe Text)
  , optPage :: !(Maybe Int)
  , optPerPage :: !(Maybe Int)

  -- , optAccountName :: !(Maybe ListOp)
  -- , optCustomerName :: !(Maybe ListOp)
  -- , optCustomerId :: !(Maybe CustomerId)
  -- , optPaidThroughAccountId :: !(Maybe AccountId)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

$(makeLensesWith abbreviatedFields ''ListOpts)

listRequest :: OrgId -> ListOpts -> Request
listRequest orgId ListOpts{..} =
  let params =  ZO.applyOptionalQueryParam "date_before" (show <$> optDateBefore) $
                ZO.applyOptionalQueryParam "date_after" (show <$> optDateAfter) $
                ZO.applyOptionalQueryParam "date_start" (show <$> optDateStart) $
                ZO.applyOptionalQueryParam "date_end" (show <$> optDateEnd) $
                ZO.applyOptionalQueryParam "search_text" optSearchText $
                ZO.applyOptionalQueryParam "vendor_id" (rawVendorId <$> optVendorId) $
                applyOptionalListOp "vendor_name" optVendorName $
                applyOptionalListOp "bill_number" optBillNumber $
                applyOptionalListOp "description" optDescription $
                applyOptionalListOp "reference_number" optReferenceNumber $
                ZO.applyOptionalQueryParam "page" (show <$> optPage) $
                ZO.applyOptionalQueryParam "per_page" (show <$> optPerPage) $
                Common.orgIdParam orgId
  in ZO.prepareGet (Common.mkApiEndpoint "/bills") params []

list :: (HasZoho m, FromJSON cf) => OrgId -> ListOpts -> m (Either Error (PaginatedResponse "bills" [Bill cf]))
list orgId opts = 
  ZM.runRequestAndParseResponse $ listRequest orgId opts