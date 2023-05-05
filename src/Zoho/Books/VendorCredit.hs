{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Zoho.Books.VendorCredit where

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
import Zoho.Books.Bill (BillId)

newtype VendorCreditId = VendorCreditId { rawVendorCreditId :: Text} 
  deriving (Eq, Show, Generic, Ord)
  deriving ToJSON via Text
  deriving FromJSON via Text

data VendorCreditLineItem = VendorCreditLineItem 
  { vcliLineItemId :: !(Maybe Text)
  , vcliItemId :: !(Maybe Text)
  , vcliAccountId :: !(Maybe AccountId)
  , vcliName :: !(Maybe Text)
  , vcliHsnOrSac :: !(Maybe Text)
  , vcliDescription :: !(Maybe Text)
  , vcliQuantity :: !(Maybe Double)
  , vcliRate :: !(Maybe Double)
  , vcliTaxId :: !(Maybe TaxId)
  , vcliGstTreatmentCode :: !(Maybe Text)
  -- , vcliIsBillable :: !(Maybe Bool)
  } deriving (Eq, Show, Generic)
$(makeLensesWith abbreviatedFields ''VendorCreditLineItem)

instance ToJSON VendorCreditLineItem where
  toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

instance FromJSON VendorCreditLineItem where
  parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

data VendorCredit cf = VendorCredit
  { vcVendorCreditId :: !(Maybe VendorCreditId)
  , vcVendorId :: !(Maybe VendorId)
  , vcVendorCreditNumber :: !(Maybe Text)
  , vcGstTreatment :: !(Maybe Text) -- TODO
  , vcGstNo :: !(Maybe Text) 
  , vcSourceOfSupply :: !(Maybe Text) -- TODO: Change to GstStateCode
  , vcDestinationOfSupply :: !(Maybe Text) -- TODO: Change to GstStateCode
  , vcReferenceNumber :: !(Maybe Text)
  , vcDate :: !(Maybe Day)
  , vcIsInclusiveTax :: !(Maybe Bool)
  , vcLineItems :: !(Maybe [VendorCreditLineItem])
  , vcNotes :: !(Maybe Text)
  , vcCustomFields :: !(Maybe [CustomField])
  , vcOtherFields :: !(Maybe cf)
  , vcAdjustmentDescription :: !(Maybe Text)
  , vcAdjustment :: !(Maybe Double)
  , vcCurrencyId :: !(Maybe CurrencyId)
  , vcBillId :: !(Maybe Text)
  , vcReferenceInvoiceType :: !(Maybe Text)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)
$(makeLensesWith abbreviatedFields ''VendorCredit)

instance (ToJSON cf) => ToJSON (VendorCredit cf) where
  toJSON :: ToJSON cf => VendorCredit cf -> Value
  toJSON acc =
    let x  = genericToJSON (zohoPrefix Casing.snakeCase) acc{vcOtherFields=(Nothing :: Maybe OmitField)}
        cf = toJSON (vcOtherFields acc)
        y = unsafeMergeObjects x cf
    in case y of
      Aeson.Object o -> Aeson.Object $ HML.delete "other_fields" o
      _ -> y

instance (FromJSON cf) => FromJSON (VendorCredit cf) where
  parseJSON v = do
    acc :: VendorCredit OmitField <- genericParseJSON (zohoPrefix Casing.snakeCase) v
    cf <- parseJSON v
    pure acc{vcOtherFields=cf}

-- instance ToJSON Expense where
--   toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

-- instance FromJSON Expense where
--   parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)

createRequest :: (ToJSON cf) => OrgId -> VendorCredit cf -> Request
createRequest orgId obj =
  let params = Common.orgIdParam orgId
  in ZO.prepareJSONPost (Common.mkApiEndpoint "/vendorcredits") params [] obj

create :: forall m cf . (HasZoho m, ToJSON cf, FromJSON cf) => OrgId -> VendorCredit cf -> m (Either Error (VendorCredit cf))
create orgId obj = do
  (ZM.runRequestAndParseResponse $ createRequest orgId obj) >>= \case
    Left e -> pure $ Left e
    Right (r :: ResponseWrapper "vendor_credit" (VendorCredit cf)) -> pure $ Right $ unwrapResponse r


deleteRequest :: OrgId -> VendorCreditId -> Request
deleteRequest orgId VendorCreditId{rawVendorCreditId} = 
  let params = Common.orgIdParam orgId
  in ZO.prepareDelete (Common.mkApiEndpoint $ "/vendorcredits/" <> toS rawVendorCreditId) params [] Nothing

delete :: (HasZoho m) => OrgId -> VendorCreditId -> m (Either Error DeleteResult)
delete orgId objId = 
  ZM.runRequestAndParseResponse $  deleteRequest orgId objId

data ListOpts = ListOpts
  { optVendorCreditNumber :: !(Maybe ListOp)
  , optDateBefore :: !(Maybe Day)
  , optDateAfter :: !(Maybe Day)
  , optDateStart :: !(Maybe Day)
  , optDateEnd :: !(Maybe Day)
  , optReferenceNumber :: !(Maybe ListOp)
  , optNotes :: !(Maybe ListOp)
  , optSearchText :: !(Maybe Text)

  -- , optBillNumber :: !(Maybe ListOp)
  -- , optDescription :: !(Maybe ListOp)
  -- , optVendorId :: !(Maybe VendorId)
  , optPage :: !(Maybe Int)
  , optPerPage :: !(Maybe Int)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

$(makeLensesWith abbreviatedFields ''ListOpts)

listRequest :: OrgId -> ListOpts -> Request
listRequest orgId ListOpts{..} =
  let params =  ZO.applyOptionalQueryParam "date_before" (show <$> optDateBefore) $
                ZO.applyOptionalQueryParam "date_after" (show <$> optDateAfter) $
                ZO.applyOptionalQueryParam "date_start" (show <$> optDateStart) $
                ZO.applyOptionalQueryParam "date_end" (show <$> optDateEnd) $
                ZO.applyOptionalQueryParam "search_text" optSearchText $
                applyOptionalListOp "notes" optNotes $
                applyOptionalListOp "vendor_credit_number" optVendorCreditNumber $
                applyOptionalListOp "reference_number" optReferenceNumber $
                ZO.applyOptionalQueryParam "page" (show <$> optPage) $
                ZO.applyOptionalQueryParam "per_page" (show <$> optPerPage) $
                Common.orgIdParam orgId
  in ZO.prepareGet (Common.mkApiEndpoint "/vendorcredits") params []

list :: (HasZoho m, FromJSON cf) => OrgId -> ListOpts -> m (Either Error (PaginatedResponse "vendor_credits" [VendorCredit cf]))
list orgId opts = 
  ZM.runRequestAndParseResponse $ listRequest orgId opts


data VendorCreditBill = VendorCreditBill
  { vcbBillId :: !(Maybe BillId)
  , vcbAmountApplied :: !(Maybe Double)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)
$(makeLensesWith abbreviatedFields ''VendorCreditBill)

instance ToJSON VendorCreditBill where
  toJSON = genericToJSON (zohoPrefix Casing.snakeCase)

instance FromJSON VendorCreditBill where
  parseJSON = genericParseJSON (zohoPrefix Casing.snakeCase)


applyToBillsRequest :: OrgId -> VendorCreditId -> [VendorCreditBill] -> Request
applyToBillsRequest orgId VendorCreditId{..} vcbs = 
  let params = Common.orgIdParam orgId
      pload = ResponseWrapper vcbs :: ResponseWrapper "bills" [VendorCreditBill]
  in ZO.prepareJSONPost (Common.mkApiEndpoint $ "/vendorcredits/" <> toS rawVendorCreditId <> "/bills") params [] pload

applyToBills :: (HasZoho m) => OrgId -> VendorCreditId -> [VendorCreditBill] -> m (Either Error [VendorCreditBill])
applyToBills orgId vcId vcbs = do
  (ZM.runRequestAndParseResponse $ applyToBillsRequest orgId vcId vcbs) >>= \case
    Left e -> 
      pure $ Left e
    Right (r :: ResponseWrapper "apply_to_bills" (ResponseWrapper "bills" [VendorCreditBill])) -> 
      pure $ Right $ unwrapResponse $ unwrapResponse r
